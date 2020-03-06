#' Process report data
#'
#' Imports, filters, and processes datasets and outputs a list of desired datasets
#' @param Start_year Earliest year you would like included in the report. Must be an integer. Defaults to \code{2002}.
#' @param Variables Character vector of variables you would like included in the dataset. Defaults to all possible options: \code{Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality")}.
#' @param Shapefile Shapefile you would like used to define regions in the dataset. Must be in \code{\link[sf]{sf}} format, e.g., imported with \code{\link[sf]{st_read}}. Defaults to \code{\link{deltaregions}}.
#' @param Region_column Unquoted name of the column in the Shapefile with the region designations.
#' @param Regions Character vector of regions to be included in the dataset. Must correspond with levels of the \code{Region_column}. To include all data points regardless of whether they correspond to a region in the \code{Shapefile} set \code{Regions = NULL}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A list of datasets


DSCDater <- function(Start_year=2002,
                     Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality"),
                     Shapefile = deltareportr::deltaregions,
                     Region_column = Stratum,
                     Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){

  Region_column <- rlang::enquo(Region_column)

  Data_list <- list()

  # Stations ----------------------------------------------------------------

  Stations<-deltareportr::stations%>%
    dplyr::select(-.data$StationID)%>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs=4326)%>%
    sf::st_transform(crs=sf::st_crs(Shapefile))%>%
    sf::st_join(Shapefile%>%
                  dplyr::select(!!Region_column),
                join=sf::st_within)%>%
    tibble::as_tibble()%>%
    dplyr::select(-.data$geometry)%>%
    dplyr::rename(Region=!!Region_column)

  # Bivalves ----------------------------------------------------------------

  if("Bivalves"%in%Variables){

    Data_list[["Bivalves"]]<-deltareportr::bivalves%>%
      dplyr::select(.data$Date, Station=.data$StationCode, .data$`Potamocorbula amurensis`, .data$`Corbicula fluminea`)%>%
      tidyr::pivot_longer(c(-.data$Date, -.data$Station), names_to = "Taxa", values_to = "CPUE")%>%
      dplyr::mutate(Year=lubridate::year(.data$Date),
                    MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                    Source="EMP")

    #Add regions and lat/long to zoop dataset
    Data_list[["Bivalves"]]<-Data_list[["Bivalves"]]%>%
      dplyr::filter(.data$Year>=Start_year)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        dplyr::filter(., .data$Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::mutate(Month=lubridate::month(.data$MonthYear))%>%
      dplyr::mutate(Season=dplyr::case_when(
        .data$Month%in%c(12,1,2) ~ "Winter",
        .data$Month%in%c(3,4,5) ~ "Spring",
        .data$Month%in%c(6,7,8) ~ "Summer",
        .data$Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(.data$Month==12, .data$Year-1, .data$Year)
      )

  }

  # Zooplankton -------------------------------------------------------------

  if("Zooplankton"%in%Variables){
    #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#

    ZoopCB<-deltareportr::zoop_cb%>%
      dplyr::select(.data$Date, .data$Station, .data$ACARTELA, .data$ACARTIA, .data$DIAPTOM, .data$EURYTEM, .data$OTHCALAD, .data$PDIAPFOR, .data$PDIAPMAR, .data$SINOCAL, .data$TORTANUS, .data$AVERNAL, .data$OTHCYCAD, .data$BOSMINA, .data$DAPHNIA, .data$DIAPHAN, .data$OTHCLADO)%>%
      tidyr::pivot_longer(c(-.data$Date, -.data$Station), names_to = "Taxa", values_to = "CPUE")


    ZoopPump<-deltareportr::zoop_pump%>%
      dplyr::select(Date=.data$SampleDate, .data$Station, .data$LIMNOSPP, .data$LIMNOSINE, .data$LIMNOTET, .data$OITHDAV, .data$OITHSIM, .data$OITHSPP)%>%
      tidyr::pivot_longer(c(-.data$Date, -.data$Station), names_to = "Taxa", values_to = "CPUE")



    ZoopMysid<-deltareportr::zoop_mysid%>%
      dplyr::select(.data$Date, .data$Station, .data$`Acanthomysis aspera`:.data$`Unidentified mysid`)%>%
      dplyr::mutate(Mysida=rowSums(dplyr::select(., -.data$Date, -.data$Station), na.rm=T))%>%
      tidyr::pivot_longer(c(-.data$Date, -.data$Station), names_to = "Taxa", values_to = "BPUE")%>%
      dplyr::mutate(BPUE=.data$BPUE*1000,
                    Taxa="Mysida") # Convert to ug

    Data_list[["Zooplankton"]]<-dplyr::bind_rows(ZoopCB, ZoopPump)%>%
      dplyr::left_join(deltareportr::zoop_mass_conversions, by=c("Taxa"="taxon"))%>%
      dplyr::mutate(BPUE=.data$CPUE*.data$mass_indiv_ug,
                    Taxa=dplyr::case_when(
                      .data$Taxa%in%c("ACARTELA", "ACARTIA", "DIAPTOM", "EURYTEM", "OTHCALAD", "PDIAPFOR", "PDIAPMAR", "SINOCAL", "TORTANUS") ~ "Calanoida",
                      .data$Taxa%in%c("AVERNAL", "LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD") ~ "Cyclopoida",
                      .data$Taxa%in%c("BOSMINA", "DAPHNIA", "DIAPHAN", "OTHCLADO") ~ "Cladocera"))%>%
      dplyr::select(-.data$CPUE, -.data$mass_indiv_ug)%>%
      dplyr::bind_rows(ZoopMysid)%>%
      dplyr::mutate(Year=lubridate::year(.data$Date),
                    MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                    Source="EMP")

    #Add regions and lat/long to zoop dataset
    Data_list[["Zooplankton"]]<-Data_list[["Zooplankton"]]%>%
      dplyr::filter(.data$Year>=Start_year)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        dplyr::filter(., .data$Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::mutate(Month=lubridate::month(.data$MonthYear))%>%
      dplyr::mutate(Season=dplyr::case_when(
        .data$Month%in%c(12,1,2) ~ "Winter",
        .data$Month%in%c(3,4,5) ~ "Spring",
        .data$Month%in%c(6,7,8) ~ "Summer",
        .data$Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(.data$Month==12, .data$Year-1, .data$Year)
      )

  }
  # Phytoplankton -----------------------------------------------------------

  if("Phytoplankton"%in%Variables){

    Data_list[["Phytoplankton"]]<-deltareportr::phyto%>%
      dplyr::rename(Date=.data$SampleDate, Station=.data$StationCode)%>%
      dplyr::mutate(Date=lubridate::parse_date_time(.data$Date, "mdy"))%>%
      dplyr::bind_rows(read_excel("Data/Phytoplankton 2017.xlsx")%>%
                         dplyr::rename(Station=.data$`Station Code`, Cryptophytes=.data$Cryptomonads))%>%
      tidyr::pivot_longer(c(-.data$Date, -.data$Station), names_to = "Taxa", values_to = "CPUE")%>%
      dplyr::mutate(Taxa=dplyr::case_when(.data$Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                                          .data$Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                                          .data$Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Cyanobacteria") ~ .data$Taxa,
                                          TRUE ~ "Other taxa"))%>%
      dplyr::mutate(Year=lubridate::year(.data$Date),
                    MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                    Station=ifelse(.data$Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(.data$Station, .data$Date), .data$Station),
                    Source="EMP")%>%
      dplyr::filter(.data$Year>=2008 & .data$Year>Start_year)

    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to phyto dataset
    Data_list[["Phytoplankton"]]<-Data_list[["Phytoplankton"]]%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        dplyr::filter(., .data$Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::group_by(.data$Taxa, .data$Region, .data$Year, .data$Date, .data$Station)%>%
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(Month=lubridate::month(.data$Date))%>%
      dplyr::mutate(Season=dplyr::case_when(
        .data$Month%in%c(12,1,2) ~ "Winter",
        .data$Month%in%c(3,4,5) ~ "Spring",
        .data$Month%in%c(6,7,8) ~ "Summer",
        .data$Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(.data$Month==12, .data$Year-1, .data$Year)
      )
  }

  # Water quality -----------------------------------------------------------

  if("Water_quality"%in%Variables){

    # Load and combine data ---------------------------------------------------

    FMWT<-deltareportr::fmwt%>%
      dplyr::select(.data$Date, .data$Station, Conductivity=tidyselect::starts_with("Top EC"), Secchi=.data$`Secchi (m)`, .data$Microcystis, Temperature=tidyselect::starts_with("Top Temperature"))%>%
      dplyr::mutate(Source="FMWT",
                    Secchi=.data$Secchi*100,
                    Microcystis=dplyr::if_else(.data$Microcystis==6, 2, .data$Microcystis))

    STN<-deltareportr::stn%>%
      dplyr::select(Date=.data$SampleDate, Station=.data$StationCode, .data$Secchi, Temperature=.data$`TemperatureTop`, Conductivity=.data$`ConductivityTop`, .data$Microcystis
      )%>%
      dplyr::mutate(Source="TNS")

    EDSM<-deltareportr::edsm%>%
      dplyr::select(.data$Date, Latitude=.data$StartLat, Longitude=.data$StartLong, Conductivity=.data$TopEC, Temperature=.data$TopTemp, Secchi=.data$Scchi)%>%
      dplyr::bind_rows(deltareportr::edsm_kdtr%>%
                         dplyr::select(.data$Date, Latitude=.data$StartLat, Longitude=.data$StartLong, Conductivity=.data$EC, Temperature=.data$Temp, Secchi=.data$Scchi))%>%
      dplyr::mutate(Secchi=.data$Secchi*100)%>%
      dplyr::mutate(Station=paste(.data$Latitude, .data$Longitude),
                    Source="EDSM",
                    Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y"))%>%
      dplyr::select(-.data$Conductivity)%>% #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
      dplyr::distinct()%>%
      dplyr::filter(!is.na(.data$Latitude) & !is.na(.data$Longitude))%>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), #Add regions to EDSM data
                   crs=4326)%>%
      sf::st_transform(crs=sf::st_crs(Shapefile))%>%
      sf::st_join(Shapefile%>%
                    dplyr::select(!!Region_column),
                  join=sf::st_within)%>%
      tibble::as_tibble()%>%
      dplyr::select(-.data$geometry)%>%
      dplyr::rename(Region=!!Region_column)

    Data_list[["Water_quality"]]<-deltareportr::wq_field%>%
      dplyr::select(Date=.data$SampleDate, Station=.data$StationCode, Parameter=.data$AnalyteName, Value=.data$Result, Notes=.data$TextResult, .data$Matrix)%>%
      dplyr::filter(.data$Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)") & .data$Matrix=="Water")%>%
      dplyr::group_by(.data$Date, .data$Station, .data$Parameter, .data$Notes)%>%
      dplyr::summarise(Value=mean(.data$Value, na.rm=T))%>%
      dplyr::ungroup()%>%
      dplyr::bind_rows(deltareportr::wq_lab%>%
                         dplyr::select(Station=.data$StationCode, Date=.data$SampleDate, Parameter=.data$ConstituentName, Value=.data$Result, Notes=.data$LabAnalysisRemarks)%>%
                         dplyr::filter(.data$Parameter=="Chlorophyll a")%>%
                         dplyr::group_by(.data$Date, .data$Station, .data$Parameter, .data$Notes)%>%
                         dplyr::summarise(Value=mean(.data$Value, na.rm=T))%>%
                         dplyr::ungroup())%>%
      pivot_wider(names_from = .data$Parameter, values_from = .data$Value)%>%
      dplyr::rename(Chlorophyll=.data$`Chlorophyll a`, Secchi=.data$`Secchi Depth`, Conductivity=.data$`Conductance (EC)`)%>%
      dplyr::bind_rows(deltareportr::wq_2000%>%
                         dplyr::select(Station=.data$`Station Name`, .data$Date, Chlorophyll=tidyselect::starts_with("Chlorophyll"), Latitude=.data$`North Latitude Degrees (d.dd)`, Longitude=.data$`West Longitude Degrees (d.dd)`, Microcystis=.data$`Microcystis aeruginosa`, Secchi=.data$`Secchi Depth Centimeters`, Temperature=tidyselect::starts_with("Water Temperature"), Conductivity=tidyselect::starts_with("Specific Conductance"))%>%
                         dplyr::mutate(Chlorophyll=readr::parse_double(ifelse(.data$Chlorophyll%in%c("<0.05", "<0.5"), 0, .data$Chlorophyll)),
                                       Latitude=readr::parse_double(.data$Latitude),
                                       Longitude=readr::parse_double(.data$Longitude),
                                       Microcystis=readr::parse_double(.data$Microcystis),
                                       Secchi=readr::parse_double(.data$Secchi),
                                       Temperature=readr::parse_double(.data$Temperature),
                                       Conductivity=readr::parse_double(.data$Conductivity),
                                       Station=ifelse(.data$Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(.data$Station, .data$Date), .data$Station))%>%
                         dplyr::mutate(Microcystis=round(.data$Microcystis))%>% #EMP has some 2.5 and 3.5 values
                         dplyr::select(-.data$Latitude, -.data$Longitude))%>%
      dplyr::mutate(Source="EMP")%>%
      dplyr::bind_rows(FMWT, STN, EDSM)%>%
      dplyr::mutate(MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                    Year=lubridate::year(.data$Date),
                    Salinity=((0.36966/(((.data$Conductivity*0.001)^(-1.07))-0.00074))*1.28156))

    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to dataset
    Data_list[["Water_quality"]]<-dplyr::filter(Data_list[["Water_quality"]], .data$Source!="EDSM")%>%
      dplyr::select(-.data$Region)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      dplyr::bind_rows(dplyr::filter(Data_list[["Water_quality"]], .data$Source=="EDSM"))%>%
      dplyr::filter(lubridate::year(.data$MonthYear)>=Start_year)%>%
      {if (is.null(Regions)){
        dplyr::filter(., .data$Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::mutate(Month=lubridate::month(.data$MonthYear))%>%
      dplyr::mutate(Season=dplyr::case_when(
        .data$Month%in%c(12,1,2) ~ "Winter",
        .data$Month%in%c(3,4,5) ~ "Spring",
        .data$Month%in%c(6,7,8) ~ "Summer",
        .data$Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(.data$Month==12, .data$Year-1, .data$Year)
      )
  }

  # Return ------------------------------------------------------------------

  return(Data_list)
}
