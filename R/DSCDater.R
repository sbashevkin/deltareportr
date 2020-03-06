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
    dplyr::select(-StationID)%>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs=4326)%>%
    sf::st_transform(crs=sf::st_crs(Shapefile))%>%
    sf::st_join(Shapefile%>%
                  dplyr::select(!!Region_column),
                join=sf::st_within)%>%
    tibble::as_tibble()%>%
    dplyr::select(-geometry)%>%
    dplyr::rename(Region=!!Region_column)

  # Bivalves ----------------------------------------------------------------

  if("Bivalves"%in%Variables){

    Data_list[["Bivalves"]]<-deltareportr::bivalves%>%
      dplyr::select(Date, Station=StationCode, `Potamocorbula amurensis`, `Corbicula fluminea`)%>%
      tidyr::pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")%>%
      dplyr::mutate(Year=lubridate::year(Date),
                    MonthYear=lubridate::floor_date(Date, unit = "month"),
                    Source="EMP")

    #Add regions and lat/long to zoop dataset
    Data_list[["Bivalves"]]<-Data_list[["Bivalves"]]%>%
      dplyr::filter(Year>=Start_year)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        dplyr::filter(., Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::mutate(Month=lubridate::month(MonthYear))%>%
      dplyr::mutate(Season=dplyr::case_when(
        Month%in%c(12,1,2) ~ "Winter",
        Month%in%c(3,4,5) ~ "Spring",
        Month%in%c(6,7,8) ~ "Summer",
        Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(Month==12, Year-1, Year)
      )

  }

  # Zooplankton -------------------------------------------------------------

  if("Zooplankton"%in%Variables){
    #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#

    ZoopCB<-deltareportr::zoop_cb%>%
      dplyr::select(Date, Station, ACARTELA, ACARTIA, DIAPTOM, EURYTEM, OTHCALAD, PDIAPFOR, PDIAPMAR, SINOCAL, TORTANUS, AVERNAL, OTHCYCAD, BOSMINA, DAPHNIA, DIAPHAN, OTHCLADO)%>%
      tidyr::pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")


    ZoopPump<-deltareportr::zoop_pump%>%
      dplyr::select(Date=SampleDate, Station, LIMNOSPP, LIMNOSINE, LIMNOTET, OITHDAV, OITHSIM, OITHSPP)%>%
      tidyr::pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")



    ZoopMysid<-deltareportr::zoop_mysid%>%
      dplyr::select(Date, Station, `Acanthomysis aspera`:`Unidentified mysid`)%>%
      dplyr::mutate(Mysida=rowSums(dplyr::select(., -Date, -Station), na.rm=T))%>%
      tidyr::pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "BPUE")%>%
      dplyr::mutate(BPUE=BPUE*1000,
                    Taxa="Mysida") # Convert to ug

    Data_list[["Zooplankton"]]<-dplyr::bind_rows(ZoopCB, ZoopPump)%>%
      dplyr::left_join(deltareportr::zoop_mass_conversions, by=c("Taxa"="taxon"))%>%
      dplyr::mutate(BPUE=CPUE*mass_indiv_ug,
                    Taxa=dplyr::case_when(
                      Taxa%in%c("ACARTELA", "ACARTIA", "DIAPTOM", "EURYTEM", "OTHCALAD", "PDIAPFOR", "PDIAPMAR", "SINOCAL", "TORTANUS") ~ "Calanoida",
                      Taxa%in%c("AVERNAL", "LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD") ~ "Cyclopoida",
                      Taxa%in%c("BOSMINA", "DAPHNIA", "DIAPHAN", "OTHCLADO") ~ "Cladocera"))%>%
      dplyr::select(-CPUE, -mass_indiv_ug)%>%
      dplyr::bind_rows(ZoopMysid)%>%
      dplyr::mutate(Year=lubridate::year(Date),
                    MonthYear=lubridate::floor_date(Date, unit = "month"),
                    Source="EMP")

    #Add regions and lat/long to zoop dataset
    Data_list[["Zooplankton"]]<-Data_list[["Zooplankton"]]%>%
      dplyr::filter(Year>=Start_year)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        dplyr::filter(., Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::mutate(Month=lubridate::month(MonthYear))%>%
      dplyr::mutate(Season=dplyr::case_when(
        Month%in%c(12,1,2) ~ "Winter",
        Month%in%c(3,4,5) ~ "Spring",
        Month%in%c(6,7,8) ~ "Summer",
        Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(Month==12, Year-1, Year)
      )

  }
  # Phytoplankton -----------------------------------------------------------

  if("Phytoplankton"%in%Variables){

    Data_list[["Phytoplankton"]]<-deltareportr::phyto%>%
      dplyr::rename(Date=SampleDate, Station=StationCode)%>%
      dplyr::mutate(Date=lubridate::parse_date_time(Date, "mdy"))%>%
      dplyr::bind_rows(read_excel("Data/Phytoplankton 2017.xlsx")%>%
                         dplyr::rename(Station=`Station Code`, Cryptophytes=Cryptomonads))%>%
      tidyr::pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")%>%
      dplyr::mutate(Taxa=dplyr::case_when(Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                                          Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                                          Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Cyanobacteria") ~ Taxa,
                                          TRUE ~ "Other taxa"))%>%
      dplyr::mutate(Year=lubridate::year(Date),
                    MonthYear=lubridate::floor_date(Date, unit = "month"),
                    Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station),
                    Source="EMP")%>%
      dplyr::filter(Year>=2008 & Year>Start_year)

    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to phyto dataset
    Data_list[["Phytoplankton"]]<-Data_list[["Phytoplankton"]]%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        dplyr::filter(., Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::group_by(Taxa, Region, Year, Date, Station)%>%
      dplyr::summarise(CPUE=sum(CPUE, na.rm=T))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(Month=lubridate::month(Date))%>%
      dplyr::mutate(Season=dplyr::case_when(
        Month%in%c(12,1,2) ~ "Winter",
        Month%in%c(3,4,5) ~ "Spring",
        Month%in%c(6,7,8) ~ "Summer",
        Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(Month==12, Year-1, Year)
      )
  }

  # Water quality -----------------------------------------------------------

  if("Water_quality"%in%Variables){

    # Load and combine data ---------------------------------------------------

    FMWT<-deltareportr::fmwt%>%
      dplyr::select(Date, Station, Conductivity=tidyselect::starts_with("Top EC"), Secchi=`Secchi (m)`, Microcystis, Temperature=tidyselect::starts_with("Top Temperature"))%>%
      dplyr::mutate(Source="FMWT",
                    Secchi=Secchi*100,
                    Microcystis=dplyr::if_else(Microcystis==6, 2, Microcystis))

    STN<-deltareportr::stn%>%
      dplyr::select(Date=SampleDate, Station=StationCode, Secchi, Temperature=`TemperatureTop`, Conductivity=`ConductivityTop`, Microcystis
      )%>%
      dplyr::mutate(Source="TNS")

    EDSM<-deltareportr::edsm%>%
      dplyr::select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=TopEC, Temperature=TopTemp, Secchi=Scchi)%>%
      dplyr::bind_rows(deltareportr::edsm_kdtr%>%
                         dplyr::select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=EC, Temperature=Temp, Secchi=Scchi))%>%
      dplyr::mutate(Secchi=Secchi*100)%>%
      dplyr::mutate(Station=paste(Latitude, Longitude),
                    Source="EDSM",
                    Date=lubridate::parse_date_time(Date, "%m/%d/%Y"))%>%
      dplyr::select(-Conductivity)%>% #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
      dplyr::distinct()%>%
      dplyr::filter(!is.na(Latitude) & !is.na(Longitude))%>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), #Add regions to EDSM data
                   crs=4326)%>%
      sf::st_transform(crs=sf::st_crs(Shapefile))%>%
      sf::st_join(Shapefile%>%
                    dplyr::select(!!Region_column),
                  join=sf::st_within)%>%
      tibble::as_tibble()%>%
      dplyr::select(-geometry)%>%
      dplyr::rename(Region=!!Region_column)

    Data_list[["Water_quality"]]<-deltareportr::wq_field%>%
      dplyr::select(Date=SampleDate, Station=StationCode, Parameter=AnalyteName, Value=Result, Notes=TextResult, Matrix)%>%
      dplyr::filter(Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)") & Matrix=="Water")%>%
      dplyr::group_by(Date, Station, Parameter, Notes)%>%
      dplyr::summarise(Value=mean(Value, na.rm=T))%>%
      dplyr::ungroup()%>%
      dplyr::bind_rows(deltareportr::wq_lab%>%
                         dplyr::select(Station=StationCode, Date=SampleDate, Parameter=ConstituentName, Value=Result, Notes=LabAnalysisRemarks)%>%
                         dplyr::filter(Parameter=="Chlorophyll a")%>%
                         dplyr::group_by(Date, Station, Parameter, Notes)%>%
                         dplyr::summarise(Value=mean(Value, na.rm=T))%>%
                         dplyr::ungroup())%>%
      pivot_wider(names_from = Parameter, values_from = Value)%>%
      dplyr::rename(Chlorophyll=`Chlorophyll a`, Secchi=`Secchi Depth`, Conductivity=`Conductance (EC)`)%>%
      dplyr::bind_rows(deltareportr::wq_2000%>%
                         dplyr::select(Station=`Station Name`, Date, Chlorophyll=tidyselect::starts_with("Chlorophyll"), Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`, Microcystis=`Microcystis aeruginosa`, Secchi=`Secchi Depth Centimeters`, Temperature=tidyselect::starts_with("Water Temperature"), Conductivity=tidyselect::starts_with("Specific Conductance"))%>%
                         dplyr::mutate(Chlorophyll=readr::parse_double(ifelse(Chlorophyll%in%c("<0.05", "<0.5"), 0, Chlorophyll)),
                                       Latitude=readr::parse_double(Latitude),
                                       Longitude=readr::parse_double(Longitude),
                                       Microcystis=readr::parse_double(Microcystis),
                                       Secchi=readr::parse_double(Secchi),
                                       Temperature=readr::parse_double(Temperature),
                                       Conductivity=readr::parse_double(Conductivity),
                                       Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
                         dplyr::mutate(Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
                         dplyr::select(-Latitude, -Longitude))%>%
      dplyr::mutate(Source="EMP")%>%
      dplyr::bind_rows(FMWT, STN, EDSM)%>%
      dplyr::mutate(MonthYear=lubridate::floor_date(Date, unit = "month"),
                    Year=lubridate::year(Date),
                    Salinity=((0.36966/(((Conductivity*0.001)^(-1.07))-0.00074))*1.28156))

    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to dataset
    Data_list[["Water_quality"]]<-dplyr::filter(Data_list[["Water_quality"]], Source!="EDSM")%>%
      dplyr::select(-Region)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      dplyr::bind_rows(dplyr::filter(Data_list[["Water_quality"]], Source=="EDSM"))%>%
      dplyr::filter(lubridate::year(MonthYear)>=Start_year)%>%
      {if (is.null(Regions)){
        dplyr::filter(., Region%in%Regions)
      } else{
        .
      }}%>%
      dplyr::mutate(Month=lubridate::month(MonthYear))%>%
      dplyr::mutate(Season=dplyr::case_when(
        Month%in%c(12,1,2) ~ "Winter",
        Month%in%c(3,4,5) ~ "Spring",
        Month%in%c(6,7,8) ~ "Summer",
        Month%in%c(9,10,11) ~ "Fall"),
        Year=dplyr::if_else(Month==12, Year-1, Year)
      )
  }

  # Return ------------------------------------------------------------------

  return(Data_list)
}
