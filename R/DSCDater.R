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

    #Add regions and lat/long to zoop dataset
    Data_list[["Bivalves"]]<-deltareportr::bivalves%>%
      dplyr::filter(.data$Year>=Start_year)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        .
      } else{
        dplyr::filter(., .data$Region%in%Regions)
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

    Data_list[["Zooplankton"]]<-dplyr::bind_rows(deltareportr::zoop_cb, deltareportr::zoop_pump)%>%
      dplyr::left_join(deltareportr::zoop_mass_conversions, by=c("Taxa"="taxon"))%>%
      dplyr::mutate(BPUE=.data$CPUE*.data$mass_indiv_ug,
                    Taxa=dplyr::case_when(
                      .data$Taxa%in%c("ACARTELA", "ACARTIA", "DIAPTOM", "EURYTEM", "OTHCALAD", "PDIAPFOR", "PDIAPMAR", "SINOCAL", "TORTANUS") ~ "Calanoida",
                      .data$Taxa%in%c("AVERNAL", "LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD") ~ "Cyclopoida",
                      .data$Taxa%in%c("BOSMINA", "DAPHNIA", "DIAPHAN", "OTHCLADO") ~ "Cladocera"))%>%
      dplyr::select(-.data$CPUE, -.data$mass_indiv_ug)%>%
      dplyr::bind_rows(deltareportr::zoop_mysid)

    #Add regions and lat/long to zoop dataset
    Data_list[["Zooplankton"]]<-Data_list[["Zooplankton"]]%>%
      dplyr::filter(.data$Year>=Start_year)%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        .
      } else{
        dplyr::filter(., .data$Region%in%Regions)
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
      dplyr::mutate(Taxa=dplyr::case_when(.data$Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                                          .data$Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                                          .data$Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Cyanobacteria") ~ .data$Taxa,
                                          TRUE ~ "Other taxa"))%>%
      dplyr::mutate(Station=ifelse(.data$Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(.data$Station, .data$Date), .data$Station))%>%
      dplyr::filter(.data$Year>=2008 & .data$Year>Start_year)

    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to phyto dataset
    Data_list[["Phytoplankton"]]<-Data_list[["Phytoplankton"]]%>%
      dplyr::left_join(Stations, by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        .
      } else{
        dplyr::filter(., .data$Region%in%Regions)
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

    FMWT<-deltareportr::wq_fmwt

    STN<-deltareportr::wq_stn

    EDSM<-deltareportr::wq_edsm%>%
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

    Data_list[["Water_quality"]]<-deltareportr::wq_EMP%>%
      dplyr::bind_rows(deltareportr::wq_fmwt, deltareportr::wq_stn, EDSM)%>%
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
        .
      } else{
        dplyr::filter(., .data$Region%in%Regions)
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
