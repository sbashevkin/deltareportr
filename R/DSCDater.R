#' Process report data
#'
#' Imports, filters, and processes datasets and outputs a list of desired datasets
#' @param Start_year Earliest year you would like included in the report. Must be an integer. Defaults to \code{2002}.
#' @param Variables Character vector of variables you would like included in the dataset. Defaults to all possible options: \code{Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality")}.
#' @param WQ_sources Character vector of data sources for the water quality variables. Choices include "EMP" (Environmental Monitoring Program, \code{\link{wq_emp}}), "TNS" (Summer Townet Survey, \code{\link{wq_tns}}), "FMWT" (Fall Midwater Trawl, \code{\link{wq_fmwt}}), "EDSM" (Enhanced Delta Smelt Monitoring, \code{\link{wq_edsm}}), "20mm" (20mm Survey, \code{\link{wq_20mm}}), "SKT" (Spring Kodiak Trawl, \code{\link{wq_skt}}), and "Suisun" (Suisun Marsh Fish Study, \code{\link{wq_suisun}}).
#' @param Shapefile Shapefile you would like used to define regions in the dataset. Must be in \code{\link[sf]{sf}} format, e.g., imported with \code{\link[sf]{st_read}}. Defaults to \code{\link{deltaregions}}.
#' @param Region_column Quoted name of the column in the Shapefile with the region designations.
#' @param Regions Character vector of regions to be included in the dataset. Must correspond with levels of the \code{Region_column}. To include all data points regardless of whether they correspond to a region in the \code{Shapefile} set \code{Regions = NULL}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A list of datasets
#' @export


DSCDater <- function(Start_year=2002,
                     Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality"),
                     WQ_sources = c("EMP", "TNS", "FMWT", "EDSM"),
                     Shapefile = deltareportr::deltaregions,
                     Region_column = "Stratum",
                     Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){

  Region_column <- rlang::sym(Region_column)
  Region_column <- rlang::enquo(Region_column)

  Data_list <- list()

  # Stations ----------------------------------------------------------------

  Stations<-deltareportr::stations%>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs=4326,
                 remove=FALSE)%>%
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
      dplyr::left_join(Stations%>%
                         dplyr::select(-.data$StationID), by=c("Source", "Station"))%>%
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

    #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#

    Data_list[["Zooplankton"]]<-zooper::zoopComb%>%
      dplyr::filter(.data$Source=="EMP" & ((.data$SizeClass=="Meso" & .data$Taxlifestage%in%paste(c("Acartiella sinensis", "Acartia", "Diaptomidae", "Eurytemora affinis", "Calanoida", "Pseudodiaptomus forbesi", "Pseudodiaptomus marinus", "Sinocalanus doerrii", "Tortanus", "Acanthocyclops vernalis", "Cyclopoida", "Bosmina longirostris", "Daphnia", "Diaphanosoma", "Cladocera"), "Adult")) | (.data$SizeClass=="Micro" & .data$Taxlifestage%in%paste(c("Limnoithona", "Limnoithona sinensis", "Limnoithona tetraspina", "Oithona davisae", "Oithona similis", "Oithona"), "Adult"))))%>%
      dplyr::select(.data$Source, .data$Taxlifestage, .data$SampleID, .data$CPUE)%>%
      dplyr::left_join(zooper::zoopEnvComb%>%
                         dplyr::select(.data$Year, .data$Date, .data$Station, .data$SampleID),
                       by="SampleID")%>%
      dplyr::mutate(MonthYear=lubridate::floor_date(.data$Date, unit = "month"))%>%
      dplyr::left_join(deltareportr::zoop_mass_conversions, by=c("Taxlifestage"))%>%
      dplyr::mutate(BPUE=.data$CPUE*.data$Mass,
                    Taxa=dplyr::case_when(
                      .data$Taxlifestage%in%paste(c("Acartiella sinensis", "Acartia", "Diaptomidae",
                                                    "Eurytemora affinis", "Calanoida", "Pseudodiaptomus forbesi",
                                                    "Pseudodiaptomus marinus", "Sinocalanus doerrii", "Tortanus"), "Adult") ~ "Calanoida",
                      .data$Taxlifestage%in%paste(c("Acanthocyclops vernalis", "Cyclopoida", "Limnoithona",
                                                    "Limnoithona sinensis", "Limnoithona tetraspina",
                                                    "Oithona davisae", "Oithona similis", "Oithona"), "Adult") ~ "Cyclopoida",
                      .data$Taxlifestage%in%paste(c("Bosmina longirostris", "Daphnia", "Diaphanosoma", "Cladocera"), "Adult") ~ "Cladocera"))%>%
      dplyr::select(-.data$CPUE, -.data$Mass, -.data$Taxlifestage)%>%
      dplyr::bind_rows(deltareportr::zoop_mysid)

    #Add regions and lat/long to zoop dataset
    Data_list[["Zooplankton"]]<-Data_list[["Zooplankton"]]%>%
      dplyr::filter(.data$Year>=Start_year)%>%
      dplyr::left_join(Stations%>%
                         dplyr::select(-.data$StationID), by=c("Source", "Station"))%>%
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
      dplyr::left_join(Stations%>%
                         dplyr::select(-.data$StationID), by=c("Source", "Station"))%>%
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

  if("Water quality"%in%Variables){

    # Load and combine data ---------------------------------------------------
    WQ_list<-list()

    if("FMWT"%in%WQ_sources){

    WQ_list[["FMWT"]]<-deltareportr::wq_fmwt
    }

    if("TNS"%in%WQ_sources){
    WQ_list[["TNS"]]<-deltareportr::wq_tns
    }

    if("Suisun"%in%WQ_sources){
      WQ_list[["Suisun"]]<-deltareportr::wq_suisun
    }

    if("SKT"%in%WQ_sources){
      WQ_list[["SKT"]]<-deltareportr::wq_skt
    }

    if("20mm"%in%WQ_sources){
      WQ_list[["twentymm"]]<-deltareportr::wq_20mm
    }

    if("EDSM"%in%WQ_sources){
    WQ_list[["EDSM"]]<-deltareportr::wq_edsm
    }

    if("EMP"%in%WQ_sources){
      WQ_list[["EMP"]]<-deltareportr::wq_emp
    }

    Data_list[["Water_quality"]]<-dplyr::bind_rows(WQ_list)%>%
      dplyr::mutate(MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                    Year=lubridate::year(.data$Date),
                    StationID=paste(.data$Source, .data$Station))%>%
      {if("Salinity"%in%names(.)){
        dplyr::mutate(., Salinity=dplyr::if_else(is.na(.data$Salinity), (wql::ec2pss(.data$Conductivity/1000, t=25)), .data$Salinity))
      } else{
        dplyr::mutate(., Salinity=wql::ec2pss(.data$Conductivity/1000, t=25))
      }}



    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to dataset
    Data_list[["Water_quality"]]<-dplyr::filter(Data_list[["Water_quality"]], .data$StationID%in%Stations$StationID)%>%
      dplyr::select(-.data$Latitude, -.data$Longitude)%>%
      dplyr::left_join(Stations, by=c("Source", "Station", "StationID"))%>%
      dplyr::bind_rows(dplyr::filter(Data_list[["Water_quality"]], !(.data$StationID%in%Stations$StationID) & !is.na(.data$Latitude) & !is.na(.data$Longitude))%>%
                         sf::st_as_sf(coords = c("Longitude", "Latitude"), #Add regions to EDSM data
                                      crs=4326,
                                      remove = FALSE)%>%
                         sf::st_transform(crs=sf::st_crs(Shapefile))%>%
                         sf::st_join(Shapefile%>%
                                       dplyr::select(!!Region_column),
                                     join=sf::st_within)%>%
                         tibble::as_tibble()%>%
                         dplyr::select(-.data$geometry)%>%
                         dplyr::rename(Region=!!Region_column))%>%
      dplyr::filter(lubridate::year(.data$MonthYear)>=Start_year)%>%
      {if (is.null(Regions)){
        dplyr::bind_rows(., dplyr::filter(Data_list[["Water_quality"]], !(.data$StationID%in%Stations$StationID) & (is.na(.data$Latitude) | is.na(.data$Longitude))))
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

  if(length(Data_list)==1){
    Data_list<-Data_list[[1]]
  }
  return(Data_list)
}
