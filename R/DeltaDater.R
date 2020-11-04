#' Process report data
#'
#' Imports, filters, and processes datasets and outputs a list of desired datasets
#' @param Start_year Earliest year you would like included in the report. Must be an integer. Defaults to \code{2002}.
#' @param End_year Latest year you would like included in the dataset. Must be an integer. Defaults to \code{2020}.
#' @param Variables Character vector of variables you would like included in the dataset.
#'   Defaults to all possible options: \code{Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality")}.
#' @param WQ_sources Character vector of data sources for the water quality variables, pulled from the \code{\link[discretewq]{discretewq}} package.
#'   Choices include "EMP" (Environmental Monitoring Program, \code{\link[discretewq]{EMP}}),
#'   "STN" (Summer Townet Survey, \code{\link[discretewq]{STN}}),
#'   "FMWT" (Fall Midwater Trawl, \code{\link[discretewq]{FMWT}}),
#'   "EDSM" (Enhanced Delta Smelt Monitoring, \code{\link[discretewq]{EDSM}}),
#'   "DJFMP" (Delta Juvenile Fish Monitoring Program, \code{\link[discretewq]{DJFMP}}),
#'   "20mm" (20mm Survey, \code{\link[discretewq]{twentymm}}),
#'   "SKT" (Spring Kodiak Trawl, \code{\link[discretewq]{SKT}}),
#'   "Baystudy" (Bay Study, \code{\link[discretewq]{baystudy}}),
#'   "USGS" (USGS San Francisco Bay Surveys, \code{\link[discretewq]{USGS}}),
#'   "USBR" (United States Bureau of Reclamation Sacramento Deepwater Ship Channel data, \code{\link[discretewq]{USBR}}), and
#'   "Suisun" (Suisun Marsh Fish Study, \code{\link[discretewq]{suisun}}).
#' @param Shapefile Shapefile you would like used to define regions in the dataset. Must be in \code{\link[sf]{sf}} format, e.g., imported with \code{\link[sf]{st_read}}. Defaults to \code{\link[deltamapr]{R_EDSM_Strata_1819P1}}.
#' @param Region_column Quoted name of the column in the Shapefile with the region designations.
#' @param Regions Character vector of regions to be included in the dataset. Must correspond with levels of the \code{Region_column}. To include all data points regardless of whether they correspond to a region in the \code{Shapefile} set \code{Regions = NULL}.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A list of datasets
#' @examples
#' Data <- DeltaDater(Start_year = 1900,
#' WQ_sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "SKT",
#' "20mm", "Suisun", "Baystudy", "USBR", "USGS"),
#' Variables = "Water quality",
#' Regions = NULL)
#' @export


DeltaDater <- function(Start_year=2002,
                       End_year=2020,
                       Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality"),
                       WQ_sources = c("EMP", "STN", "FMWT", "EDSM"),
                       Shapefile = deltamapr::R_EDSM_Strata_1819P1,
                       Region_column = "Stratum",
                       Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){

  Region_column2 <- rlang::sym(Region_column)
  Region_column2 <- rlang::enquo(Region_column2)

  Data_list <- list()

  # Stations ----------------------------------------------------------------

  Stations<-deltareportr::stations%>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs=4326,
                 remove=FALSE)%>%
    sf::st_transform(crs=sf::st_crs(Shapefile))%>%
    sf::st_join(Shapefile%>%
                  dplyr::select(!!Region_column2))%>%
    tibble::as_tibble()%>%
    dplyr::select(-.data$geometry)%>%
    dplyr::rename(Region=!!Region_column2)

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
      dplyr::group_by(.data$Source, .data$SampleID, .data$Year, .data$Date, .data$Station, .data$MonthYear, .data$Taxa)%>%
      dplyr::summarise(BPUE=sum(.data$BPUE, na.rm=T), .groups="drop")%>%
      dplyr::bind_rows(deltareportr::zoop_mysid)

    #Add regions and lat/long to zoop dataset
    Data_list[["Zooplankton"]]<-Data_list[["Zooplankton"]]%>%
      dplyr::filter(.data$Year>=Start_year)%>%
      dplyr::mutate(Station=ifelse(.data$Station%in%c("NZEZ2", "NZEZ6", "NZEZ2SJR", "NZEZ6SJR"), paste(.data$Station, .data$Date), .data$Station))%>%
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
      dplyr::filter(.data$Year>=2008 & .data$Year>Start_year)%>%
      dplyr::group_by(.data$Taxa, .data$Year, .data$Date, .data$Station, .data$Source)%>%
      dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T), .groups="drop")

    # Add regions and summarise -------------------------------------------------------------

    #Add regions and lat/long to phyto dataset
    Data_list[["Phytoplankton"]]<-Data_list[["Phytoplankton"]]%>%
      dplyr::left_join(Stations%>%
                         dplyr::select(.data$Station, .data$Source, .data$Region), by=c("Source", "Station"))%>%
      {if (is.null(Regions)){
        .
      } else{
        dplyr::filter(., .data$Region%in%Regions)
      }}%>%
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

    Data_list[["Water_quality"]]<-discretewq::wq(Start_year=Start_year,
                                                 End_year=End_year,
                                                 Sources=WQ_sources,
                                                 Shapefile = Shapefile,
                                                 Region_column = Region_column,
                                                 Regions = Regions)
  }

  # Return ------------------------------------------------------------------

  if(length(Data_list)==1){
    Data_list<-Data_list[[1]]
  }
  return(Data_list)
}
