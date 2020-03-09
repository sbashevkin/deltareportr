#' Plot Delta Smelt data
#'
#' Function to process and plot Delta Smelt data
#' @inherit DSCBvalves
#' @param Save Logical. Should plot be saved?
#' @param Save_location If \code{Save=TRUE}, where should plot be saved? Must end with a filename with a ".png" extension.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DSCmapper<-function(Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"),
                    Save=FALSE,
                    Save_location){

  require(sf)
  require(rgdal)
  require(leaflet)
  require(tidyverse)
  require(readxl)
  require(lubridate)
  require(RColorBrewer)

  Deltaregions<-deltareportr::deltaregions%>%
    sf::st_transform(crs=4326)%>%
    dplyr::rename(Region=.data$Stratum)%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    mutate(Region=factor(.data$Region, levels=Regions))

  pal<-leaflet::colorFactor(RColorBrewer::brewer.pal(length(unique(Deltaregions$Region)), "RdYlBu"), Deltaregions$Region)

  p<-leaflet(data=Deltaregions)%>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    setView(lng=-121.804075, lat=38.188039, zoom=10)%>%
    addPolygons(fillColor = ~pal(Region), smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.3, weight=1, color = "black")%>%
    addLegend(pal=pal, values = ~Region, position = "topleft", title = "Region")

  if (Save){
    mapview::mapshot(p, file=Save_location, vheight=850, vwidth=600, zoom=8)
  }

  return(p)
}
