#' Plot Delta Regions Map
#'
#' Function to map delta regions
#' @inherit DeltaBivalver
#' @param Save Logical. Should plot be saved?
#' @param Save_location If \code{Save=TRUE}, where should plot be saved? Must end with a filename with a ".png" extension.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DeltaMapper<-function(Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"),
                    Save=FALSE,
                    Save_location){

  Deltaregions<-deltamapr::R_EDSM_Strata_1819P1%>%
    sf::st_transform(crs=4326)%>%
    dplyr::rename(Region=.data$Stratum)%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  pal<-leaflet::colorFactor(RColorBrewer::brewer.pal(length(unique(Deltaregions$Region)), "RdYlBu"), Deltaregions$Region)

  p<-leaflet::leaflet(data=Deltaregions)%>%
    leaflet::addProviderTiles("Esri.WorldGrayCanvas")%>%
    leaflet::setView(lng=-121.804075, lat=38.188039, zoom=10)%>%
    leaflet::addPolygons(fillColor = ~pal(Region), smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.3, weight=1, color = "black")%>%
    leaflet::addLegend(pal=pal, values = ~Region, position = "topleft", title = "Region")

  if (Save){
    mapview::mapshot(p, file=Save_location, vheight=850, vwidth=600, zoom=8)
  }

  invisible(p)
}
