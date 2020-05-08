#' Browse station locations
#'
#' Launches a leaflet map to browse station locations.
#'
#' @param Data Station location data. Defaults to \code{\link{stations}}, which only incldues fixed station locations + EMP EZ locations post 2004. Does not incldue EDSM.
#' Data must include columns named Latitude, Longitude, Source, and StationID.
#' @param Sources Which data sources would you like to include? Set \code{Sources=NULL} (the default) to include all.
#' @param StationIDs Which StationIDs to include. StationIDs take the form "Source, Station". Set \code{StationIDs=NULL} (the default) to include all.
#' @export

StationBrowser <- function(Data=deltareportr::stations, Sources=NULL, StationIDs=NULL){
  Data<-Data%>%
    {if(!is.null(Sources)){
      dplyr::filter(., .data$Source%in%Sources)
    } else{
      .
    }}%>%
    {if(!is.null(StationIDs)){
      dplyr::filter(., .data$StationID%in%StationIDs)
    }else{
      .
    }}


  pal<-leaflet::colorFactor(RColorBrewer::brewer.pal(length(unique(Data$Source)), "Paired"), unique(Data$Source))

  p<-leaflet::leaflet()%>%
    leaflet::addProviderTiles("Esri.WorldGrayCanvas")%>%
    leaflet::addCircleMarkers(data=Data, lat=~Latitude, lng=~Longitude, radius=5,
                              fillOpacity=0.8, weight=1, color="black", fillColor = ~pal(Source),
                              popup = ~StationID)

  return(p)

}
