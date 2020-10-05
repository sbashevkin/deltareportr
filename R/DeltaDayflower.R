#' Plot dayflow data
#'
#' Function to process and plot dayflow data
#' @inherit DeltaBivalver
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DeltaDayFlower<-function(End_year,
                         Start_year=2002){

  # Load and summarise data ---------------------------------------------------------------

  DF<-deltareportr::dayflow%>%
    dplyr::filter(lubridate::year(.data$Date)>=Start_year)%>%
    dplyr::mutate(MonthYear=lubridate::floor_date(.data$Date, unit = "month"))%>%
    dplyr::group_by(.data$MonthYear)%>%
    dplyr::summarise(OUT=mean(.data$OUT, na.rm=T), X2=mean(.data$X2, na.rm=T), .groups="drop")


  # Plot data ---------------------------------------------------------------

  Fallshade<-DF%>%
    dplyr::mutate(Year=lubridate::year(.data$MonthYear))%>%
    dplyr::select(.data$Year)%>%
    dplyr::distinct()%>%
    dplyr::mutate(September=lubridate::parse_date_time(paste0("09/", .data$Year), "%m/%Y"),
                  November=lubridate::parse_date_time(paste0("11/", .data$Year), "%m/%Y"))%>%
    dplyr::mutate(X2min=min(DF$X2),
                  X2max=max(DF$X2),
                  OUTmin=min(DF$OUT),
                  OUTmax=max(DF$OUT))

  p<-list()

  p$X2<-ggplot2::ggplot()+
    ggplot2::geom_line(data=DF, ggplot2::aes(x=.data$MonthYear, y=.data$X2), color="dodgerblue4")+
    ggplot2::geom_line(data=dplyr::filter(DF, lubridate::year(.data$MonthYear)==End_year), ggplot2::aes(x=.data$MonthYear, y=.data$X2), color="firebrick3", size=2)+
    ggplot2::geom_rect(data=Fallshade, ggplot2::aes(xmin=.data$September, xmax=.data$November, ymin=.data$X2min, ymax=.data$X2max), alpha=0.4, fill="darkorange1")+
    ggplot2::coord_cartesian(expand=0)+
    ggplot2::scale_x_datetime(labels=insert_minor(seq(2000, 2020, by=5), 4),
                              breaks = seq(lubridate::floor_date(as.POSIXct(as.character(2000), format="%Y"), "year"),
                                           lubridate::floor_date(as.POSIXct(as.character(2020), format="%Y"), "year"), by="1 years"),
                              limits=c(lubridate::floor_date(as.POSIXct(as.character(Start_year), format="%Y"), "year"),
                                       lubridate::floor_date(as.POSIXct(as.character(End_year), format="%Y"), "year")+lubridate::years(1)),
                              expand=ggplot2::expansion(0,0))+
    ggplot2::ylab("X2 (km)")+
    ggplot2::xlab("Date")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=20), plot.margin = ggplot2::margin(r=10))

  p$Out<-ggplot2::ggplot()+
    ggplot2::geom_line(data=DF, ggplot2::aes(x=.data$MonthYear, y=.data$OUT), color="dodgerblue4")+
    ggplot2::geom_line(data=dplyr::filter(DF, lubridate::year(.data$MonthYear)==End_year), ggplot2::aes(x=.data$MonthYear, y=.data$OUT), color="firebrick3", size=2)+
    ggplot2::geom_rect(data=Fallshade, ggplot2::aes(xmin=.data$September, xmax=.data$November, ymin=.data$OUTmin, ymax=.data$OUTmax), alpha=0.4, fill="darkorange1")+
    ggplot2::coord_cartesian(expand=0)+
    ggplot2::scale_x_datetime(labels=insert_minor(seq(2000, 2020, by=5), 4),
                              breaks = seq(lubridate::floor_date(as.POSIXct(as.character(2000), format="%Y"), "year"),
                                           lubridate::floor_date(as.POSIXct(as.character(2020), format="%Y"), "year"), by="1 years"),
                              limits=c(lubridate::floor_date(as.POSIXct(as.character(Start_year), format="%Y"), "year"),
                                       lubridate::floor_date(as.POSIXct(as.character(End_year), format="%Y"), "year")+lubridate::years(1)),
                              expand=ggplot2::expansion(0,0))+
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
    ggplot2::ylab(bquote("Delta"~outflow~"("*ft^3*"/s)"))+
    ggplot2::xlab("Date")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=20), plot.margin = ggplot2::margin(r=10))

  Data_out <- DF%>%
    dplyr::mutate(Month = lubridate::month(.data$MonthYear),
                  Year = lubridate::year(.data$MonthYear),
                  OUT = round(.data$OUT, 2),
                  X2 = round(.data$X2, 2))%>%
    dplyr::select(.data$Month, .data$Year, Outflow=.data$OUT, .data$X2)

  return(list(Plots=p, Data=Data_out))
}
