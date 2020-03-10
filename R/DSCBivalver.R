#' Plot bivalve data
#'
#' Function to process and plot bivalve data
#' @param Data Input dataset created by \code{\link{DSCDater}}.
#' @param Start_year First year (integer) that should be included in the plot
#' @param End_year Last year (integer) that should be included in the plot. This year will also be highlighted.
#' @param Regions Character vector of regions to include in the plot. The data will be filtered to only include these regions and ordered in the order provided here. To include data with NA regions, set \code{Regions=NULL}.
#' @param Seasons Character vector of seasons to include in the dataset. Should be one of "Summer", "Fall", "Winter", or "Spring".
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return A list with the plot and processed data.
#' @seealso \code{\link{DSCDater}}
#' @export


DSCBivalver<-function(Data,
                      Start_year=2002,
                      End_year=2018,
                      Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"),
                      Seasons="Fall"){

  # Load and combine data ---------------------------------------------------

  if(is.data.frame(Data)){
    Data<-Data
  } else{
    Data<-Data$Bivalves
  }

  Bivsum<-Data%>%
    dplyr::filter(.data$Season%in%Seasons)%>%
    droplevels()%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Taxa)%>%
    dplyr::summarise(CPUE=mean(.data$CPUE, na.rm=T))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(missing="na",
           Region=as.character(.data$Region))%>%
    tidyr::complete(Year=Start_year:(End_year), .data$Region, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  Bivmissing<-Bivsum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region)

  Bivsum<-Bivsum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)



  # Plot --------------------------------------------------------------------

  p<-ggplot2::ggplot()+
    ggplot2::geom_vline(data=Bivmissing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
    ggplot2::geom_bar(data=Bivsum, ggplot2::aes(x=.data$Year, y=.data$CPUE, fill=.data$Taxa), stat="identity")+
    ggplot2::geom_bar(data=Bivsum%>%dplyr::filter(.data$Year==End_year)%>%dplyr::group_by(.data$Region, .data$Year)%>%dplyr::summarise(CPUE=sum(.data$CPUE)), ggplot2::aes(x=.data$Year, y=.data$CPUE), stat="identity", color="firebrick3", fill=NA, size=1)+
    ggplot2::scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,End_year+1), expand=ggplot2::expansion(0,0))+
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","), expand=ggplot2::expansion(0,0))+
    ggplot2::xlab("Date")+
    ggplot2::ylab(bquote(Number~of~clams~"/"~m^2))+
    ggplot2::scale_fill_manual(values=c("#d8b365", "#5ab4ac"), guide=ggplot2::guide_legend(title=NULL))+
    ggplot2::facet_wrap(~.data$Region, scales="free_x")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), legend.position = c(0.85, 0.2), legend.background=ggplot2::element_rect(fill="white", color="black"))

  Data_out <- Bivsum%>%
    dplyr::mutate(CPUE=round(.data$CPUE,2))%>%
    dplyr::rename(`Count per unit effort` = .data$CPUE)

  return(list(Plot = p, Data = Data_out))

}
