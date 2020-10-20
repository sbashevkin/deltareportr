#' Plot Delta Smelt data
#'
#' Function to process and plot Delta Smelt data
#' @inherit DeltaBivalver
#' @param EDSM_regions Character vector of regions to include in the EDSM plot. The data will be filtered to only include these regions and ordered in the order provided here. To include data with NA regions, set \code{Regions=NULL}. No Delta Smelt were caught in the Southern Delta so that region is excluded by default.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DeltaSmelter<-function(End_year,
                       Start_year=2002,
                       EDSM_regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River")){

  # Load and combine data ---------------------------------------------------

  IEP_Indices<-deltareportr::smelt_iep%>%
    dplyr::filter(.data$Year>=Start_year)%>%
    dplyr::mutate(Source=factor(.data$Source, levels=c("SKT", "STN", "20mm", "FMWT")))%>%
    dplyr::select(.data$Year, .data$Source, .data$Index)

  EDSM<-deltareportr::smelt_edsm%>%
    dplyr::filter(!is.na(.data$Abundance))%>%
    dplyr::mutate(Variance=tidyr::replace_na(.data$Variance, 0))%>%
    dplyr::group_by(.data$Region, .data$MonthYear)%>%
    dplyr::summarise(Abundance=mean(.data$Abundance, na.rm=T), Abundance_CV=sqrt((1/dplyr::n()^2)*sum(.data$Variance))/.data$Abundance, .groups="drop")%>%
    dplyr::mutate(l95=stats::qlnorm(0.025, meanlog=log(.data$Abundance/sqrt(1+.data$Abundance_CV^2)), sdlog=log(1+.data$Abundance_CV^2)),
                  u95=stats::qlnorm(0.975, meanlog=log(.data$Abundance/sqrt(1+.data$Abundance_CV^2)), sdlog=log(1+.data$Abundance_CV^2)))%>%
    dplyr::mutate(Abundance_l=log10(.data$Abundance+1),
                  l95_l=log10(.data$l95),
                  u95_l=log10(.data$u95))%>%
    {if (is.null(EDSM_regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%EDSM_regions)
    }}%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(.data$MonthYear, .data$Region, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=EDSM_regions))

  EDSMmissing<-EDSM%>%
    dplyr::filter(missing=="n.d.")%>%
    dplyr::select(.data$MonthYear, .data$Region)

  EDSM<-EDSM%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)

  # Plot --------------------------------------------------------------------

  p<-list()

  p$IEP<-ggplot2::ggplot()+
    ggplot2::geom_line(data=IEP_Indices, ggplot2::aes(x=.data$Year, y=.data$Index, color=.data$Source), size=1)+
    ggplot2::geom_point(data=IEP_Indices, ggplot2::aes(x=.data$Year, y=.data$Index, color=.data$Source))+
    ggplot2::geom_point(data=dplyr::filter(IEP_Indices, .data$Year==End_year), ggplot2::aes(x=.data$Year, y=.data$Index, color=.data$Source), size=3, color="firebrick3")+
    ggplot2::facet_grid(.data$Source~., scales = "free_y")+
    ggplot2::scale_color_brewer(type="div", palette="RdYlBu", guide="none")+
    ggplot2::scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year,End_year+1), expand=ggplot2::expansion(0,0))+
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=c(0,0.05)), limits = c(0,NA))+
    ggplot2::ylab("Index value")+
    ggplot2::xlab("Date")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=20), panel.spacing = ggplot2::unit(0.2, "in"))

  p$EDSM<-ggplot2::ggplot()+
    ggplot2::geom_line(data=EDSM, ggplot2::aes(x=.data$MonthYear, y=.data$Abundance_l), color="darkorchid4")+
    ggplot2::geom_errorbar(data=EDSM, ggplot2::aes(x=.data$MonthYear, ymax=.data$u95_l, ymin=.data$l95_l))+
    ggplot2::geom_point(data=dplyr::filter(EDSM, lubridate::year(.data$MonthYear)!=End_year), ggplot2::aes(x=.data$MonthYear, y=.data$Abundance_l), color="darkorchid4")+
    ggplot2::geom_point(data=dplyr::filter(EDSM, lubridate::year(.data$MonthYear)==End_year), ggplot2::aes(x=.data$MonthYear, y=.data$Abundance_l), color="firebrick3", size=2.3)+
    ggplot2::geom_vline(data=EDSMmissing, ggplot2::aes(xintercept=.data$MonthYear), linetype=2)+
    ggplot2::coord_cartesian(ylim=c(2,5.7))+
    ggplot2::facet_wrap(~.data$Region)+
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks="3 months")+
    ggplot2::scale_y_continuous(labels = function(x) format(10^x, scientific=F, big.mark=","))+
    ggplot2::ylab("Delta Smelt abundance")+
    ggplot2::xlab("Date")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=20), axis.text.x = ggplot2::element_text(angle=45, hjust=1))

  EDSM_out <- EDSM%>%
    dplyr::mutate(Month=lubridate::month(.data$MonthYear),
                  Year=lubridate::year(.data$MonthYear),
                  Abundance=round(.data$Abundance, 2),
                  l95 = round(.data$l95, 2),
                  u95 = round(.data$u95, 2))%>%
    dplyr::select(.data$Month, .data$Year, `Estimated abundance` = .data$Abundance, `Lower 95% CI` = .data$l95, `Upper 95% CI` = .data$u95)

  return(list(IEP = list(Plot = p$IEP, Data = IEP_Indices), EDSM= list(Plot = p$EDSM, Data = EDSM_out)))

}
