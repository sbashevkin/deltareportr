#' Plot Metadata
#'
#' Function to process and plot metadata
#' @inherit DSCBvalves
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DSCMetadater<-function(Data,
                       Start_year=2002,
                       Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta")){


  # Water quality data ------------------------------------------------------



  # Load and combine data ---------------------------------------------------


  WQsum<-Data$Water_quality%>%
    dplyr::select(.data$Region, .data$Season, .data$Year, .data$Source, .data$Chlorophyll, .data$Salinity, .data$Secchi, .data$Temperature, .data$Microcystis)%>%
    tidyr::pivot_longer(c(.data$Chlorophyll, .data$Salinity, .data$Secchi, .data$Temperature, .data$Microcystis), names_to="Parameter", values_to = "Value")%>%
    dplyr::filter(!is.na(.data$Value))%>%
    dplyr::select(-.data$Value)


  # Bivalves ----------------------------------------------------------------

  Bivsum<-Data$Bivalves%>%
    dplyr::select(.data$Region, .data$Season, .data$Year)%>%
    dplyr::mutate(Source="EMP",
                  Parameter="Bivalves")


  # Zooplankton -------------------------------------------------------------

  Zoopsum<-Data$Zooplankton%>%
    dplyr::select(-.data$Taxa, -.data$BPUE)%>%
    dplyr::distinct()%>%
    dplyr::select(.data$Region, .data$Season, .data$Year)%>%
    dplyr::mutate(Source="EMP",
                  Parameter="Zooplankton")


  # Phytoplankton -----------------------------------------------------------

  Phytosum<-Data$Phytoplankton%>%
    dplyr::select(.data$Region, .data$Season, .data$Year)%>%
    dplyr::mutate(Source="EMP",
                  Parameter="Phytoplankton")

  # Combine all datasets ----------------------------------------------------

  sum<-dplyr::bind_rows(WQsum, Bivsum, Zoopsum, Phytosum)%>%
    dplyr::group_by(.data$Parameter)%>%
    dplyr::mutate(Years=length(unique(.data$Year)))%>%
    dplyr::ungroup()%>%
    dplyr::group_by(.data$Region, .data$Season, .data$Source, .data$Parameter, .data$Years)%>%
    dplyr::summarise(N=dplyr::n())%>%
    dplyr::ungroup()%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions),
                  Yearly_samples=.data$N/.data$Years,
                  Season=factor(.data$Season, levels=c("Winter", "Spring", "Summer", "Fall")),
                  Season=dplyr::recode(.data$Season, Winter="Winter\nDec - Feb", Spring="Spring\nMar - May", Summer="Summer\nJun - Aug", Fall="Fall\nSep - Nov"),
                  Source=dplyr::recode(.data$Source, TNS="STN"),
                  Parameter=factor(.data$Parameter, levels=c("Temperature", "Secchi", "Salinity", "Chlorophyll", "Phytoplankton", "Microcystis", "Zooplankton", "Bivalves")))


  # Plot --------------------------------------------------------------------

  p<-ggplot2::ggplot(sum, ggplot2::aes(x=.data$Region, y=.data$Yearly_samples, fill=.data$Source))+
    ggplot2::geom_bar(stat="identity")+
    ggplot2::facet_grid(.data$Parameter~.data$Season, scales = "free_y")+
    ggplot2::scale_fill_colorblind(guide=ggplot2::guide_legend(direction="horizontal"))+
    ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,NA))+
    ggplot2::ylab("Average number of data points per year")+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1), panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), text=ggplot2::element_text(size=12), plot.margin = ggplot2::margin(35,0,0,35), strip.text.y = ggplot2::element_text(angle=0, hjust=0), panel.spacing.y = ggplot2::unit(0.5, "lines"), legend.position=c(0.5, 1.1), legend.background = ggplot2::element_rect(color="black"))

  Data_out <- sum%>%
    dplyr::mutate(Yearly_samples = round(.data$Yearly_samples, 2),
                  Season = dplyr::recode(.data$Season, "Winter\nDec - Feb"="Winter", "Spring\nMar - May"="Spring", "Summer\nJun - Aug"="Summer", "Fall\nSep - Nov"="Fall"))%>%
    dplyr::rename(`Total samples`=.data$N, `Samples per year` = .data$Yearly_samples, `Years sampled` = .data$Years)

  return(list(Plot=p, Data=Data_out))

}
