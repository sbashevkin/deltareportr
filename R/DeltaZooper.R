#' Plot phytoplankton data
#'
#' Function to process and plot phytoplankton data
#' @inherit DeltaBivalver
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DeltaZooper<-function(Data,
                      End_year,
                      Start_year=2002,
                      Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Lower Joaquin River", "Southern Delta"),
                      Seasons=c("Winter", "Spring", "Summer", "Fall")){


  # Load and combine data --------------------------------------------------


  Zoopsum<-Data%>%
    dplyr::filter(.data$Season%in%Seasons)%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}

  # Filter to only months, years, and regions represented in mysid and other zoop data

  Mysid<-Zoopsum%>%
    dplyr::filter(.data$Taxa=="Mysida")%>%
    dplyr::select(.data$Region, .data$MonthYear)%>%
    dplyr::distinct()

  Meso_Micro<-Zoopsum%>%
    dplyr::filter(.data$Taxa!="Mysida")%>%
    dplyr::select(.data$Region, .data$MonthYear)%>%
    dplyr::distinct()

  Keep<-dplyr::intersect(Mysid, Meso_Micro)

  Zoopsum<-Zoopsum%>%
    dplyr::inner_join(Keep, by=c("MonthYear", "Region"))%>%
    droplevels()%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Taxa, .data$Season)%>%
    dplyr::mutate(Nmonths = dplyr::n_distinct(.data$Month))%>%
    dplyr::filter(.data$Nmonths>=3)%>%
    dplyr::summarise(BPUE=mean(.data$BPUE, na.rm=T), .groups="drop")%>%
    droplevels()%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(Year=Start_year:End_year, .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  Zoopmissing<-Zoopsum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Zoopsum<-Zoopsum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)%>%
    dplyr::mutate(Taxa=factor(.data$Taxa, levels=c("Calanoida", "Cyclopoida", "Cladocera", "Mysida")))

  # Plot --------------------------------------------------------------------

  Zoopplot<-function(season){
    Data_sum<-dplyr::filter(Zoopsum, .data$Season==season)
    Data_missing<-dplyr::filter(Zoopmissing, .data$Season==season)

    ggplot2::ggplot()+
    ggplot2::geom_bar(data=Data_sum, ggplot2::aes(x=.data$Year, y=.data$BPUE, fill=.data$Taxa), stat="identity")+
    ggplot2::geom_bar(data=Data_sum%>%dplyr::filter(.data$Year==End_year)%>%dplyr::group_by(.data$Region, .data$Year)%>%dplyr::summarise(BPUE=sum(.data$BPUE), .groups="drop"), ggplot2::aes(x=.data$Year, y=.data$BPUE), stat="identity", color="firebrick3", fill=NA, size=1)+
    ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
    ggplot2::scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,max(Zoopsum$Year)+1), expand=ggplot2::expansion(0,0))+
    ggplot2::scale_fill_brewer(type="div", palette="BrBG", guide=ggplot2::guide_legend(title=NULL, keyheight=0.8))+
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","), expand=ggplot2::expansion(0,0))+
    ggplot2::xlab("Date")+
    ggplot2::ylab(bquote(Biomass~"("*mu*g*") /"~m^3))+
    ggplot2::facet_wrap(~.data$Region, scales="free_x")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size=20),
                   legend.position = c(0.85,0.2), legend.text=ggplot2::element_text(size=8),
                   legend.background=ggplot2::element_rect(fill="white", color="black"), plot.margin = ggplot2::margin(r=10))

  }

  p<-purrr::map(rlang::set_names(Seasons), Zoopplot)

  Data_out <- Zoopsum%>%
    dplyr::mutate(BPUE=round(.data$BPUE,3))%>%
    dplyr::rename(`Biomass per unit effort` = .data$BPUE)

  return(list(Plot = p, Data = Data_out))

}
