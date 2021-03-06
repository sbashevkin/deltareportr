#' Plot phytoplankton data
#'
#' Function to process and plot phytoplankton data
#' @inherit DeltaBivalver
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

DeltaPhyter<-function(Data,
                      End_year,
                      Start_year=2002,
                      Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"),
                      Seasons=c("Winter", "Spring", "Summer", "Fall")){

  # Load and combine data ---------------------------------------------------

  Phytosum<-Data%>%
    dplyr::filter(.data$Season%in%Seasons)%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    droplevels()%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Taxa, .data$Season)%>%
    dplyr::mutate(Nmonths = dplyr::n_distinct(.data$Month))%>%
    dplyr::filter(.data$Nmonths>=3)%>%
    dplyr::summarise(CPUE=mean(.data$CPUE, na.rm=T), .groups="drop")%>%
    dplyr::filter(.data$Year>=Start_year)%>%
    dplyr::mutate(Taxa=factor(.data$Taxa, levels=c("Diatoms", "Cryptophytes", "Green Algae", "Chrysophytes", "Cyanobacteria", "Dinoflagellates", "Other flagellates", "Other taxa")),
                  missing="na",
                  Region=as.character(.data$Region))%>%
    tidyr::complete(Year=Start_year:End_year, .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  Phytomissing<-Phytosum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Phytosum<-Phytosum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)

  Peak<-Phytosum%>%
    dplyr::filter(.data$Taxa!="Cyanobacteria")%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Season)%>%
    dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=T), .groups="drop")%>%
    dplyr::filter(.data$CPUE>41000)%>%
    dplyr::mutate(label=format(round(.data$CPUE), big.mark=","))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))


  # Plot --------------------------------------------------------------------

  Phytocyanoplot<-function(season){

    Data_sum<-dplyr::filter(Phytosum, .data$Season==season)
    Data_missing<-dplyr::filter(Phytomissing, .data$Season==season)
    Data_peak<-dplyr::filter(Peak, .data$Season==season)

    pphyto<-ggplot2::ggplot()+
      ggplot2::geom_bar(data=dplyr::filter(Data_sum, .data$Taxa!="Cyanobacteria"), ggplot2::aes(x=.data$Year, y=.data$CPUE, fill=.data$Taxa), stat="identity", alpha=1)+
      {if(End_year%in%unique(Data_sum$Year)){
        ggplot2::geom_bar(data=Data_sum%>%dplyr::filter(.data$Taxa!="Cyanobacteria" & .data$Year==End_year)%>%dplyr::group_by(.data$Region, .data$Year)%>%dplyr::summarise(CPUE=sum(.data$CPUE), .groups="drop")%>%droplevels(), ggplot2::aes(x=.data$Year, y=.data$CPUE), stat="identity", color="firebrick3", fill=NA, size=1)
      }}+
      ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
      ggplot2::geom_label(data=Data_peak, ggplot2::aes(x=.data$Year, y=38000, label=.data$label), size=3, alpha=0.8)+
      ggplot2::scale_fill_brewer(type="div", palette="BrBG", guide=ggplot2::guide_legend(keyheight=0.6, title=NULL), direction=-1)+
      ggplot2::xlab("Date")+
      ggplot2::ylab("Number of cells, colonies, or filaments / ml")+
      ggplot2::coord_cartesian(expand=0, ylim=c(0,41000))+
      ggplot2::scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,max(Phytosum$Year)+1), expand=ggplot2::expansion(0,0))+
      ggplot2::scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","))+
      ggplot2::facet_wrap(~.data$Region, scales="free_x")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5, size=20), legend.position = c(0.85,0.2),
                     legend.background=ggplot2::element_rect(fill="white", color="black"),
                     legend.text = ggplot2::element_text(size=8), plot.margin = ggplot2::margin(r=10))

    pcyano<-ggplot2::ggplot()+
      ggplot2::geom_bar(data=dplyr::filter(Data_sum, .data$Taxa=="Cyanobacteria"), ggplot2::aes(x=.data$Year, y=.data$CPUE), fill="chartreuse4", stat="identity")+
      {if(End_year%in%unique(Data_sum$Year)){
        ggplot2::geom_bar(data=dplyr::filter(Data_sum, .data$Taxa=="Cyanobacteria" & .data$Year==End_year), ggplot2::aes(x=.data$Year, y=.data$CPUE), stat="identity", color="firebrick3", fill=NA, size=1)
      }}+
      ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
      ggplot2::xlab("Date")+
      ggplot2::ylab("Number of cells, colonies, or filaments / ml")+
      ggplot2::scale_x_continuous(labels=insert_minor(seq(2000, 2020, by=5), 4), breaks = 2000:2020, limits=c(Start_year-1,max(Phytosum$Year)+1), expand=ggplot2::expansion(0,0))+
      ggplot2::scale_y_continuous(labels = function(x) format(x, scientific=F, big.mark=","), expand=ggplot2::expansion(0,0))+
      ggplot2::facet_wrap(~.data$Region, scales="free_x")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5, size=20), plot.margin = ggplot2::margin(r=10))
    return(list(Phytoplankton = pphyto, Cyanobacteria = pcyano))
  }

  p<-purrr::map(rlang::set_names(Seasons), Phytocyanoplot)

  Data_out <- Phytosum%>%
    dplyr::mutate(CPUE=round(.data$CPUE,2))%>%
    dplyr::rename(`Count per unit effort` = .data$CPUE)

  return(list(Plots = p, Data = Data_out))

}
