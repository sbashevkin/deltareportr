#' Plot water quality data
#'
#' Function to process and plot water quality (temperature, Secchi depth, salinity, chlorophyll, and \emph{Microcystis}) data
#' @inherit DeltaBivalver
#' @param Temp_seasons Character vector of seasons to retain for temperature. One plot will be produced for each season. Should be a combination of "Summer", "Fall", "Winter", or "Spring".
#' @param Secchi_seasons Character vector of seasons to retain for Secchi. One plot will be produced for each season. Should be a combination of "Summer", "Fall", "Winter", or "Spring".
#' @param Salinity_seasons Character vector of seasons to retain for salinity. One plot will be produced for each season. Should be a combination of "Summer", "Fall", "Winter", or "Spring".
#' @param Chl_seasons Character vector of seasons to retain for chlorophyll. One plot will be produced for each season. Should be a combination of "Summer", "Fall", "Winter", or "Spring".
#' @param Micro_seasons Character vector of seasons to retain for \emph{Microcystis}. One plot will be produced for each season. Should be a combination of "Summer", "Fall", "Winter", or "Spring".
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export

DeltaWQer<-function(Data,
                    End_year,
                    Start_year=2002,
                    Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"),
                    Temp_seasons=c("Winter", "Spring", "Summer", "Fall"),
                    Secchi_seasons=c("Winter", "Spring", "Summer", "Fall"),
                    Salinity_seasons=c("Winter", "Spring", "Summer", "Fall"),
                    Chl_seasons=c("Winter", "Spring", "Summer", "Fall"),
                    Micro_seasons=c("Winter", "Spring", "Summer", "Fall")){

  Secchisum<-Data%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::select(.data$Month, .data$Region, .data$Secchi, .data$Year, .data$Season)%>%
    dplyr::filter(!is.na(.data$Secchi))%>%
    dplyr::filter(.data$Season%in%Secchi_seasons)%>%
    droplevels()%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Season)%>%
    dplyr::summarise(SD=stats::sd(.data$Secchi, na.rm=T), Secchi=mean(.data$Secchi, na.rm=T), .groups="drop")%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(Year=Start_year:(End_year), .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  Secchimissing<-Secchisum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Secchisum<-Secchisum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)

  Salsum<-Data%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::select(.data$Month, .data$Region, .data$Salinity, .data$Year, .data$Season)%>%
    dplyr::filter(!is.na(.data$Salinity))%>%
    dplyr::filter(.data$Season%in%Salinity_seasons)%>%
    droplevels()%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Season)%>%
    dplyr::summarise(SD=stats::sd(.data$Salinity, na.rm=T), Salinity=mean(.data$Salinity, na.rm=T), .groups="drop")%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(Year=Start_year:(End_year), .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  Salmissing<-Salsum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Salsum<-Salsum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)

  Chlsum<-Data%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::select(.data$Month, .data$Region, .data$Chlorophyll, .data$Year, .data$Season)%>%
    dplyr::filter(!is.na(.data$Chlorophyll))%>%
    dplyr::filter(.data$Season%in%Chl_seasons)%>%
    droplevels()%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Season)%>%
    dplyr::summarise(SD=stats::sd(.data$Chlorophyll, na.rm=T), Chlorophyll=mean(.data$Chlorophyll, na.rm=T), .groups="drop")%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(Year=Start_year:(End_year), .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"))%>%
    dplyr::mutate(Region=factor(.data$Region, levels=Regions))

  Chlmissing<-Chlsum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Chlsum<-Chlsum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)

  Microsum<-Data%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::select(.data$Month, .data$Region, .data$Microcystis, .data$Year, .data$Season)%>%
    dplyr::filter(!is.na(.data$Microcystis))%>%
    dplyr::filter(.data$Season%in%Micro_seasons)%>%
    droplevels()%>%
    dplyr::group_by(.data$Region, .data$Year, .data$Season)%>%
    dplyr::summarise(N_Microcystis=length(which(!is.na(.data$Microcystis))),
                     Microcystis1=length(which(.data$Microcystis==1))/.data$N_Microcystis,
                     Microcystis2=length(which(.data$Microcystis==2))/.data$N_Microcystis,
                     Microcystis3=length(which(.data$Microcystis==3))/.data$N_Microcystis,
                     Microcystis4=length(which(.data$Microcystis==4))/.data$N_Microcystis,
                     Microcystis5=length(which(.data$Microcystis==5))/.data$N_Microcystis, .groups="drop")%>%
    dplyr::filter(.data$N_Microcystis>0)%>%
    tidyr::pivot_longer(c(.data$Microcystis1, .data$Microcystis2, .data$Microcystis3, .data$Microcystis4, .data$Microcystis5), names_to = "Severity", values_to = "Frequency")%>%
    dplyr::mutate(Severity=dplyr::recode(.data$Severity, "Microcystis1"="Absent", "Microcystis2"="Low", "Microcystis3"="Medium", "Microcystis4"="High", "Microcystis5"="Very high"))%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(Year=Start_year:(End_year), .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"),
                  Region=factor(.data$Region, levels=Regions))

  Micromissing<-Microsum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Microsum<-Microsum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)%>%
    dplyr::filter(.data$Severity!="Absent")%>%
    dplyr::mutate(Severity=factor(.data$Severity, levels=c("Very high", "High", "Medium", "Low", "Absent")))

  Tempsum<-Data%>%
    {if (is.null(Regions)){
      .
    } else{
      dplyr::filter(., .data$Region%in%Regions)
    }}%>%
    dplyr::select(.data$Month, .data$Region, .data$Temperature, .data$Year, .data$Season)%>%
    dplyr::filter(.data$Season%in%Temp_seasons & !is.na(.data$Temperature))%>%
    droplevels()%>%
    dplyr::group_by(.data$Year, .data$Region, .data$Season)%>%
    dplyr::mutate(Nmonths = dplyr::n_distinct(.data$Month))%>%
    dplyr::filter(.data$Nmonths>=3)%>%
    dplyr::summarise(SD=stats::sd(.data$Temperature, na.rm=T), Temperature=mean(.data$Temperature, na.rm=T), .groups="drop")%>%
    dplyr::mutate(missing="na")%>%
    tidyr::complete(Year=Start_year:(End_year), .data$Region, .data$Season, fill=list(missing="n.d."))%>%
    dplyr::mutate(missing=dplyr::na_if(.data$missing, "na"),
                  Region=factor(.data$Region, levels=Regions))

  Tempmissing<-Tempsum%>%
    dplyr::filter(.data$missing=="n.d.")%>%
    dplyr::select(.data$Year, .data$Region, .data$Season)

  Tempsum<-Tempsum%>%
    dplyr::filter(is.na(.data$missing))%>%
    dplyr::select(-.data$missing)

  Salrange<-Salsum%>%
    dplyr::filter(!is.na(.data$Salinity))%>%
    dplyr::group_by(.data$Region, .data$Season)%>%
    dplyr::summarise(Salrange=paste0("min: ", round(min(.data$Salinity), 2), ", max: ", round(max(.data$Salinity), 2)), .groups="drop")

  # Plot --------------------------------------------------------------------

  plotWQ<-function(Data, Parameter, ylabel){
    Parameter<-rlang::sym(Parameter)
    Parameter<-rlang::enquo(Parameter)
    ggplot2::ggplot()+
      ggplot2::geom_line(data=Data, ggplot2::aes(x=.data$Year, y=!!Parameter), color="firebrick3")+
      ggplot2::geom_ribbon(data=Data, ggplot2::aes(x=.data$Year, ymin=!!Parameter-.data$SD, ymax=!!Parameter+.data$SD), alpha=0.4, fill="gray")+
      ggplot2::geom_point(data=dplyr::filter(Data, .data$Year==End_year), ggplot2::aes(x=.data$Year, y=!!Parameter), color="firebrick3", size=3)+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(0,0))+
      ggplot2::scale_x_continuous(labels=insert_minor(seq(floor(Start_year/10)*10, ceiling(End_year/10)*10, by=5), 4), breaks = (floor(Start_year/10)*10):(ceiling(End_year/10)*10), limits=c(Start_year,End_year+1), expand=ggplot2::expansion(0,0))+
      ggplot2::facet_wrap(~.data$Region, scales = "free_x")+
      ggplot2::ylab(ylabel)+
      ggplot2::xlab("Date")+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5, size=20), plot.margin = ggplot2::margin(r=10))
  }

  TempShades<-expand.grid(Region=unique(Tempsum$Region), Quality=c("Good", "Marginal", "Bad"))%>%
    dplyr::mutate(xmin=Start_year,
                  xmax=End_year+1,
                  ymin=dplyr::case_when(
                    Quality=="Good" ~ min(Tempsum$Temperature-Tempsum$SD),
                    Quality=="Marginal" ~ 20,
                    Quality=="Bad" ~ 22
                  ),
                  ymax=dplyr::case_when(
                    Quality=="Good" ~ 20,
                    Quality=="Marginal" ~ 22,
                    Quality=="Bad" ~ max(Tempsum$Temperature+Tempsum$SD)
                  ))

Templot<-function(season){
  Data_sum<-dplyr::filter(Tempsum, .data$Season==season)
  Data_missing<-dplyr::filter(Tempmissing, .data$Season==season)

  p<-plotWQ(Data_sum, "Temperature", bquote(Temperature~"("*degree*c*")"))+
    ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
    ggplot2::coord_cartesian(ylim = c(min(Data_sum$Temperature-Data_sum$SD),max(Data_sum$Temperature+Data_sum$SD)))

    if(season%in%c("Summer", "Fall")){
      p<-p+ggplot2::geom_rect(data=TempShades, ggplot2::aes(xmin=.data$xmin, xmax=.data$xmax, ymin=.data$ymin, ymax=.data$ymax, fill=.data$Quality), alpha=0.2)+
      ggplot2::scale_fill_brewer(type="div", palette = "RdYlBu", direction=-1)+
        ggplot2::theme(legend.position = "none")
    }

  return(p)

}

Secchiplot<-function(season){
  Data_sum<-dplyr::filter(Secchisum, .data$Season==season)
  Data_missing<-dplyr::filter(Secchimissing, .data$Season==season)

  plotWQ(Data_sum, "Secchi", "Secchi depth (cm)")+
    ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)
}

Chlaplot<-function(season){
  Data_sum<-dplyr::filter(Chlsum, .data$Season==season)
  Data_missing<-dplyr::filter(Chlmissing, .data$Season==season)

  plotWQ(Data_sum, "Chlorophyll", bquote(Chlorophyll~a~"("*mu*g*"/L)"))+
    ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)
}

Salplot<-function(season){
  Data_sum<-dplyr::filter(Salsum, .data$Season==season)
  Data_missing<-dplyr::filter(Salmissing, .data$Season==season)
  Data_range<-dplyr::filter(Salrange, .data$Season==season)

  plotWQ(Data_sum, "Salinity", "Salinity")+
    ggplot2::geom_label(data=Data_range, ggplot2::aes(x=2006, y=15, label=.data$Salrange), alpha=0.5, size=2.5)+
    ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
    ggplot2::coord_cartesian(ylim = c(0,max(Data_sum$Salinity+Data_sum$SD)))
}

Microplot<-function(season){
  Data_sum<-dplyr::filter(Microsum, .data$Season==season)
  Data_missing<-dplyr::filter(Micromissing, .data$Season==season)

  ggplot2::ggplot()+
    ggplot2::geom_bar(data=Data_sum, ggplot2::aes(x=.data$Year, y=.data$Frequency, fill=.data$Severity), stat="identity")+
    ggplot2::geom_bar(data=tibble::tibble(End_year), ggplot2::aes(x=End_year, y=1), stat="identity", color="firebrick3", fill=NA, size=1)+
    ggplot2::geom_vline(data=Data_missing, ggplot2::aes(xintercept=.data$Year), linetype=2)+
    ggplot2::scale_fill_brewer(type="div", palette="RdYlBu", guide=ggplot2::guide_legend(keyheight=0.8, title=NULL, direction="horizontal", label.position="top", reverse=TRUE))+
    ggplot2::scale_x_continuous(labels=insert_minor(seq(floor(Start_year/10)*10, ceiling(End_year/10)*10, by=5), 4), breaks = (floor(Start_year/10)*10):(ceiling(End_year/10)*10), limits=c(Start_year,End_year+1), expand=ggplot2::expansion(0,0))+
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(0,0))+
    ggplot2::facet_wrap(~.data$Region, scales = "free_x")+
    ggplot2::ylab("Relative frequency")+
    ggplot2::xlab("Date")+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.grid=ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size=20), legend.position=c(0.85,0.2),
                   legend.background=ggplot2::element_rect(fill="white", color="black"),
                   legend.text = ggplot2::element_text(size=8), plot.margin = ggplot2::margin(r=10))

}

pTemp<-purrr::map(rlang::set_names(Temp_seasons), Templot)
pSecchi<-purrr::map(rlang::set_names(Secchi_seasons), Secchiplot)
pSal<-purrr::map(rlang::set_names(Salinity_seasons), Salplot)
pChla<-purrr::map(rlang::set_names(Chl_seasons), Chlaplot)
pMicro<-purrr::map(rlang::set_names(Micro_seasons), Microplot)

  Datacleaner<-function(Data, Parameter){
    Parameter <- rlang::sym(Parameter)
    Parameter <- rlang::enquo(Parameter)
    out <- Data%>%
      dplyr::mutate(SD=round(.data$SD, 2),
                    !!Parameter := round(!!Parameter, 2))%>%
      dplyr::select(.data$Year, .data$Region, `Standard deviation` = .data$SD, !!Parameter)
    return(out)
  }

  Tempdata <- Datacleaner(Tempsum, "Temperature")
  Secchidata <- Datacleaner(Secchisum, "Secchi")
  Saldata <- Datacleaner(Salsum, "Salinity")
  Chldata <- Datacleaner(Chlsum, "Chlorophyll")
  Microdata <- Microsum%>%
    dplyr::mutate(Frequency = round(.data$Frequency, 2))%>%
    dplyr::rename(Samples = .data$N_Microcystis, `Relative frequency`=.data$Frequency)

  plots<-list(Temperature = list(Plot = pTemp, Data = Tempdata),
              Secchi = list(Plot = pSecchi, Data = Secchidata),
              Salinity = list(Plot = pSal, Data = Saldata),
              Chlorophyll = list(Plot = pChla, Data = Chldata),
              Microcystis = list(Plot = pMicro, Data = Microdata))

  return(plots)

}
