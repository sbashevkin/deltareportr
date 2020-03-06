
DSCDater <- function(Start_year=2002,
                     Shapefile = deltareportr::deltaregions,
                     Regions=c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River", "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island", "Lower Joaquin River", "Southern Delta"),
                     Variables = c("Bivalves", "Zooplankton", "Phytoplankton", "Water quality")){

  require(sf)
  require(tidyverse)
  require(readxl)
  require(lubridate)

  # Stations ----------------------------------------------------------------

  Stations<-deltareportr::stations%>%
    select(-StationID)%>%
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs=4326)%>%
    st_transform(crs=st_crs(Shapefile))%>%
    st_join(Shapefile, join=st_within)%>%
    as_tibble()%>%
    select(-geometry, -SQM)%>%
    rename(Region=Stratum)

  # Bivalves ----------------------------------------------------------------

  Biv<-deltareportr::bivalves%>%
    select(Date, Station=StationCode, `Potamocorbula amurensis`, `Corbicula fluminea`)%>%
    gather(key="Taxa", value="CPUE", -Station, -Date)%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"),
           Source="EMP")

  #Add regions and lat/long to zoop dataset
  Biv<-Biv%>%
    filter(Year>=Start_year)%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(Region%in%Regions)%>%
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )


# Zooplankton -------------------------------------------------------------

  #**********Only including OTHCYCAD from CB because biomass indicates they're large, and only including small cyclopoids from pump sample******#

  ZoopCB<-deltareportr::zoop_cb%>%
    select(Date, Station, ACARTELA, ACARTIA, DIAPTOM, EURYTEM, OTHCALAD, PDIAPFOR, PDIAPMAR, SINOCAL, TORTANUS, AVERNAL, OTHCYCAD, BOSMINA, DAPHNIA, DIAPHAN, OTHCLADO)%>%
    pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")


  ZoopPump<-deltareportr::zoop_pump%>%
    select(Date=SampleDate, Station, LIMNOSPP, LIMNOSINE, LIMNOTET, OITHDAV, OITHSIM, OITHSPP)%>%
    pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")



  ZoopMysid<-deltareportr::zoop_mysid%>%
    #mutate(Date=parse_date_time(Date, orders="mdy"))%>%
    select(Date, Station, `Acanthomysis aspera`:`Unidentified mysid`)%>%
    mutate(Mysida=rowSums(select(., -Date, -Station), na.rm=T))%>%
    pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "BPUE")%>%
    mutate(BPUE=BPUE*1000,
           Taxa="Mysida") # Convert to ug

  Zoop<-bind_rows(ZoopCB, ZoopPump)%>%
    left_join(deltareportr::zoop_mass_conversions, by=c("Taxa"="taxon"))%>%
    mutate(BPUE=CPUE*mass_indiv_ug,
           Taxa=case_when(
             Taxa%in%c("ACARTELA", "ACARTIA", "DIAPTOM", "EURYTEM", "OTHCALAD", "PDIAPFOR", "PDIAPMAR", "SINOCAL", "TORTANUS") ~ "Calanoida",
             Taxa%in%c("AVERNAL", "LIMNOSPP", "LIMNOSINE", "LIMNOTET", "OITHDAV", "OITHSIM", "OITHSPP", "OTHCYCAD") ~ "Cyclopoida",
             Taxa%in%c("BOSMINA", "DAPHNIA", "DIAPHAN", "OTHCLADO") ~ "Cladocera"))%>%
    select(-CPUE, -mass_indiv_ug)%>%
    bind_rows(ZoopMysid)%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"),
           Source="EMP")

  #Add regions and lat/long to zoop dataset
  Zoop<-Zoop%>%
    filter(Year>=Start_year)%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(Region%in%Regions)%>%
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )


# Phytoplankton -----------------------------------------------------------


  Phyto<-deltareportr::phyto%>%
    rename(Date=SampleDate, Station=StationCode)%>%
    mutate(Date=parse_date_time(Date, "mdy"))%>%
    bind_rows(read_excel("Data/Phytoplankton 2017.xlsx")%>%
                rename(Station=`Station Code`, Cryptophytes=Cryptomonads))%>%
    gather(key="Taxa", value="CPUE", -Date, -Station)%>%
    mutate(Taxa=case_when(Taxa%in%c("Centric Diatoms", "Pennate Diatoms") ~ "Diatoms",
                          Taxa%in%c("Other flagellate", "Unknown Flagellates") ~ "Other flagellates",
                          Taxa%in%c("Cryptophytes", "Green Algae", "Chrysophytes", "Dinoflagellates", "Cyanobacteria") ~ Taxa,
                          TRUE ~ "Other taxa"))%>%
    mutate(Year=year(Date),
           MonthYear=floor_date(Date, unit = "month"),
           Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station),
           Source="EMP")%>%
    filter(Year>=2008 & Year>Start_year)

  # Add regions and summarise -------------------------------------------------------------

  #Add regions and lat/long to phyto dataset
  Phyto<-Phyto%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    filter(Region%in%Regions)%>%
    group_by(Taxa, Region, Year, Date, Station)%>%
    summarise(CPUE=sum(CPUE, na.rm=T))%>%
    ungroup()%>%
    mutate(Month=month(Date))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )


# Water quality -----------------------------------------------------------

  # Load and combine data ---------------------------------------------------

  FMWT<-read_excel("Data/FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet="FlatFile", guess_max=30000)%>%
    select(Date, Station, Conductivity=starts_with("Top EC"), Secchi=`Secchi (m)`, Microcystis, Temperature=starts_with("Top Temperature"))%>%
    mutate(Source="FMWT",
           Secchi=Secchi*100,
           Microcystis=if_else(Microcystis==6, 2, Microcystis))

  STN<-read_excel("Data/STN Sample.xlsx", guess_max=10000)%>%
    select(Date=SampleDate, Station=StationCode, Secchi, Temperature=`TemperatureTop`, Conductivity=`ConductivityTop`, Microcystis
    )%>%
    mutate(Source="TNS")

  EDSM<-read_csv("Data/EDSM_20mm.csv", guess_max=9000)%>%
    select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=TopEC, Temperature=TopTemp, Secchi=Scchi)%>%
    bind_rows(read_csv("Data/EDSM_KDTR.csv", guess_max=30000)%>%
                select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=EC, Temperature=Temp, Secchi=Scchi))%>%
    mutate(Secchi=Secchi*100)%>%
    mutate(Station=paste(Latitude, Longitude),
           Source="EDSM",
           Date=parse_date_time(Date, "%m/%d/%Y"))%>%
    select(-Conductivity)%>% #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
    distinct()%>%
    filter(!is.na(Latitude) & !is.na(Longitude))%>%
    st_as_sf(coords = c("Longitude", "Latitude"), #Add regions to EDSM data
             crs=4326)%>%
    st_transform(crs=st_crs(Shapefile))%>%
    st_join(Shapefile, join=st_within)%>%
    as_tibble()%>%
    select(-geometry, -SQM)%>%
    rename(Region=Stratum)

  WQ<-deltareportr::wq_field%>%
    select(Date=SampleDate, Station=StationCode, Parameter=AnalyteName, Value=Result, Notes=TextResult, Matrix)%>%
    filter(Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)") & Matrix=="Water")%>%
    group_by(Date, Station, Parameter, Notes)%>%
    summarise(Value=mean(Value, na.rm=T))%>%
    ungroup()%>%
    bind_rows(deltareportr::wq_lab%>%
                select(Station=StationCode, Date=SampleDate, Parameter=ConstituentName, Value=Result, Notes=LabAnalysisRemarks)%>%
                filter(Parameter=="Chlorophyll a")%>%
                group_by(Date, Station, Parameter, Notes)%>%
                summarise(Value=mean(Value, na.rm=T))%>%
                ungroup())%>%
    spread(key=Parameter, value=Value)%>%
    rename(Chlorophyll=`Chlorophyll a`, Secchi=`Secchi Depth`, Conductivity=`Conductance (EC)`)%>%
    bind_rows(deltareportr::wq_2000%>%
                select(Station=`Station Name`, Date, Chlorophyll=starts_with("Chlorophyll"), Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`, Microcystis=`Microcystis aeruginosa`, Secchi=`Secchi Depth Centimeters`, Temperature=starts_with("Water Temperature"), Conductivity=starts_with("Specific Conductance"))%>%
                mutate(Chlorophyll=parse_double(ifelse(Chlorophyll%in%c("<0.05", "<0.5"), 0, Chlorophyll)),
                       Latitude=parse_double(Latitude),
                       Longitude=parse_double(Longitude),
                       Microcystis=parse_double(Microcystis),
                       Secchi=parse_double(Secchi),
                       Temperature=parse_double(Temperature),
                       Conductivity=parse_double(Conductivity),
                       Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
                mutate(Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
                select(-Latitude, -Longitude))%>%
    mutate(Source="EMP")%>%
    bind_rows(FMWT, STN, EDSM)%>%
    mutate(MonthYear=floor_date(Date, unit = "month"),
           Year=year(Date),
           Salinity=((0.36966/(((Conductivity*0.001)^(-1.07))-0.00074))*1.28156))

  # Add regions and summarise -------------------------------------------------------------

  #Add regions and lat/long to dataset
  WQsum<-filter(WQ, Source!="EDSM")%>%
    select(-Region)%>%
    left_join(Stations, by=c("Source", "Station"))%>%
    bind_rows(filter(WQ, Source=="EDSM"))%>%
    filter(year(MonthYear)>=Start_year)%>%
    filter(Region%in%Regions)%>%
    mutate(Month=month(MonthYear))%>%
    mutate(Season=case_when(
      Month%in%c(12,1,2) ~ "Winter",
      Month%in%c(3,4,5) ~ "Spring",
      Month%in%c(6,7,8) ~ "Summer",
      Month%in%c(9,10,11) ~ "Fall"),
      Year=if_else(Month==12, Year-1, Year)
    )

# Return ------------------------------------------------------------------

All_dat <- list(Bivalves=Biv, Zooplankton=Zoop, Phytoplankton=Phyto, Water_quality=WQsum)

  return(All_dat)
}
