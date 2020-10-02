## code to prepare `abiotic` dataset goes here

library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(tidyselect)
library(tidyr)
library(stringr)
library(purrr)

Download <- FALSE

#FMWT
if(Download){
  #EDSM 20mm
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=d468c513fa69c4fc6ddc02e443785f28", file.path("data-raw", "data", "EDSM", "EDSM_20mm.csv"), mode="wb")
  #EDSM KDTR
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.3&entityid=4d7de6f0a38eff744a009a92083d37ae", file.path("data-raw", "data", "EDSM", "EDSM_KDTR.csv"), mode="wb")
  #DJFMP downloaded when file is loaded since file sizes are so large

  #EMP
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.3&entityid=abedcf730c2490fde0237df58c897556", file.path("data-raw", "data", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), mode="wb")

}

wq_fmwt <- read_csv(file.path("data-raw", "data", "FMWT", "Sample.csv"),
                    col_types = cols_only(StationCode="c", SampleTimeStart="c", WaterTemperature="d", Secchi="d",
                                          ConductivityTop="d", TideCode="i", DepthBottom="d",
                                          Microcystis="d", BottomTemperature="d", DateID="i"))%>%
  left_join(read_csv(file.path("data-raw", "data", "FMWT", "Date.csv"), col_types=cols_only(DateID="i", SampleDate="c"))%>%
              mutate(SampleDate=parse_date_time(SampleDate, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
              rename(Date=SampleDate),
            by="DateID")%>% # Add dates
  rename(Station=StationCode, Tide=TideCode, Time=SampleTimeStart, Depth=DepthBottom, Conductivity=ConductivityTop, Temperature=WaterTemperature,
         Temperature_bottom=BottomTemperature)%>%
  mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=if_else(hour(Time)==0, parse_date_time(NA_character_, tz="America/Los_Angeles"), Time),
         Tide=recode(Tide, `1` = "High Slack", `2` = "Ebb", `3` = "Low Slack", `4` = "Flood"),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  mutate(Microcystis=if_else(Microcystis==6, 2, Microcystis),
         Source="FMWT",
         Secchi=Secchi*100, #convert to cm
         Depth = Depth*0.3048)%>% # Convert to meters
  select(Source, Station, Date, Datetime, Depth, Tide, Microcystis, Secchi, Temperature, Temperature_bottom, Conductivity)


wq_stn<-read_csv(file.path("data-raw", "data", "STN", "Sample.csv"),
                 col_types=cols_only(SampleRowID="i", SampleDate="c", StationCode="c",
                                TemperatureTop="d", TemperatureBottom="d",
                                Secchi="d", ConductivityTop="d",
                                TideCode="i", DepthBottom='d', SampleComments="c",
                                Microcystis="d"))%>%
  rename(Date=SampleDate, Station=StationCode,
         Temperature=TemperatureTop, Conductivity=ConductivityTop,
         Tide=TideCode, Depth=DepthBottom, Notes=SampleComments,
         Temperature_bottom=TemperatureBottom)%>%
  mutate(Source="STN",
         Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(read_csv(file.path("data-raw", "data", "STN", "TowEffort.csv"),
                       col_types=cols_only(SampleRowID="i", TimeStart="c"))%>%
              select(SampleRowID, Time=TimeStart)%>%
              mutate(Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
              drop_na()%>%
              group_by(SampleRowID)%>%
              summarise(Time=min(Time))%>%
              ungroup(),
            by="SampleRowID")%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048)%>% #Convert feet to meters
  select(Source, Station, Date, Datetime, Depth, Tide, Microcystis, Secchi, Temperature, Temperature_bottom, Conductivity, Notes)
tz(wq_stn$Date)<-"America/Los_Angeles"

#Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
wq_edsm <- read_csv(file.path("data-raw", "data", "EDSM", "EDSM_20mm.csv"),
                    col_types=cols_only(Date="c", StartLat="d", StartLong="d",
                                        TopTemp="d", BottomTemp="d", Scchi="d", Time="c",
                                        Tide="c", Depth="d", SampleComments="c"))%>%
  rename(Latitude=StartLat, Longitude=StartLong, Temperature=TopTemp, Secchi=Scchi, Notes = SampleComments, Temperature_bottom=BottomTemp)%>%
  bind_rows(read_csv(file.path("data-raw", "data", "EDSM", "EDSM_KDTR.csv"),
                     col_types=cols_only(Date="c", StartLat="d", StartLong="d",
                                         Temp="d", Scchi="d", Time="c",
                                         Tide="c", StartDepth="d", Comments="c"))%>%
              rename(Latitude=StartLat, Longitude=StartLong, Temperature=Temp, Secchi=Scchi, Depth=StartDepth, Notes=Comments))%>%
  mutate(Secchi=Secchi*100, # convert Secchi to cm
         Tide=recode(Tide, HS="High Slack", LS = "Low Slack"),
         Tide=na_if(Tide, "n/p"))%>% #Standardize tide codes
  mutate(Station=paste(Latitude, Longitude),
         Source="EDSM",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  distinct()%>%
  mutate(Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Temperature_bottom, Notes)

wq_emp<-read_csv(file.path("data-raw", "data", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), na=c("NA", "ND"),
                   col_types = cols_only(Station="c", Date="c", Time="c", Chla="d",
                                         Depth="d", Secchi="d", Microcystis="d", SpCndSurface="d",
                                         WTSurface="d", WTBottom='d'))%>%
  rename(Chlorophyll=Chla, Conductivity=SpCndSurface, Temperature=WTSurface,
         Temperature_bottom=WTBottom)%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste(Date, Time)), "%m/%d/%Y %H:%M", tz="America/Los_Angeles"),
         Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
  select(-Time)%>%
  mutate(Datetime=if_else(hour(Datetime)==0, parse_date_time(NA_character_, tz="America/Los_Angeles"), Datetime))%>%
  mutate(Source="EMP",
         Tide = "High Slack",
         Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station),
         Depth=Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Date, Datetime, Depth, Tide, Microcystis, Chlorophyll, Secchi, Temperature, Temperature_bottom, Conductivity)

wq_skt <- read_csv(file.path("data-raw", "data", "SKT", "tblSample.csv"),
                   col_types = cols_only(SampleDate="c", StationCode="c", SampleTimeStart="c", Secchi="d", ConductivityTop="d",
                                         WaterTemperature="d", DepthBottom="d", TideCode="i",
                                         SampleComments="c", Latitude="c", Longitude="c"))%>%
  select(Date=SampleDate, Station=StationCode, Time=SampleTimeStart, Secchi,
         Conductivity=ConductivityTop, Temperature=WaterTemperature, Depth=DepthBottom, Tide=TideCode,
         Notes=SampleComments, Latitude, Longitude)%>%
  mutate(Latitude=na_if(Latitude, "0"),
         Longitude=na_if(Longitude, "0"),
         Date = parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Source="SKT",
         Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Longitude=if_else(Longitude=="121-29.41.7", "121-29-41.7", Longitude))%>%
  mutate(Latitude = str_remove(Latitude, '".*'),
         Longitude = str_remove(Longitude, '".*'))%>%
  mutate(Latitude = str_remove(Latitude, "'.*"),
         Longitude = str_remove(Longitude, "'.*"))%>%
  mutate(Latitude = str_remove(Latitude, "[a-zA-Z]"),
         Longitude = str_remove(Longitude, "[a-zA-Z]"))%>%
  separate(Latitude, into=c("LatD", "LatM", "LatS"), sep="-", remove=TRUE, convert=TRUE)%>%
  separate(Longitude, into=c("LonD", "LonM", "LonS"), sep="-", remove=TRUE, convert=TRUE)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1)%>%
  mutate(Datetime = parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time, -LatD, -LatM, -LatS, -LonD, -LonM, -LonS)%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity, Notes)



wq_20mm <- read_csv(file.path("data-raw", "data", "20mm", "Station.csv"),
                    col_types = cols_only(StationID="c", SurveyID="c", Station="c",
                                          LatDeg="d", LatMin="d", LatSec="d", LonDeg="d",
                                          LonMin="d", LonSec="d", Temp="d", TopEC="d",
                                          Secchi="d", Comments="c"))%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LonDeg+LonMin/60+LonSec/3600)*-1)%>%
  left_join(read_csv(file.path("data-raw", "data", "20mm", "Survey.csv"),
                     col_types = cols_only(SurveyID="c", SampleDate = "c"))%>%
              rename(Date=SampleDate)%>%
              mutate(Date = parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles")),
            by="SurveyID")%>%
  left_join(read_csv(file.path("data-raw", "data", "20mm", "Tow.csv"),
                     col_types = cols_only(StationID="c", TowTime="c", Tide="d", BottomDepth="d", TowNum="d"))%>%
              rename(Time=TowTime)%>%
              mutate(Time = parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
              group_by(StationID)%>%
              mutate(Retain=if_else(Time==min(Time), TRUE, FALSE))%>%
              ungroup()%>%
              filter(Retain)%>%
              select(-Retain)%>%
              group_by(StationID)%>%
              mutate(Retain=if_else(TowNum==min(TowNum), TRUE, FALSE))%>%
              ungroup()%>%
              filter(Retain)%>%
              select(-Retain, -TowNum),
            by="StationID")%>%
  select(Station, Temperature=Temp, Conductivity=TopEC, Secchi,
         Notes=Comments, Latitude, Longitude, Date,
         Time, Depth=BottomDepth, Tide)%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Source = "20mm",
         Depth = Depth*0.3048)%>% # Convert feet to meters
  select(Source, Station, Latitude, Longitude, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity, Notes)

#Removing salinity because data do not correspond well with conductivity
wq_suisun<-read_csv(file.path("data-raw", "data", "Suisun", "Suisun_Sample.csv"),
                    col_types = cols_only(SampleRowID="c", StationCode="c", SampleDate="c", SampleTime="c",
                                          QADone="l", WaterTemperature="d",
                                          Secchi="d", SpecificConductance="d", TideCode="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Conductivity=SpecificConductance, Tide=TideCode)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"),
         Source="Suisun")%>%
  left_join(read_csv(file.path("data-raw", "data", "Suisun", "Suisun_Depth.csv"),
                     col_types=cols_only(SampleRowID="c", Depth="d"))%>%
              group_by(SampleRowID)%>%
              summarise(Depth=mean(Depth, na.rm=T))%>%
              ungroup(),
            by="SampleRowID")%>%
  select(Source, Station, Date, Datetime, Depth, Tide, Secchi, Temperature, Conductivity)

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=71c16ead9b8ffa4da7a52da180f601f4", file.path(tempdir(), "DJFMP_1976-2001.csv"), mode="wb",method="libcurl")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=c4726f68b76c93a7e8a1e13e343ebae2", file.path(tempdir(), "DJFMP_2002-2019.csv"), mode="wb",method="libcurl")

wq_djfmp <- read_csv(file.path(tempdir(), "DJFMP_1976-2001.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c",
                                           SampleTime="c", WaterTemperature="d", Secchi="d"))%>%
  bind_rows(read_csv(file.path(tempdir(), "DJFMP_2002-2019.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c",
                                           SampleTime="c", WaterTemperature="d", Secchi="d")))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime, Temperature=WaterTemperature)%>%
  mutate(Secchi=Secchi*100)%>% # convert Secchi to cm
  mutate(Source="DJFMP",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  distinct()%>%
  group_by(Date, Station, Source)%>%
  summarise(Temperature=mean(Temperature), Secchi=mean(Secchi), Datetime=min(Datetime, na.rm=T)+(max(Datetime, na.rm=T)-min(Datetime, na.rm=T))/2)%>%
  ungroup()%>%
  select(Source, Station, Date, Datetime, Secchi, Temperature)

#Baystudy

tidecodes_baystudy <- read_csv(file.path("data-raw", "data", "Baystudy", "TideCodes_LookUp.csv"),
                               col_types=cols_only(Tide="i", Description="c"))


boatstation_baystudy <- read_csv(file.path("data-raw", "data", "Baystudy", "BoatStation.csv"),
                                 col_types = cols_only(Year="i", Survey="i", Station="c",
                                                       Date="c", Depth="d", Secchi="d", Tide="i"))%>%
  mutate(Date=parse_date_time(Date, orders="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide)%>%
  rename(Tidestation=Description)%>%
  left_join(read_csv(file.path("data-raw", "data", "Baystudy", "SalinTemp.csv"),
                     col_types = cols_only(Year="i", Survey="i", Station="c",
                                           ECSurf="d", TempSurf="d", TempBott="d")),
            by=c("Year", "Survey", "Station"))

boattow_baystudy<-read_csv(file.path("data-raw", "data", "Baystudy", "BoatTow.csv"),
                           col_types = cols_only(Year="i", Survey="i", Station="c", Time="c", Tide="i"))%>%
  mutate(Time=parse_date_time(Time, orders="%m/%d/%Y %H:%M:%S"))%>%
  group_by(Year, Survey, Station)%>%
  filter(Time==min(Time))%>% # Just keep records at the time of the first tow
  ungroup()%>%
  left_join(tidecodes_baystudy, by="Tide")%>%
  select(-Tide)%>%
  rename(Tidetow=Description)

wq_baystudy <- left_join(boattow_baystudy, boatstation_baystudy, by=c("Year", "Survey", "Station"))%>%
  mutate(Tide=if_else(is.na(Tidetow), Tidestation, Tidetow),
         Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="Baystudy")%>%
  select(Source, Station, Date, Datetime, Depth, Tide, Secchi, Temperature=TempSurf, Temperature_bottom=TempBott, Conductivity=ECSurf)

#USBR

wq_usbr <- read_csv(file.path("data-raw", "data", "USBR", "YSILongTermSites_AllDepths.csv"),
                    col_types=cols_only(Station="c", DateTime.PT="c", Depth.feet="d",
                                        Temp.C="d", SpCond.uS="d", Chl.ug.L="d", Date="c"))%>%
  rename(Datetime=DateTime.PT, Sample_depth=Depth.feet, Temperature=Temp.C, Conductivity=SpCond.uS, Chlorophyll=Chl.ug.L)%>%
  mutate(Datetime=parse_date_time(Datetime, orders="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
         Date=parse_date_time(Date, orders="%Y-%m-%d", tz="America/Los_Angeles"))%>%
  group_by(Station, Date)%>%
  mutate(Depth_bin=case_when(
    Sample_depth==min(Sample_depth) & Sample_depth<3 ~ "surface",
    Sample_depth==max(Sample_depth) & Sample_depth>3 ~ "bottom",
    TRUE ~ "Middle"
  ),
  Datetime=min(Datetime))%>%
  ungroup()%>%
  filter(Depth_bin%in%c("surface", "bottom"))%>%
  pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Conductivity, Chlorophyll, Temperature),
              values_fn=list(Sample_depth=mean, Conductivity=mean, Chlorophyll=mean, Temperature=mean))%>% # There are 2 duplicated rows this should take care of
  left_join(read_csv(file.path("data-raw", "data", "USBR", "USBRSiteLocations.csv"),
                     col_types=cols_only(Station="c", `Depth (m)`="d"))%>%
              mutate(Station=str_remove(Station, "NL "))%>%
              mutate(Station=recode(Station, PS="Pro"))%>%
              rename(Depth="Depth (m)"),
            by="Station")%>%
  mutate(Source="USBR",
         Sample_depth_surface = Sample_depth_surface*0.3048,
         Sample_depth_bottom = Sample_depth_bottom*0.3048)%>% # Convert to meters
  select(Source, Station, Date, Datetime, Depth, Sample_depth_surface, Sample_depth_bottom, Chlorophyll=Chlorophyll_surface,
         Temperature=Temperature_surface, Temperature_bottom, Conductivity=Conductivity_surface)

#USGS

USGSfiles <- list.files(path = file.path("data-raw", "data", "USGS"), full.names = T, pattern="SanFranciscoBayWaterQualityData.csv")

wq_usgs <- map_dfr(USGSfiles, ~read_csv(., col_types = cols_only(Date="c", Time="c", Station_Number="d", Depth="d",
                                                                 Calculated_Chlorophyll="d", Salinity="d", Temperature="d")))%>%
  rename(Station=Station_Number, Chlorophyll=Calculated_Chlorophyll)%>%
  mutate(Time=paste0(str_sub(Time, end=-3), ":", str_sub(Time, start=-2)))%>%
  bind_rows(read_csv(file.path("data-raw", "data", "USGS", "1969_2015USGS_SFBAY_22APR20.csv"),
                     col_types = cols_only(Date="c", Time="c", Station="d", `Depth`="d", `Calculated Chl-a`="d",
                                           Salinity="d", `Temperature`="d"))%>%
              rename(Chlorophyll=starts_with("Calculated")))%>%
  filter(!is.na(Date))%>%
  mutate(Date=parse_date_time(Date, orders=c("%m/%d/%Y", "%m/%d/%y"), tz="America/Los_Angeles"),
         Time=hm(Time),
         Station=as.character(Station))%>%
  mutate(Datetime=parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %H:%M", tz="America/Los_Angeles"),
         Source="USGS")%>%
  select(-Time)%>%
  rename(Sample_depth=Depth)%>%
  group_by(Station, Date)%>%
  mutate(Depth_bin=case_when(
    Sample_depth==min(Sample_depth) & Sample_depth<2~ "surface",
    Sample_depth==max(Sample_depth) & Sample_depth>2~ "bottom",
    TRUE ~ "Middle"
  ),
  Datetime=min(Datetime)+(max(Datetime)-min(Datetime))/2)%>%
  ungroup()%>%
  filter(Depth_bin%in%c("surface", "bottom"))%>%
  pivot_wider(names_from=Depth_bin, values_from=c(Sample_depth, Salinity, Chlorophyll, Temperature),
              values_fn=list(Sample_depth=mean, Salinity=mean, Chlorophyll=mean, Temperature=mean))%>% # This will just average out multiple measurements at same depth
  select(Source, Station, Date, Datetime, Sample_depth_surface, Sample_depth_bottom, Chlorophyll=Chlorophyll_surface,
         Temperature=Temperature_surface, Temperature_bottom, Salinity=Salinity_surface)

usethis::use_data(wq_emp, wq_fmwt, wq_stn, wq_edsm, wq_skt, wq_20mm, wq_suisun, wq_djfmp, wq_baystudy, wq_usbr, wq_usgs, overwrite = TRUE)
