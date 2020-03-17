## code to prepare `abiotic` dataset goes here

library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(tidyselect)

Download <- FALSE

#FMWT
if(Download){
  #FMWT
  temp <- tempfile()
  download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2019%20Catch%20Matrix_updated.zip", temp)
  unzip (temp, exdir="data-raw")
  unlink(temp)
  #STN
  download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/Townet_Data_1959-2018.xlsx", "data-raw/data/Townet_Data_1959-2018.xlsx", mode="wb")
  #EDSM 20mm
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.1&entityid=7c76313e27c4ef4685e7fe016c1e4608", "data-raw/data/EDSM_20mm.csv", mode="wb")
  #EDSM KDTR
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.1&entityid=93c01636cfc51919b8f363c7bfa829ca", "data-raw/data/EDSM_KDTR.csv", mode="wb")
}

wq_fmwt<-read_excel("data-raw/data/FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet="FlatFile",
                    col_types = c("numeric", "date", "numeric", "text", "date",
                                  rep("numeric", 8), "text", rep("numeric", 114)))%>%
  select(Date, Time=`Start Time`, Station, Conductivity=starts_with("Top EC"), Secchi=`Secchi (m)`, Microcystis, Temperature=starts_with("Top Temperature"), Depth=starts_with("Depth"), Tide)%>%
  mutate(Source="FMWT",
                Secchi=Secchi*100,
                Microcystis=if_else(Microcystis==6, 2, Microcystis),
                Time=if_else(hour(Time)==0 & minute(Time)==0 & second(Time)==0, parse_date_time(NA), Time))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(Tide, `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"))


wq_stn<-read_excel("data-raw/data/STN Sample.xlsx", guess_max=10000)%>%
  select(Date=SampleDate, Station=StationCode, Secchi,
                Temperature=`TemperatureTop`, Conductivity=`ConductivityTop`,
                Microcystis, Tide=TideCode, Depth=DepthBottom, Notes=SampleComments,
                SampleRowID)%>%
  mutate(Source="TNS")%>%
  left_join(read_excel("data-raw/data/STN_TowEffort.xlsx",
                       col_types=c(rep("numeric", 3), "date", "date",
                                   "text", rep("numeric", 3), "text", "text"))%>%
              select(SampleRowID, Time=TimeStart)%>%
              drop_na()%>%
              group_by(SampleRowID)%>%
              summarise(Time=min(Time))%>%
              ungroup(),
            by="SampleRowID")%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time, -SampleRowID)%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"))

wq_edsm <- read_csv("data-raw/data/EDSM_20mm.csv", guess_max=9000)%>%
  select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=TopEC, Temperature=TopTemp, Secchi=Scchi, Time, Tide, Depth, Notes = SampleComments)%>%
  bind_rows(read_csv("data-raw/data/EDSM_KDTR.csv", guess_max=30000)%>%
                     select(Date, Latitude=StartLat, Longitude=StartLong, Conductivity=EC, Temperature=Temp, Secchi=Scchi, Time, Tide, Depth=StartDepth, Notes=Comments))%>%
  mutate(Secchi=Secchi*100)%>%
  mutate(Station=paste(Latitude, Longitude),
                Source="EDSM",
                Date=lubridate::parse_date_time(Date, "%m/%d/%Y"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Conductivity, -Time)%>% #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
  distinct()

Fieldfiles <- list.files(path = "data-raw/data/EMP water quality", full.names = T, pattern="Field")

Labfiles <- list.files(path = "data-raw/data/EMP water quality", full.names = T, pattern="Lab")

wq_emp<-sapply(Fieldfiles, function(x) read_excel(x, guess_max = 5e4))%>%
  bind_rows()%>%
  select(Date=SampleDate, Station=StationCode, Parameter=AnalyteName, Value=Result, Matrix)%>%
  filter(Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)") & Matrix=="Water")%>%
  group_by(Date, Station, Parameter)%>%
  summarise(Value=mean(Value, na.rm=T))%>%
  ungroup()%>%
  bind_rows(sapply(Labfiles, function(x) read_excel(x, guess_max = 5e4))%>%
              bind_rows()%>%
              select(Station=StationCode, Date=SampleDate, Parameter=ConstituentName, Value=Result)%>%
              filter(Parameter=="Chlorophyll a")%>%
              group_by(Date, Station, Parameter)%>%
              summarise(Value=mean(Value, na.rm=T))%>%
              ungroup())%>%
  pivot_wider(names_from = Parameter, values_from = Value)%>%
  rename(Chlorophyll=`Chlorophyll a`, Secchi=`Secchi Depth`, Conductivity=`Conductance (EC)`)%>%
  bind_rows(read_excel("data-raw/data/EMP water quality/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 3), "date", rep("text", 37)))%>%
                     select(Station=`Station Name`, Date, Chlorophyll=starts_with("Chlorophyll"), Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`, Microcystis=`Microcystis aeruginosa`, Secchi=`Secchi Depth Centimeters`, Temperature=starts_with("Water Temperature"), Conductivity=starts_with("Specific Conductance"))%>%
                     mutate(Chlorophyll=readr::parse_double(ifelse(Chlorophyll%in%c("<0.05", "<0.5"), 0, Chlorophyll)),
                                   Latitude=readr::parse_double(Latitude),
                                   Longitude=readr::parse_double(Longitude),
                                   Microcystis=readr::parse_double(Microcystis),
                                   Secchi=readr::parse_double(Secchi),
                                   Temperature=readr::parse_double(Temperature),
                                   Conductivity=readr::parse_double(Conductivity),
                                   Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
                     mutate(Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
                     select(-Latitude, -Longitude))%>%
  mutate(Source="EMP")

wq_skt <- read_csv("data-raw/data/SKT_tblSample.csv",
                   col_types = cols_only(SampleDate="D", StationCode="c", SampleTimeStart="T",
                                         SampleTimeEnd="T", Secchi="d", ConductivityTop="d",
                                         WaterTemperature="d", DepthBottom="d", TideCode="i",
                                         SampleComments="c", Latitude="c", Longitude="c"))%>%
  select(Date=SampleDate, Station=StationCode, TimeStart=SampleTimeStart, TimeEnd=SampleTimeEnd, Secchi,
         Conductivity=ConductivityTop, Temperature=WaterTemperature, Depth=DepthBottom, Tide=TideCode,
         Notes=SampleComments, Latitude, Longitude)%>%
  mutate(Latitude=na_if(Latitude, "0"),
         Longitude=na_if(Longitude, "0"),
         Source="SKT",
         Time = if_else(is.na(TimeEnd), TimeStart, (TimeEnd-TimeStart)/2+TimeStart))%>%
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
  mutate(Datetime = parse_date_time(paste0(Date, " ", hour(Time), ":", minute(Time)), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-TimeEnd, -TimeStart, -Time, -LatD, -LatM, -LatS, -LonD, -LonM, -LonS)

wq_20mm <- read_csv("data-raw/data/20mm_Station.csv",
                        col_types = cols_only(StationID="c", SurveyID="c", Station="c",
                                              LatDeg="d", LatMin="d", LatSec="d", LonDeg="d",
                                              LonMin="d", LonSec="d", Temp="d", TopEC="d",
                                              Secchi="d", Comments="c"))%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LonDeg+LonMin/60+LonSec/3600)*-1)%>%
  left_join(read_csv("data-raw/data/20mm_Survey.csv",
                     col_types = cols_only(SurveyID="c", SampleDate = "D")),
            by="SurveyID")%>%
  left_join(read_csv("data-raw/data/20mm_Tow.csv",
                     col_types = cols_only(StationID="c", TowTime="T", Tide="d", BottomDepth="d", TowNum="d"))%>%
              group_by(StationID)%>%
              mutate(Retain=if_else(TowTime==min(TowTime), TRUE, FALSE))%>%
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
         Notes=Comments, Latitude, Longitude, Date=SampleDate,
         Time=TowTime, Depth=BottomDepth, Tide)%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="High Slack", `2`="Ebb", `1`="Low Slack"),
         Source = "20mm")


usethis::use_data(wq_emp, wq_fmwt, wq_stn, wq_edsm, wq_skt, wq_20mm, overwrite = TRUE)
