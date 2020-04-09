## code to prepare `abiotic` dataset goes here

library(readxl)
library(dplyr)
library(readr)
library(lubridate)
library(tidyselect)
library(tidyr)
library(stringr)

Download <- FALSE

#FMWT
if(Download){
  #FMWT
  temp <- tempfile()
  download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/FMWT%20Data/FMWT%201967-2019%20Catch%20Matrix_updated.zip", temp)
  unzip (temp, exdir="data-raw")
  unlink(temp)
  #TNS
  download.file("ftp://ftp.dfg.ca.gov/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/Townet_Data_1959-2018.xlsx", "data-raw/data/Townet_Data_1959-2018.xlsx", mode="wb")
  #EDSM 20mm
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.1&entityid=7c76313e27c4ef4685e7fe016c1e4608", "data-raw/data/EDSM_20mm.csv", mode="wb")
  #EDSM KDTR
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.415.1&entityid=93c01636cfc51919b8f363c7bfa829ca", "data-raw/data/EDSM_KDTR.csv", mode="wb")
  #DJFMP downloaded when file is loaded since file sizes are so large

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
  mutate(Tide=recode(Tide, `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048) # Convert to meters
tz(wq_fmwt$Date)<-"America/Los_Angeles"


wq_tns<-read_excel("data-raw/data/STN Sample.xlsx", guess_max=10000)%>%
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
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048) #Convert feet to meters
tz(wq_tns$Date)<-"America/Los_Angeles"

#Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
wq_edsm <- read_csv("data-raw/data/EDSM_20mm.csv",
                    col_types=cols_only(Date="c", StartLat="d", StartLong="d",
                                        TopTemp="d", Scchi="d", Time="c",
                                        Tide="c", Depth="d", SampleComments="c"))%>%
  rename(Latitude=StartLat, Longitude=StartLong, Temperature=TopTemp, Secchi=Scchi, Notes = SampleComments)%>%
  bind_rows(read_csv("data-raw/data/EDSM_KDTR.csv",
                     col_types=cols_only(Date="c", StartLat="d", StartLong="d",
                                         Temp="d", Scchi="d", Time="c",
                                         Tide="c", StartDepth="d", Comments="c"))%>%
              rename(Latitude=StartLat, Longitude=StartLong, Temperature=Temp, Secchi=Scchi, Depth=StartDepth, Notes=Comments))%>%
  mutate(Secchi=Secchi*100, # convert Secchi to cm
         Tide=recode(Tide, HS="High Slack", LS = "Low Slack"),
         Tide=na_if(Tide, "n/p"))%>% #Standardize tide codes
  mutate(Station=paste(Latitude, Longitude),
         Source="EDSM",
         Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, c("%I:%M:%S %Op", "%H:%M:%S"), tz="America/Los_Angeles"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  distinct()%>%
  mutate(Depth = Depth*0.3048) # Convert feet to meters

Fieldfiles <- list.files(path = "data-raw/data/EMP water quality", full.names = T, pattern="Field")

emp_field<-sapply(Fieldfiles, function(x) read_excel(x, guess_max = 5e4))%>%
  bind_rows()%>%
  select(Date=SampleDate, Station=StationCode, Parameter=AnalyteName, Value=Result, Notes=TextResult, Matrix, Time=DateResult)%>%
  filter(Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)") & Matrix=="Water")%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  group_by(Date, Station, Parameter, Notes, Datetime)%>%
  summarise(Value=mean(Value, na.rm=T))%>%
  ungroup()
tz(emp_field$Date)<-"America/Los_Angeles"
tz(emp_field$Datetime)<-"America/Los_Angeles"

Labfiles <- list.files(path = "data-raw/data/EMP water quality", full.names = T, pattern="Lab")

emp_lab <- sapply(Labfiles, function(x) read_excel(x, guess_max = 5e4))%>%
  bind_rows()%>%
  select(Station=StationCode, Date=SampleDate, Parameter=ConstituentName, Value=Result, Notes=LabAnalysisRemarks)%>%
  filter(Parameter=="Chlorophyll a")%>%
  group_by(Date, Station, Parameter, Notes)%>%
  summarise(Value=mean(Value, na.rm=T))%>%
  ungroup()
tz(emp_lab$Date)<-"America/Los_Angeles"

emp_2000<-read_excel("data-raw/data/EMP water quality/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 2), rep("date", 2), rep("text", 37)))%>%
  select(Station=`Station Name`, Datetime = `Sample Date`, Date, Chlorophyll=starts_with("Chlorophyll"), Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`, Microcystis=`Microcystis aeruginosa`, Secchi=`Secchi Depth Centimeters`, Temperature=starts_with("Water Temperature"), Conductivity=starts_with("Specific Conductance"))%>%
  mutate(Chlorophyll=parse_double(ifelse(Chlorophyll%in%c("<0.05", "<0.5"), 0, Chlorophyll)),
         Latitude=parse_double(Latitude),
         Longitude=parse_double(Longitude),
         Microcystis=parse_double(Microcystis),
         Secchi=parse_double(Secchi),
         Temperature=parse_double(Temperature),
         Conductivity=parse_double(Conductivity),
         Station=ifelse(Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(Station, Date), Station))%>%
  mutate(Microcystis=round(Microcystis))%>% #EMP has some 2.5 and 3.5 values
  select(-Latitude, -Longitude)
tz(emp_2000$Date)<-"America/Los_Angeles"
tz(emp_2000$Datetime)<-"America/Los_Angeles"

wq_emp<- bind_rows(emp_field, emp_lab)%>%
  pivot_wider(names_from = Parameter, values_from = Value)%>%
  rename(Chlorophyll=`Chlorophyll a`, Secchi=`Secchi Depth`, Conductivity=`Conductance (EC)`)%>%
  bind_rows(emp_2000)%>%
  mutate(Source="EMP",
         Tide = "High Slack")

wq_skt <- read_csv("data-raw/data/SKT_tblSample.csv",
                   col_types = cols_only(SampleDate="c", StationCode="c", SampleTimeStart="c", Secchi="d", ConductivityTop="d",
                                         WaterTemperature="d", DepthBottom="d", TideCode="i",
                                         SampleComments="c", Latitude="c", Longitude="c"))%>%
  select(Date=SampleDate, Station=StationCode, Time=SampleTimeStart, Secchi,
         Conductivity=ConductivityTop, Temperature=WaterTemperature, Depth=DepthBottom, Tide=TideCode,
         Notes=SampleComments, Latitude, Longitude)%>%
  mutate(Latitude=na_if(Latitude, "0"),
         Longitude=na_if(Longitude, "0"),
         Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Source="SKT",
         Time = parse_date_time(Time, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))%>%
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
  select(-Time, -LatD, -LatM, -LatS, -LonD, -LonM, -LonS)%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Depth = Depth*0.3048) # Convert feet to meters


wq_20mm <- read_csv("data-raw/data/20mm_Station.csv",
                    col_types = cols_only(StationID="c", SurveyID="c", Station="c",
                                          LatDeg="d", LatMin="d", LatSec="d", LonDeg="d",
                                          LonMin="d", LonSec="d", Temp="d", TopEC="d",
                                          Secchi="d", Comments="c"))%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LonDeg+LonMin/60+LonSec/3600)*-1)%>%
  left_join(read_csv("data-raw/data/20mm_Survey.csv",
                     col_types = cols_only(SurveyID="c", SampleDate = "c"))%>%
              rename(Date=SampleDate)%>%
              mutate(Date = parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles")),
            by="SurveyID")%>%
  left_join(read_csv("data-raw/data/20mm_Tow.csv",
                     col_types = cols_only(StationID="c", TowTime="c", Tide="d", BottomDepth="d", TowNum="d"))%>%
              rename(Time=TowTime)%>%
              mutate(Time = parse_date_time(Time, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))%>%
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
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(as.character(Tide), `4`="Flood", `3`="Low Slack", `2`="Ebb", `1`="High Slack"),
         Source = "20mm",
         Depth = Depth*0.3048) # Convert feet to meters

#Removing salinity because data do not correspond well with conductivity
wq_suisun<-read_csv("data-raw/data/Suisun_Sample.csv",
                    col_types = cols_only(SampleRowID="c", StationCode="c", SampleDate="c", SampleTime="c",
                                          QADone="l", WaterTemperature="d",
                                          Secchi="d", SpecificConductance="d", TideCode="c"))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime,
         Temperature=WaterTemperature, Conductivity=SpecificConductance, Tide=TideCode)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime=parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  mutate(Tide=recode(Tide, flood="Flood", ebb="Ebb", low="Low Slack", high="High Slack", outgoing="Ebb", incoming="Flood"),
         Source="Suisun")%>%
  left_join(read_csv("data-raw/data/Suisun_Depth.csv",
                     col_types=cols_only(SampleRowID="c", Depth="d"))%>%
              group_by(SampleRowID)%>%
              summarise(Depth=mean(Depth, na.rm=T))%>%
              ungroup(),
            by="SampleRowID")%>%
  select(-SampleRowID)

download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.3&entityid=71c16ead9b8ffa4da7a52da180f601f4", file.path(tempdir(), "DJFMP_1976-2001.csv"), mode="wb")
download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.3&entityid=93cb0e8ec9fa92adc7aba9499b3ea6d7", file.path(tempdir(), "DJFMP_2002-2018.csv"), mode="wb")

wq_djfmp <- read_csv(file.path(tempdir(), "DJFMP_1976-2001.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c", WaterTemperature="d", Secchi="d"))%>%
  bind_rows(read_csv(file.path(tempdir(), "DJFMP_2002-2018.csv"),
                     col_types = cols_only(StationCode = "c", SampleDate="c", SampleTime="c", WaterTemperature="d", Secchi="d")))%>%
  rename(Station=StationCode, Date=SampleDate, Time=SampleTime, Temperature=WaterTemperature)%>%
  mutate(Secchi=Secchi*100)%>% # convert Secchi to cm
  mutate(Source="DJFMP",
         Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Time=parse_date_time(Time, "%H:%M:%S", tz="America/Los_Angeles"))%>%
  mutate(Datetime = parse_date_time(if_else(is.na(Time), NA_character_, paste0(Date, " ", hour(Time), ":", minute(Time))), "%Y-%m-%d %%H:%M", tz="America/Los_Angeles"))%>%
  select(-Time)%>%
  distinct()


usethis::use_data(wq_emp, wq_fmwt, wq_tns, wq_edsm, wq_skt, wq_20mm, wq_suisun, overwrite = TRUE)
