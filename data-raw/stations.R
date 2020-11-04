## code to prepare `stations` dataset goes here
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(readxl)
require(stringr)
require(lubridate)

Zoopxl<-read_excel(file.path("data-raw", "data", "zoop_stations.xlsx"))%>%
  rename(Source=Project)%>%
  filter(Source!="FMWT")%>% # Remove FMWT stations since we have good ones now
  mutate(Source=recode(Source, TNS="STN"),
         StationID=paste(Source, Station))%>%
  drop_na()

WQ<-read_csv(file.path("data-raw", "data", "EMP", "wq_stations.csv"),
             col_types=cols_only(site="c", lat="d", long="d"))%>%
  select(Station=site, Latitude=lat, Longitude=long)%>%
  mutate(Source="EMP",
         StationID=paste(Source, Station))%>%
  drop_na()

#EZ stations

EZ<-read_csv(file.path("data-raw", "data", "EMP", "SACSJ_delta_water_quality_1975_2019.csv"), na=c("NA", "ND"),
             col_types = cols_only(Station="c", Date="c", NorthLat="d", WestLong="d"))%>%
  rename(Latitude=NorthLat, Longitude=WestLong)%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz="America/Los_Angeles"),
         Station=paste(Station, Date),
         Source="EMP")%>%
  select(Station, Latitude, Longitude, Source)%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

ZoopEZ<-zooper::stationsEMPEZ%>%
  mutate(Station=paste(Station, Date),
         Source="EMP")%>%
  select(Station, Latitude, Longitude, Source)%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

#EMP Bivalve stations

EMPBIV <-read_excel(file.path("data-raw", "data", "EMP", "1975-18 CPUE bivalves only, 2019Sept9.xlsx"),
                    sheet = "75-17 station locations", skip=1)%>%
  select(Station=Site_Code, Latitude, Longitude)%>%
  mutate(Source="EMP")%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

stations<-bind_rows(
  Zoopxl,
  WQ%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  EZ,
  EMPBIV,
  ZoopEZ)%>%
  drop_na()

usethis::use_data(stations, overwrite = TRUE)
