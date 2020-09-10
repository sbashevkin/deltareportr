## code to prepare `stations` dataset goes here
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(readxl)
require(stringr)
require(lubridate)

Download <- FALSE

if(Download){
  #DJFMP
  download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.4&entityid=99a038d691f27cd306ff93fdcbc03b77", file.path("data-raw", "data", "DJFMP", "DJFMP_stations.csv"), mode="wb")
}

FMWT<-read_csv(file.path("data-raw", "data", "FMWT", "StationsLookUp.csv"),
               col_types = cols_only(StationCode="c", DD_Latitude="d", DD_Longitude="d"))%>%
  rename(Station=StationCode, Latitude=DD_Latitude, Longitude=DD_Longitude)%>%
  mutate(Source="FMWT",
         StationID=paste(Source, Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

STN<-read_csv(file.path("data-raw", "data", "STN", "luStation.csv"),
              col_types = cols_only(StationCodeSTN="c", LatD='d', LatM="d", LatS="d",
                                    LonD='d', LonM='d', LonS='d'))%>%
  rename(Station=StationCodeSTN)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="STN",
         StationID=paste(Source, Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

SKT<-read_csv(file.path("data-raw", "data", "SKT", "lktblStationsSKT.csv"),
              col_types=cols_only(Station="c", LatDeg="d", LatMin="d", LatSec="d",
                                  LongDec="d", LongMin="d", LongSec="d"))%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LongDec+LongMin/60+LongSec/3600)*-1,
         Source="SKT",
         StationID=paste(Source, Station),
         Station=as.character(Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

twentymm<-read_csv(file.path("data-raw", "data", "20mm", "20mmStations.csv"),
                   col_types=cols_only(Station="c", LatD="d", LatM="d", LatS="d",
                                       LonD="d", LonM="d", LonS="d"))%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="20mm",
         StationID=paste(Source, Station),
         Station=as.character(Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

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

EZ<-read_csv(file.path("data-raw", "data", "EMP", "Water quality", "SACSJ_delta_water_quality_2000_2018.csv"), na=c("N/A", "ND"),
             col_types = cols_only(Station="c", Date="c", NorthLat="d", WestLong="d"))%>%
  rename(Latitude=NorthLat, Longitude=WestLong)%>%
  mutate(Date=parse_date_time(Date, "%Y-%m-%d", tz="America/Los_Angeles"),
         Station=paste(Station, Date),
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

#Suisun Study

Suisun<-read_csv(file.path("data-raw", "data", "Suisun", "Suisun_StationsLookUp.csv"),
                 col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))%>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)%>%
  mutate(Source="Suisun",
         StationID=paste(Source, Station))

#DJFMP

DJFMP <- read_csv(file.path("data-raw", "data", "DJFMP", "DJFMP_stations.csv"),
                  col_types=cols_only(StationCode="c", Latitude_location="d", Longitude_location="d"))%>%
  rename(Station=StationCode, Latitude=Latitude_location, Longitude=Longitude_location)%>%
  mutate(Source="DJFMP",
         StationID=paste(Source, Station))

#Baystudy

Baystudy <- read_excel(file.path("data-raw", "data", "Baystudy", "Bay Study_Station Coordinates for Distribution_04May2020.xlsx"))%>%
  separate(Latitude, into=c("Lat_Deg", "Lat_Min"), sep = "°", convert=T)%>%
  separate(Longitude, into=c("Lon_Deg", "Lon_Min"), sep = "°", convert=T)%>%
  mutate(Latitude=Lat_Deg+Lat_Min/60,
         Longitude=Lon_Deg-Lon_Min/60)%>%
  select(Station, Latitude, Longitude)%>%
  filter(Station!="211E")%>%
  mutate(Station=recode(Station, `211W`="211"),
         Source="Baystudy",
         StationID=paste(Source, Station))

# USBR

USBR <- read_csv(file.path("data-raw", "data", "USBR", "USBRSiteLocations.csv"),
                 col_types=cols_only(Station="c", Lat="d", Long="d"))%>%
  rename(Latitude=Lat, Longitude=Long)%>%
  mutate(Station=str_remove(Station, "NL "),
         Station=recode(Station, PS="Pro"),
         Source="USBR",
         StationID=paste(Source, Station))

# USGS

USGS <- read_excel(file.path("data-raw", "data", "USGS", "USGSSFBayStations.xlsx"))%>%
  select(Station, Latitude="Latitude degree", Longitude="Longitude degree")%>%
  mutate(Source="USGS",
         Station=as.character(Station),
         StationID=paste(Source, Station))%>%
  select(Source, Station, StationID, Latitude, Longitude)

stations<-bind_rows(
  Zoopxl,
  FMWT,
  STN%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  WQ%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  twentymm%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  EZ,
  SKT,
  EMPBIV,
  Suisun,
  DJFMP,
  Baystudy,
  USBR,
  USGS)%>%
  drop_na()

usethis::use_data(stations, overwrite = TRUE)
