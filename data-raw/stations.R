## code to prepare `stations` dataset goes here
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(readxl)

FMWT<-read_excel("data-raw/data/FMWT Station.xlsx")%>%
  select(Station=StationCode, Lat, Long, Lat2=`WGS84 Lat`, Long2=`WGS84 Long`)%>%
  mutate(Lat2=parse_number(Lat2),
         Long2=parse_number(Long2))%>%
  separate(Lat, into=c("Lat_d", "Lat_m", "Lat_s"), sep="[ ]{1,}", convert=T)%>%
  separate(Long, into=c("Long_d", "Long_m", "Long_s"), sep="[ ]{1,}", convert=T)%>%
  mutate(Latitude=Lat_d+Lat_m/60+Lat_s/3600,
         Longitude=Long_d-Long_m/60-Long_s/3600,
         Source="FMWT",
         StationID=paste(Source, Station))%>%
  mutate(Latitude=if_else(is.na(Latitude), Lat2, Latitude),
         Longitude-if_else(is.na(Longitude), Long2, Longitude))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

STN<-read_excel("data-raw/data/STN Station.xlsx")%>%
  select(Station=StationCodeSTN, LatD, LatM, LatS, LonD, LonM, LonS)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="STN",
         StationID=paste(Source, Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

SKT<-read_csv("data-raw/data/lktblStationsSKT.csv")%>%
  select(Station, LatDeg, LatMin, LatSec, LongDec, LongMin, LongSec)%>%
  mutate(Latitude=LatDeg+LatMin/60+LatSec/3600,
         Longitude=(LongDec+LongMin/60+LongSec/3600)*-1,
         Source="SKT",
         StationID=paste(Source, Station),
         Station=as.character(Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

twentymm<-read_csv("data-raw/data/tbl20mmStations.csv")%>%
  select(Station, LatD, LatM, LatS, LonD, LonM, LonS)%>%
  mutate(Latitude=LatD+LatM/60+LatS/3600,
         Longitude=(LonD+LonM/60+LonS/3600)*-1,
         Source="20mm",
         StationID=paste(Source, Station),
         Station=as.character(Station))%>%
  select(Station, Latitude, Longitude, Source, StationID)%>%
  drop_na()

Zoopxl<-read_excel("data-raw/data/zoop_stations.xlsx")%>%
  rename(Source=Project)%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

WQ<-read_csv("data-raw/data/wq_stations.csv")%>%
  select(Station=site, Latitude=lat, Longitude=long)%>%
  mutate(Source="EMP",
         StationID=paste(Source, Station))%>%
  drop_na()

#EZ stations

EZ<-read_excel("data-raw/data/EMP water quality/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 3), "date", rep("text", 37)))%>%
  select(Station=`Station Name`, Date, Latitude=`North Latitude Degrees (d.dd)`, Longitude=`West Longitude Degrees (d.dd)`)%>%
  mutate(Latitude=parse_double(Latitude),
         Longitude=parse_double(Longitude))%>%
  mutate(Station=paste(Station, Date),
         Source="EMP")%>%
  select(Station, Latitude, Longitude, Source)%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

#EMP Bivalve stations

EMPBIV <-read_excel("data-raw/data/1975-18 CPUE bivalves only, 2019Sept9.xlsx",
                    sheet = "75-17 station locations", skip=1)%>%
  select(Station=Site_Code, Latitude, Longitude)%>%
  mutate(Source="EMP")%>%
  mutate(StationID=paste(Source, Station))%>%
  drop_na()

#Suisun Study

Suisun<-read_csv("data-raw/data/Suisun_StationsLookUp.csv",
         col_types=cols_only(StationCode="c", x_WGS84="d", y_WGS84="d"))%>%
  rename(Longitude=x_WGS84, Latitude=y_WGS84, Station=StationCode)%>%
  mutate(Source="Suisun",
         StationID=paste(Source, Station))


#Load delta regions shapefile (EDSM 2018-19 phase I strata)

stations<-bind_rows(
  Zoopxl,
  FMWT%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  STN%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  WQ%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  twentymm%>%
    filter(!(StationID%in%unique(Zoopxl$StationID))),
  EZ,
  SKT,
  EMPBIV,
  Suisun)%>%
  drop_na()

usethis::use_data(stations, overwrite = TRUE)
