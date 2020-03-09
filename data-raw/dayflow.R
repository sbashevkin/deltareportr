## code to prepare `dayflow` dataset goes here

library(readr)
library(dplyr)
library(lubridate)

Download <- FALSE

if(Download){
  #Dayflow 1997-2018
  download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2018.csv",
                "Data/Dayflow1997 2018.csv", mode="wb")
  #Dayflow 1984-1996
  download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/wy1984-1996.csv",
                "Data/Dayflow1984 1996.csv", mode="wb")
}

dayflow<-read_csv("data-raw/data/Dayflow1997 2018.csv", col_types = "ddcdddddddddddddddddddddddddd")%>%
  mutate(Date=parse_date_time(Date, "%d-%b-%y", tz = "America/Los_Angeles"))%>%
  select(Date, OUT, X2)%>%
  bind_rows(read_csv("data-raw/data/Dayflow1984 1996.csv", col_types = "cddcddddddddddddddddddddddddd")%>%
              mutate(DATE=parse_date_time(DATE, "%d-%b-%y", tz = "America/Los_Angeles"))%>%
              select(Date=DATE, OUT))

usethis::use_data(dayflow)
