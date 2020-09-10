## code to prepare `dayflow` dataset goes here

library(readr)
library(dplyr)
library(lubridate)

Download <- FALSE

if(Download){
  #Dayflow 1997-2019
  download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2019.csv",
                file.path("data-raw", "data", "Dayflow", "Dayflow1997 2019.csv"), mode="wb")
  #Dayflow 1984-1996
  download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv",
                file.path("data-raw", "data", "Dayflow", "Dayflow1984 1996.csv"), mode="wb")
}

dayflow<-read_csv(file.path("data-raw", "data", "Dayflow", "Dayflow1997 2019.csv"), col_types = cols_only(Date="c", OUT="d", X2="d"))%>%
  mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz = "America/Los_Angeles"))%>%
  bind_rows(read_csv(file.path("data-raw", "data", "Dayflow", "Dayflow1984 1996.csv"), col_types = cols_only(Date="c", OUT="d"))%>%
              mutate(Date=parse_date_time(Date, "%m/%d/%Y", tz = "America/Los_Angeles")))

usethis::use_data(dayflow, overwrite=T)
