## code to prepare `smelt` dataset goes here

library(readxl)
library(readr)
library(dplyr)
library(lubridate)

smelt_iep <- read_excel(file.path("data-raw", "data", "FMWT", "FMWT DS index.xlsx"))%>%
  select(Year, Index=Total)%>%
  mutate(Source="FMWT")%>%
  bind_rows(
    read_excel(file.path("data-raw", "data", "STN", "STN DS index.xlsx"))%>%
      mutate(Source="STN"),
    read_excel(file.path("data-raw", "data", "SKT", "SKT DS index.xlsx"))%>%
      mutate(Source="SKT"),
    read_excel(file.path("data-raw", "data", "20mm", "20mm DS index.xlsx"))%>%
      mutate(Source="20mm"))

smelt_edsm <- read_csv(file.path("data-raw", "data", "EDSM", "edsm_abund_estimates_2020-09-16.csv"))%>%
  mutate(Stratum=recode(Stratum, "Cache Slough LI"="Cache Slough/Liberty Island", "Sac DW Ship Channel"="Sac Deep Water Shipping Channel",
                        "Lower Sacramento"="Lower Sacramento River", "Lower San Joaquin"="Lower Joaquin River"),
         Date=WeekStartDate+ceiling((WeekEndDate-WeekStartDate)/2))%>%
  select(Region=Stratum, Date, Abundance=nHat, Variance=nVar)%>%
  mutate(MonthYear=floor_date(Date, unit = "month"))

usethis::use_data(smelt_iep, smelt_edsm, overwrite = TRUE)
