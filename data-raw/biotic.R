## code to prepare biotic datasets go here
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

Download <- FALSE

if (Download){
  #EMP Zoop Pump
  download.file("ftp://ftp.wildlife.ca.gov/IEP_Zooplankton/1972-2018Pump Matrix.xlsx",
                file.path("data-raw", "data", "EMP", "1972-2018Pump Matrix.xlsx"), mode="wb")
  #EMP Phytoplankton
  download.file("https://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/Phytoplankton_Algal_Type_Data_1975_-2016.csv",
                file.path("data-raw", "data", "EMP", "Phytoplankton_Algal_Type_Data_1975_-2016.csv"), mode="wb")
}

bivalves<-read_excel(file.path("data-raw", "data", "EMP", "1975-19 CPUE only, 2020Sept10.xlsx"),
                     sheet = "75-19 CPUE m2")%>%
  select(Date=SampleDate, Station=StationCode, `Potamocorbula amurensis`, `Corbicula fluminea`)%>%
  pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")%>%
  mutate(Year=year(Date),
         MonthYear=floor_date(Date, unit = "month"),
         Source="EMP")

zoop_mysid<-read_excel(file.path("data-raw", "data", "EMP", "EMPMysidBPUEMatrixAug2019.xlsx"),
                       sheet="MysidBPUEMatrix1972-2018",
                       col_types = c(rep("numeric", 4), "date", "text", "text", "numeric", "numeric", "text", "text", rep("numeric", 16)))%>%
  select(Date, Station, `Acanthomysis aspera`:`Unidentified mysid`)%>%
  mutate(Mysida=rowSums(select(., -Date, -Station), na.rm=T))%>%
  pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "BPUE")%>%
  mutate(BPUE=BPUE*1000,
         Taxa="Mysida")%>% # Convert to ug
  dplyr::mutate(Year=lubridate::year(.data$Date),
                MonthYear=lubridate::floor_date(.data$Date, unit = "month"),
                Source="EMP")

zoop_mass_conversions<-read_csv(file.path("data-raw", "data", "EMP", "zoop_individual_mass.csv"), col_types = "cd")%>%
  left_join(zooper::crosswalk%>%
              select(EMP_Meso, Taxname, Lifestage),
            by=c("taxon" = "EMP_Meso"))%>%
  mutate(Taxlifestage=paste(Taxname, Lifestage))%>%
  filter(!is.na(Taxname) & !is.na(Lifestage))%>%
  select(-taxon, -Taxname, -Lifestage)%>%
  rename(Mass=mass_indiv_ug)

phyto<-read_csv(file.path("data-raw", "data", "EMP", "Phytoplankton_Algal_Type_Data_1975_-2016.csv"),
                col_types = "ccddddddddddddddddddd")%>%
  rename(Date=SampleDate, Station=StationCode)%>%
  mutate(Date=parse_date_time(Date, "mdy"))%>%
  bind_rows(read_excel(file.path("data-raw", "data", "EMP", "Phytoplankton 2017.xlsx"))%>%
              rename(Station=`Station Code`, Cryptophytes=Cryptomonads))%>%
  pivot_longer(c(-Date, -Station), names_to = "Taxa", values_to = "CPUE")%>%
  mutate(Year=year(Date),
         MonthYear=floor_date(Date, unit = "month"),
         Source="EMP")


usethis::use_data(bivalves, zoop_mysid, zoop_mass_conversions, phyto, overwrite = TRUE)
