## code to prepare `abiotic` dataset goes here

library(readxl)
library(dplyr)
library(readr)

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

wq_fmwt<-read_excel("data-raw/data/FMWT 1967-2018 Catch Matrix_updated.xlsx", sheet="FlatFile", guess_max=30000)%>%
  dplyr::select(.data$Date, .data$Station, Conductivity=tidyselect::starts_with("Top EC"), Secchi=.data$`Secchi (m)`, .data$Microcystis, Temperature=tidyselect::starts_with("Top Temperature"))%>%
  dplyr::mutate(Source="FMWT",
                Secchi=.data$Secchi*100,
                Microcystis=dplyr::if_else(.data$Microcystis==6, 2, .data$Microcystis))

wq_stn<-read_excel("data-raw/data/STN Sample.xlsx", guess_max=10000)%>%
  dplyr::select(Date=.data$SampleDate, Station=.data$StationCode, .data$Secchi, Temperature=.data$`TemperatureTop`, Conductivity=.data$`ConductivityTop`, .data$Microcystis
  )%>%
  dplyr::mutate(Source="TNS")

wq_edsm <- read_csv("data-raw/data/EDSM_20mm.csv", guess_max=9000)%>%
  dplyr::select(.data$Date, Latitude=.data$StartLat, Longitude=.data$StartLong, Conductivity=.data$TopEC, Temperature=.data$TopTemp, Secchi=.data$Scchi)%>%
  dplyr::bind_rows(read_csv("data-raw/data/EDSM_KDTR.csv", guess_max=30000)%>%
                     dplyr::select(.data$Date, Latitude=.data$StartLat, Longitude=.data$StartLong, Conductivity=.data$EC, Temperature=.data$Temp, Secchi=.data$Scchi))%>%
  dplyr::mutate(Secchi=.data$Secchi*100)%>%
  dplyr::mutate(Station=paste(.data$Latitude, .data$Longitude),
                Source="EDSM",
                Date=lubridate::parse_date_time(.data$Date, "%m/%d/%Y"))%>%
  dplyr::select(-.data$Conductivity)%>% #Methods in EDI metadata say they do not know if their data were corrected for temperature so I will not use this data
  dplyr::distinct()

Fieldfiles <- list.files(path = "data-raw/data/Water quality", full.names = T, pattern="Field")

Labfiles <- list.files(path = "data-raw/data/Water quality", full.names = T, pattern="Lab")

wq_EMP<-sapply(Fieldfiles, function(x) read_excel(x, guess_max = 5e4))%>%
  bind_rows()%>%
  dplyr::select(Date=.data$SampleDate, Station=.data$StationCode, Parameter=.data$AnalyteName, Value=.data$Result, Notes=.data$TextResult, .data$Matrix)%>%
  dplyr::filter(.data$Parameter%in%c("Temperature", "Secchi Depth", "Conductance (EC)") & .data$Matrix=="Water")%>%
  dplyr::group_by(.data$Date, .data$Station, .data$Parameter, .data$Notes)%>%
  dplyr::summarise(Value=mean(.data$Value, na.rm=T))%>%
  dplyr::ungroup()%>%
  bind_rows(sapply(Labfiles, function(x) read_excel(x, guess_max = 5e4))%>%
              bind_rows()%>%
              dplyr::select(Station=.data$StationCode, Date=.data$SampleDate, Parameter=.data$ConstituentName, Value=.data$Result, Notes=.data$LabAnalysisRemarks)%>%
              dplyr::filter(.data$Parameter=="Chlorophyll a")%>%
              dplyr::group_by(.data$Date, .data$Station, .data$Parameter, .data$Notes)%>%
              dplyr::summarise(Value=mean(.data$Value, na.rm=T))%>%
              dplyr::ungroup())%>%
  pivot_wider(names_from = .data$Parameter, values_from = .data$Value)%>%
  dplyr::rename(Chlorophyll=.data$`Chlorophyll a`, Secchi=.data$`Secchi Depth`, Conductivity=.data$`Conductance (EC)`)%>%
  dplyr::bind_rows(read_excel("data-raw/data/EMP WQ Combined_2000-2018.xlsx", na=c("N/A", "<R.L.", "Too dark"), col_types = c(rep("text", 3), "date", rep("text", 37)))%>%
                     dplyr::select(Station=.data$`Station Name`, .data$Date, Chlorophyll=tidyselect::starts_with("Chlorophyll"), Latitude=.data$`North Latitude Degrees (d.dd)`, Longitude=.data$`West Longitude Degrees (d.dd)`, Microcystis=.data$`Microcystis aeruginosa`, Secchi=.data$`Secchi Depth Centimeters`, Temperature=tidyselect::starts_with("Water Temperature"), Conductivity=tidyselect::starts_with("Specific Conductance"))%>%
                     dplyr::mutate(Chlorophyll=readr::parse_double(ifelse(.data$Chlorophyll%in%c("<0.05", "<0.5"), 0, .data$Chlorophyll)),
                                   Latitude=readr::parse_double(.data$Latitude),
                                   Longitude=readr::parse_double(.data$Longitude),
                                   Microcystis=readr::parse_double(.data$Microcystis),
                                   Secchi=readr::parse_double(.data$Secchi),
                                   Temperature=readr::parse_double(.data$Temperature),
                                   Conductivity=readr::parse_double(.data$Conductivity),
                                   Station=ifelse(.data$Station%in%c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"), paste(.data$Station, .data$Date), .data$Station))%>%
                     dplyr::mutate(Microcystis=round(.data$Microcystis))%>% #EMP has some 2.5 and 3.5 values
                     dplyr::select(-.data$Latitude, -.data$Longitude))%>%
  dplyr::mutate(Source="EMP")

usethis::use_data(wq_EMP, wq_fmwt, wq_stn, wq_edsm, overwrite = TRUE)
