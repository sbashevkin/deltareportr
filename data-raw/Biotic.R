## code to prepare biotic datasets go here
library(readxl)
library(dplyr)
library(tidyr)

bivalves<-read_excel("data-raw/1975-18 CPUE bivalves only, 2019Sept9.xlsx",
           sheet = "75-18 CPUE per m2", skip=1)

zoop_cb<-read_excel("data-raw/1972-2018CBMatrix.xlsx",
                    sheet = "CB CPUE Matrix 1972-2018",
                    col_types = c("numeric","numeric", "numeric", "numeric", "date",
                                  "text", "text", "text", "numeric", "text", "text",
                                  "text", rep("numeric", 62)))

zoop_pump<-read_excel("data-raw/1972-2018Pump Matrix.xlsx",
                     sheet = " Pump CPUE Matrix 1972-2018",
                     col_types = c("numeric","numeric", "numeric", "numeric", "date",
                                   "text", "text", "text", "numeric",
                                   "text", rep("numeric", 36)))

zoop_mysid<-read_excel("data-raw/EMPMysidBPUEMatrixAug2019.xlsx",
                      sheet="MysidBPUEMatrix1972-2018",
                      col_types = c(rep("numeric", 4), "date", "text", "text", "numeric", "numeric", "text", "text", rep("numeric", 16)))

zoop_mass_conversions<-read_csv("data-raw/zoop_individual_mass.csv", col_types = "cd")

phyto<-read_csv("data-raw/Phytoplankton_Algal_Type_Data_1975_-2016.csv",
                col_types = "ccddddddddddddddddddd")


usethis::use_data(bivalves, zoop_cb, zoop_pump, zoop_mysid, zoop_mass_conversions, phyto, overwrite = TRUE)
