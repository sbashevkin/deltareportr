## code to prepare `deltaregions` dataset goes here
library(sf)

deltaregions<-read_sf("data-raw/data/Delta regions")%>%
  select(-SQM)


usethis::use_data(deltaregions, overwrite = TRUE)
