## code to prepare `deltaregions` dataset goes here
library(sf)

deltaregions<-read_sf("data-raw/Delta regions")


usethis::use_data(deltaregions, overwrite = TRUE)
