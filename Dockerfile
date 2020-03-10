# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.6.2

# required
MAINTAINER Sam Bashevkin <sam.bashevkin@deltacouncil.ca.gov>

COPY . /deltareportr

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  && sudo apt-get install -y libgdal-dev \
  && sudo apt-get install -y libgeos-dev \
  && sudo apt-get install -y libproj-dev \
  # build this compendium package
  && R -e "devtools::install('/deltareportr', dep=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/deltareportr/analysis/paper/Delta Smelt conditions report.Rmd')"
