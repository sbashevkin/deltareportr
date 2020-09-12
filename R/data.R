#' Bivalve dataset
#'
#' Bivalve abundance dataset from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @format a tibble with 8,892 rows and 7 variables
#' \describe{
#'   \item{Date}{Sample collection date.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Bivalve species name.}
#'   \item{CPUE}{Catch per unit effort in number of clams (\ifelse{html}{\out{m<sup>-2</sup>}}{\eqn{m^{-2}}}).}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://emp.baydeltalive.com/projects/11280}{here}.
#' @seealso \code{\link{DeltaBivalver}}, \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"bivalves"

#' Dayflow dataset
#'
#' Outflow and X2 from the California Department of Water Resources Dayflow model.
#'
#' @format a tibble with 13,149 rows and 7 variables
#' \describe{
#'   \item{Date}{Date.}
#'   \item{Out}{Delta outflow (\ifelse{html}{\out{ft<sup>3</sup>s<sup>-1</sup>}}{\eqn{ft^{3}} \eqn{s^{-1}}}).}
#'   \item{X2}{X2 (km).}
#'   }
#' @details More metadata and information on methods are available \href{https://data.cnra.ca.gov/dataset/dayflow}{here}.
#' @seealso \code{\link{DeltaBivalver}}, \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"dayflow"

#' Phytoplankton dataset
#'
#' Phytoplankton abundance dataset from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @format a tibble with 8,652 rows and 7 variables
#' \describe{
#'   \item{Date}{Sample collection date.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Phytoplankton taxa.}
#'   \item{CPUE}{Catch per unit effort in number of cells, colonies, or filaments (\ifelse{html}{\out{ml<sup>-1</sup>}}{\eqn{ml^{-1}}}).}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://emp.baydeltalive.com/projects/11282}{here}.
#' @seealso \code{\link{DeltaPhyter}}, \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"phyto"

#' Delta Smelt EDSM abundance estimates
#'
#' Estimated Delta Smelt abundance dataset from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @format a tibble with 961 rows and 7 variables
#' \describe{
#'   \item{Region}{Region of abundance estimate.}
#'   \item{Date}{Abundance estimate date. Abundances are estimated weekly, but these dates represent the midpoint of each week.}
#'   \item{Abundance}{Estimated Delta Smelt abundance.}
#'   \item{Variance}{Variance of the abundance estimate.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   }
#' @details More metadata and information on methods are available \href{https://www.fws.gov/lodi/juvenile_fish_monitoring_program/jfmp_index.htm}{here}.
#' @seealso \code{\link{DeltaSmelter}}
"smelt_edsm"

#' Delta Smelt IEP indices
#'
#' Delta Smelt abundance indices from the California Department of Fish and Wildlife surveys.
#'
#' @format a tibble with 157 rows and 3 variables
#' \describe{
#'   \item{Year}{Index year.}
#'   \item{Index}{Delta Smelt index number.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available here: \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{Fall Midwater Trawl}, \href{https://wildlife.ca.gov/Conservation/Delta/Spring-Kodiak-Trawl}{Spring Kodiak Trawl}, \href{https://wildlife.ca.gov/Conservation/Delta/20mm-Survey}{20mm Survey}, and \href{https://wildlife.ca.gov/Conservation/Delta/Townet-Survey}{Summer Townet}.
#' @seealso \code{\link{DeltaSmelter}}
"smelt_iep"

#' Station locations
#'
#' Locations of all sampling stations.
#'
#' @format a tibble with 1,361 rows and 5 variables
#' \describe{
#'   \item{Source}{Name of the source dataset..}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{StationID}{Combined source and station for a unique station ID.}
#'   }
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"stations"

#' 20mm water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife 20mm survey.
#'
#' @encoding UTF-8
#' @format a tibble with 9,712 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/20mm-Survey}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_20mm"

#' Bay Study water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Bay Study.
#'
#' @encoding UTF-8
#' @format a tibble with 20,195 rows and 10 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{http://www.dfg.ca.gov/delta/projects.asp?ProjectID=BAYSTUDY}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_baystudy"

#' EDSM water quality data
#'
#' Water quality data from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 21,565 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.415.1}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_edsm"

#' EMP water quality data
#'
#' Water quality data from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 16,293 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time sample was collected.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage (always High Slack).}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Notes or comments.}
#'   }
#'
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.458.2}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_emp"

#' DJFMP water quality data
#'
#' Water quality data from the United States Fish and Wildlife Service Delta Juvenile Fish Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 23,998 rows and 6 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature in °C.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.3}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_djfmp"

#' FMWT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Fall Midwater Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 27,804 rows and 11 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_fmwt"

#' SKT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Spring Kodiak Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 4,114 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{http://www.dfg.ca.gov/delta/projects.asp?ProjectID=SKT}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_skt"

#' STN water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Summer Townet Survey.
#'
#' @encoding UTF-8
#' @format a tibble with 8,506 rows and 12 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   \item{Notes}{Comments.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Townet-Survey}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_stn"

#' Suisun water quality data
#'
#' Water quality data from the UC Davis Suisun Marsh Fish Study.
#'
#' @encoding UTF-8
#' @format a tibble with 14,253 rows and 9 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://watershed.ucdavis.edu/project/suisun-marsh-fish-study}{here}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_suisun"

#' USBR water quality data
#'
#' Water quality data from the United States Bureau of Reclamation Sacramento Deepwater Ship Channel cruises.
#'
#' @encoding UTF-8
#' @format a tibble with 904 rows and 11 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Depth}{Bottom depth (m). Only 1 value per station, probably an average?}
#'   \item{Sample_depth_surface}{Depth (m) of surface sample.}
#'   \item{Sample_depth_bottom}{Depth (m) of bottom sample.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^{-1}}}) at surface.}
#'   }
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_usbr"

#' USGS water quality data
#'
#' Water quality data from the United States Geological Survey San Francisco Bay Water Quality Survey.
#'
#' @encoding UTF-8
#' @format a tibble with 22,149 rows and 10 variables
#' \describe{
#'   \item{Source}{Name of the source dataset.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Sample_depth_surface}{Depth (m) of surface sample.}
#'   \item{Sample_depth_bottom}{Depth (m) of bottom sample.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Temperature_bottom}{Temperature (°C) at bottom.}
#'   \item{Salinity}{Salinity at surface.}
#'   }
#' @details More metadata and information on methods are available \href{https://www.sciencebase.gov/catalog/item/5841f97ee4b04fc80e518d9f}{here for data from 1969-2015} and \href{https://www.sciencebase.gov/catalog/item/5966abe6e4b0d1f9f05cf551}{here for data from 2016-present}.
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaWQer}}, \code{\link{DeltaMetadater}}
"wq_usgs"

#' Zooplankton mass conversions
#'
#' Average dry mass for each meso and micro zooplankton taxa
#'
#' @format a tibble with 40 rows and 2 variables
#' \describe{
#'   \item{Mass}{Average individual mass (\eqn{\mu}g).}
#'   \item{Taxlifestage}{Taxonomic name and lifestage.}
#'   }
#' @seealso \code{\link{DeltaZooper}}, \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"zoop_mass_conversions"

#' Mysid data
#'
#' Mysid biomass per unit effort from the California Department of Fish and Wildlife Environmental Monitoring Program
#'
#' @format a tibble with 22,558 rows and 2 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Taxonomic name.}
#'   \item{BPUE}{Biomass per unit effort (\eqn{\mu}g \ifelse{html}{\out{m<sup>-3</sup>}}{\eqn{m^{-3}}}).}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year sample was collected.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Zooplankton-Study}{here}.
#' @seealso \code{\link{DeltaZooper}}, \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"zoop_mysid"
