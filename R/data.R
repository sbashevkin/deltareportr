#' Bivalve dataset
#'
#' Bivalve abundance dataset from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @format a tibble with 8,652 rows and 7 variables
#' \describe{
#'   \item{Date}{Sample collection date.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Bivalve species name.}
#'   \item{CPUE}{Catch per unit effort in number of clams \eqn{m^{-2}}.}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://emp.baydeltalive.com/projects/11280}{here}.
#' @seealso \code{\link{DSCBivalver}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"bivalves"

#' Dayflow dataset
#'
#' Outflow and X2 from the California Department of Water Resources Dayflow model.
#'
#' @format a tibble with 8,652 rows and 7 variables
#' \describe{
#'   \item{Date}{Date.}
#'   \item{Out}{Delta outflow (\eqn{ft^{3}} \eqn{s^{-1}}.)}
#'   \item{X2}{X2 (km).}
#'   \item{CPUE}{Catch per unit effort in number of clams \eqn{m^{-2}}.}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://data.cnra.ca.gov/dataset/dayflow}{here}.
#' @seealso \code{\link{DSCBivalver}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"dayflow"

#' Delta regions
#'
#' Region polygons for the Sacramento San Joaquin Delta from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @format an sf tibble with 8 rows and 2 variables
#' \describe{
#'   \item{Stratum}{Region.}
#'   \item{geometry}{Polygon coordinates.}
#'   }
#' @seealso \code{\link{DSCMapper}}, \code{\link{DSCDater}}
"deltaregions"

#' Phytoplankton dataset
#'
#' Phytoplankton abundance dataset from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @format a tibble with 8,652 rows and 7 variables
#' \describe{
#'   \item{Date}{Sample collection date.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Phytoplankton taxa.}
#'   \item{CPUE}{Catch per unit effort in number of cells, colonies, or filaments \eqn{ml^{-1}}.}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://emp.baydeltalive.com/projects/11282}{here}.
#' @seealso \code{\link{DSCPhyter}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
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
#' @seealso \code{\link{DSCSmelter}}
"smelt_edsm"

#' Delta Smelt IEP indices
#'
#' Delta Smelt abundance indices from the California Department of Fish and Wildlife surveys.
#'
#' @format a tibble with 152 rows and 3 variables
#' \describe{
#'   \item{Year}{Index year.}
#'   \item{Index}{Delta Smelt index number.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available here: \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{Fall Midwater Trawl}, \href{https://wildlife.ca.gov/Conservation/Delta/Spring-Kodiak-Trawl}{Spring Kodiak Trawl}, \href{https://wildlife.ca.gov/Conservation/Delta/20mm-Survey}{20mm Survey}, and \href{https://wildlife.ca.gov/Conservation/Delta/Townet-Survey}{Summer Townet}.
#' @seealso \code{\link{DSCSmelter}}
"smelt_iep"

#' Station locations
#'
#' Locations of all sampling stations.
#'
#' @format a tibble with 1,052 rows and 5 variables
#' \describe{
#'   \item{Source}{Name of the source dataset..}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{StationID}{Combined source and station for a unique station ID.}
#'   }
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"stations"

#' 20mm water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife 20mm survey.
#'
#' @encoding UTF-8
#' @format a tibble with 9,458 rows and 12 variables
#' \describe{
#'   \item{Station}{Station where sample was collected.}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Notes}{Comments.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Date}{Date sample was collected.}
#'   \item{Tide}{Tidal stage.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/20mm-Survey}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_20mm"

#' EDSM water quality data
#'
#' Water quality data from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 14,851 rows and 11 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Notes}{Comments.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Source}{Name of the source dataset.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.415.1}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_edsm"

#' EMP water quality data
#'
#' Water quality data from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 16,293 rows and 8 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Chlorophyll}{Chlorophyll concentration (\eqn{\mu}g \eqn{L^{-1}})}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://portal.edirepository.org/nis/mapbrowse?packageid=edi.458.2}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_emp"

#' FMWT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Fall Midwater Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 27,319 rows and 10 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Source}{Name of the source dataset.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   }
#' @details More metadata and information on methods are available \href{https://www.dfg.ca.gov/delta/projects.asp?ProjectID=FMWT}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_fmwt"

#' SKT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Spring Kodiak Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 3,965 rows and 12 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Tide}{Tidal stage.}
#'   \item{Notes}{Comments.}
#'   \item{Source}{Name of the source dataset.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   }
#' @details More metadata and information on methods are available \href{http://www.dfg.ca.gov/delta/projects.asp?ProjectID=SKT}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_skt"

#' TNS water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Summer Townet Survey.
#'
#' @encoding UTF-8
#' @format a tibble with 8,266 rows and 11 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Tide}{Tidal stage.}
#'   \item{Depth}{Bottom depth (m).}
#'   \item{Notes}{Comments.}
#'   \item{Source}{Name of the source dataset.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Townet-Survey}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_tns"

#' Suisun water quality data
#'
#' Water quality data from the UC Davis Suisun Marsh Fish Study.
#'
#' @encoding UTF-8
#' @format a tibble with 14,253 rows and 10 variables
#' \describe{
#'   \item{Station}{Station where sample was collected.}
#'   \item{Date}{Date sample was collected.}
#'   \item{QADone}{Was QA completed?}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Salinity}{Salinity (ppt) at the surface.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Tide}{Tidal stage.}
#'   \item{Datetime}{Date and time of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://watershed.ucdavis.edu/project/suisun-marsh-fish-study}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_suisun"

#' Zooplankton mass conversions
#'
#' Average dry mass for each meso and micro zooplankton taxa
#'
#' @format a tibble with 40 rows and 2 variables
#' \describe{
#'   \item{Mass}{Average individual mass (\eqn{\mu}g).}
#'   \item{Taxlifestage}{Taxonomic name and lifestage.}
#'   }
#' @seealso \code{\link{DSCZooper}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"zoop_mass_conversions"

#' Mysid data
#'
#' Mysid biomass per unit effort from the California Department of Fish and Wildlife Environmental Monitoring Program
#'
#' @format a tibble with 40 rows and 2 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Taxonomic name.}
#'   \item{BPUE}{Biomass per unit effort (\eqn{\mu}g \eqn{m^{-3}}).}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year sample was collected.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{https://wildlife.ca.gov/Conservation/Delta/Zooplankton-Study}{here}.
#' @seealso \code{\link{DSCZooper}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"zoop_mysid"
