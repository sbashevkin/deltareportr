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
#' @format a tibble with 202,420 rows and 7 variables
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
#' @format a tibble with 1,370 rows and 5 variables
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
#' @format a tibble with 1,266 rows and 5 variables
#' \describe{
#'   \item{Source}{Name of the source dataset..}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{StationID}{Combined source and station for a unique station ID.}
#'   }
#' @seealso \code{\link{DeltaDater}}, \code{\link{DeltaMetadater}}
"stations"

#' Zooplankton mass conversions
#'
#' Average dry mass for each meso and micro zooplankton taxa
#'
#' @format a tibble with 37 rows and 2 variables
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
