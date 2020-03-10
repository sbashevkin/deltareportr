#' Bivalve dataset
#'
#' Bivalve abundance dataset from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @format a tibble with 8652 rows and 7 variables
#' \describe{
#'   \item{Date}{Sample collection date.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Bivalve species name.}
#'   \item{CPUE}{Catch per unit effort in number of clams \eqn{m^{-2}}.}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCBivalves}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"bivalves"

#' Delta regions
#'
#' Region polygons for the Sacramento San Joaquin Delta from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @format an sf tibble with 8 rows and 2 variables
#' \describe{
#'   \item{Stratum}{Region.}
#'   \item{geometry}{Polygon coordinates.}
#'   }
#' @seealso \code{\link{DSCmap}}, \code{\link{DSCDater}}
"deltaregions"

#' Phytoplankton dataset
#'
#' Phytoplankton abundance dataset from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @format a tibble with 8652 rows and 7 variables
#' \describe{
#'   \item{Date}{Sample collection date.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Taxa}{Phytoplankton taxa.}
#'   \item{CPUE}{Catch per unit effort in number of cells, colonies, or filaments \eqn{ml^{-1}}.}
#'   \item{Year}{Year sample was collected.}
#'   \item{MonthYear}{Month and year of sample collection.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCBivalves}}, \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"phytoplankton"

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
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCsmelter}}
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
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCsmelter}}
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
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"stations"

#' EDSM water quality data
#'
#' Water quality data from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 1,052 rows and 5 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Latitude}{Latitude in decimal degrees.}
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{Temperature}{Temperature in °C.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_edsm"

#' EMP water quality data
#'
#' Water quality data from the California Department of Water Resources Environmental Monitoring Program.
#'
#' @encoding UTF-8
#' @format a tibble with 16,355 rows and 9 variables
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
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_emp"

#' FMWT water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Fall Midwater Trawl.
#'
#' @encoding UTF-8
#' @format a tibble with 27,319 rows and 7 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_fmwt"

#' STN water quality data
#'
#' Water quality data from the California Department of Fish and Wildlife Summer Townet.
#'
#' @encoding UTF-8
#' @format a tibble with 8,266 rows and 7 variables
#' \describe{
#'   \item{Date}{Date sample was collected.}
#'   \item{Station}{Station where sample was collected.}
#'   \item{Secchi}{Secchi depth (cm).}
#'   \item{Temperature}{Temperature (°C) at surface.}
#'   \item{Conductivity}{Specific conductance (\eqn{\mu}S \eqn{cm^{-1}}) at surface.}
#'   \item{Microcystis}{Microcystis bloom intensity on a qualitative scale from 1 to 5 where 1 = absent, 2 = low, 3 = medium, 4 = high, and 5 = very high.}
#'   \item{Source}{Name of the source dataset.}
#'   }
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCWQer}}, \code{\link{DSCMetadater}}
"wq_stn"
