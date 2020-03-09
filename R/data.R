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
#'   \item{Latitude}{Latitude in decimal degrees. }
#'   \item{Longitude}{Longitude in decimal degrees.}
#'   \item{StationID}{Combined source and station for a unique station ID.}
#'   }
#' @details More metadata and information on methods are available \href{}{here}.
#' @seealso \code{\link{DSCDater}}, \code{\link{DSCMetadater}}
"stations"
