% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{smelt_edsm}
\alias{smelt_edsm}
\title{Delta Smelt EDSM abundance estimates}
\format{
a tibble with 1,370 rows and 5 variables
\describe{
  \item{Region}{Region of abundance estimate.}
  \item{Date}{Abundance estimate date. Abundances are estimated weekly, but these dates represent the midpoint of each week.}
  \item{Abundance}{Estimated Delta Smelt abundance.}
  \item{Variance}{Variance of the abundance estimate.}
  \item{MonthYear}{Month and year of sample collection.}
  }
}
\usage{
smelt_edsm
}
\description{
Estimated Delta Smelt abundance dataset from the United States Fish and Wildlife Service Enhanced Delta Smelt Monitoring Program.
}
\details{
More metadata and information on methods are available \href{https://www.fws.gov/lodi/juvenile_fish_monitoring_program/jfmp_index.htm}{here}.
}
\seealso{
\code{\link{DeltaSmelter}}
}
\keyword{datasets}
