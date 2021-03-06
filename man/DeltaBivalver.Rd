% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DeltaBivalver.R
\name{DeltaBivalver}
\alias{DeltaBivalver}
\title{Plot bivalve data}
\usage{
DeltaBivalver(
  Data,
  End_year,
  Start_year = 2002,
  Regions = c("Suisun Bay", "Suisun Marsh", "Lower Sacramento River",
    "Sac Deep Water Shipping Channel", "Cache Slough/Liberty Island",
    "Lower Joaquin River", "Southern Delta"),
  Seasons = c("Winter", "Spring", "Summer", "Fall")
)
}
\arguments{
\item{Data}{Input dataset created by \code{\link{DeltaDater}}.}

\item{End_year}{Last year (integer) that should be included in the plot. This year will also be highlighted.}

\item{Start_year}{First year (integer) that should be included in the plot}

\item{Regions}{Character vector of regions to include in the plot. The data will be filtered to only include these regions and ordered in the order provided here. To include data with NA regions, set \code{Regions=NULL}.}

\item{Seasons}{Character vector of seasons to include. One plot will be produced for each season. Should be a combination of "Summer", "Fall", "Winter", or "Spring".}
}
\value{
A list with the plot and processed data.
}
\description{
Function to process and plot bivalve data
}
\seealso{
\code{\link{DeltaDater}}
}
