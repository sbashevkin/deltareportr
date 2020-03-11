#' Produce Delta Smelt Conditions Report
#'
#' Function to render the Delta Smelt Conditions Report
#'
#' @param File Filename and path to location where the report should be saved.
#' @param ... Additional parameters to provide to \code{\link[rmarkdown]{render}}.
#' @details See \code{\link[rmarkdown]{render}} for more details.
#' @keywords internal

DeltaSmeltReporter <- function(File, ...){
  rmarkdown::render(input = system.file("analysis/paper/Delta Smelt conditions report.Rmd", package="deltareportr"),
                    output_file = File,
                    ...)
}
