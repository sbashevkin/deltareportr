#' deltareportr: A package to create automated reports for the Sacramento San Joaquin Delta
#'
#' This report contains a number of functions and datasets to generate automated reports on the Delta, as well as the reports themselves in the analysis folder.
#' @docType package
#' @name deltareportr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
