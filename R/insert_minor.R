#' Minor axis breaks
#'
#' Function to create minor axis breaks
#' @param major_labs Vector with sequence of major labels
#' @param n_minor Number of minor breaks to place between major breaks.
#' @keywords internal

insert_minor <- function(major_labs,
                         n_minor) {
  labs <-c( sapply( major_labs, function(x) c(x, rep("", n_minor) ) ) )
  return(labs[1:(length(labs)-n_minor)])
}
