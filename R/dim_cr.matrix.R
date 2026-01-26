#' Matrix dimensions
#'
#' @description
#' `dim_cr.matrix` retrieves the dimensions of a matrix in the correct
#' order for libtopotoolbox.
#'
#' @param x matrix
#' @return numeric vetor
#'
#' Dimensions of the matrix
#' @keywords internal
dim_cr.matrix <- function(x) {
	c(ncol(x), nrow(x))
}