#' SpatRaster dimensions
#'
#' @description
#' `dim_cr.SpatRaster` retrieves the dimensions of a SpatRaster in the
#' correct order for libtopotoolbox.
#'
#' @param x SpatRaster
#' @return integer vector
#'
#' Dimensions of the SpatRaster
dim_cr.SpatRaster <- function(x) {
  c(terra::ncol(x), terra::nrow(x))
}