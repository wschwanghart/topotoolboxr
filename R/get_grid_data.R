#' Get grid data
#'
#' @description
#' `get_grid_data` retrieves the values, cellsize and dimension of a provided
#' GRIDobj or SpatRaster.
#'
#' @param DEM GRIDobj | SpatRaster
#'
#' Object for which to retrieve grid data
#'
#' @returns list
#'
#' A list with the components `z`, a vector containing the grid data in
#' row-major order, `cellsize`, the horizontal resolution of the grid, and
#' `dims` a two-element vector with the number of columns and rows of the grid.
get_grid_data <- function(DEM) {
  if (inherits(DEM, "SpatRaster")) {
    if (!validateraster(DEM)) {
      stop("Provided SpatRaster did not fulfill topotoolboxr requirements.")
    }
    list(z = terra::values(DEM, mat = FALSE),
         cellsize = terra::xres(DEM),
         dims = dim_cr(DEM))
  } else if (inherits(DEM, "GRIDobj")) {
    r <- DEM$raster
    list(z = terra::values(r, mat = FALSE),
         cellsize = terra::xres(r),
         dims = dim_cr(r))
  } else {
    stop("Input must be either a GRIDobj or a SpatRaster from terra.")
  }
}