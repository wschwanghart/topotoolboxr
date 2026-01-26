#' Checks a SpatRaster for GRIDobj requirements
#'
#' @description
#' `validateraster` checks whether a provided raster fulfills the requirements
#' of a TopoToolbox GRIDobj.
#'
#' @param raster character | SpatRaster
#'
#' The input can be either a path to a file containing the raster or a
#' SpatRaster from terra
#'
#' @return logical
#'
#' Returns TRUE if the raster fulfills the requirements
#'
#' @export
validateraster <- function(raster) {
  if (inherits(raster, "character")) {
    # Check path validity
    if (!file.exists(raster)) {
      stop("The file path in 'raster' does not exist: ", raster, call. = FALSE)
    }
    # Read the file as a SpatRaster
    r <- terra::rast(raster)
  } else if (inherits(raster, "SpatRaster")) {
    r <- raster
  } else {
    stop("Unsupported input type: ",
         paste(class(raster), collapse = ", "),
         call. = FALSE)
  }
  # Raster checks
  if (terra::nlyr(r) > 1) {
    stop("topotoolboxr only supports single-layer rasters.", call. = FALSE)
  }
  if (!identical(terra::xres(r), terra::yres(r))) {
    stop(
      sprintf("Raster cells must have identical raster and y resolutions.
              Found xres = %s, yres = %s.",
              terra::xres(r), terra::yres(r)),
      call. = FALSE
    )
  }
  if (!identical(terra::crs(r), "")) {
    if (terra::is.lonlat(r)) {
      stop("Coordinate reference system is not projected.", call. = FALSE)
    }
  }
  TRUE
}
