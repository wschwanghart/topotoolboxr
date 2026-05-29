#' identifyflats
#'
#' This function identifies flat pixels in a digital elevation model. 
#' A flat pixel is one surrounded by pixels with the same or higher 
#' elevations and has the value 1 in the output grid. In addition, 
#' the function identifies also sill (value = 2) and presill pixels 
#' (value = 5). presill pixels are located next to sill pixels and
#' have the same elevation. The function uses libtopotoolbox. 
#'
#' @param dem GRIDobj | SpatRaster
#'
#' Digital elevation model
#'
#' @return GRIDobj | SpatRaster
#'
#' Grid of identified flats, sills and presills
#' 
#' @examples
#'
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#' DEMf <- fillsinks(DEM)
#' I <- identifyflats(DEMf)
#' plot(I)
#'
#' @export

identifyflats <- function(dem) {
  # Input validation
  dem <- processgrid(dem)
  provided_go <- dem$provided_go
  dem <- dem$r

  # Extract input data
  d <- get_grid_data(dem)

  # Handle NaN values
  log_nans <- is.na(d$z)
  d$z[log_nans] <- min(d$z, na.rm = T) - 999

  # Compute flats using libtopotoolbox
  output <- integer(length(d$z))
  result <- .C(
    "wrap_identifyflats",
    outputR = as.integer(output),
    as.single(d$z),
    as.integer(d$dims)
  )$outputR
  result[log_nans] <- 0

  # Write results into SpatRaster or optionally GRIDobj
  flats <- dem
  terra::values(flats) <- result
  if (provided_go) {
    flats <- GRIDobj(flats)
  }
  flats
}