#' Check whether multiple objects are aligned
#'
#' @description
#' `validate_alignment` checks that the objects have the same
#' `shape` attribute and, if coordinate information is available, the
#' same coordinate system.
#'
#' @param ... matrix | array | GRIDobj | FLOWobj | STREAMobj
#'
#' Objects to test for alignment
#'
#' @return logical
#'
#' \code{TRUE} if the two objects are aligned, \code{FALSE} otherwise
#'
#' @examples
#' \dontrun{
#' M <- matrix(1:25, 5, 5)
#' DEM <- terra::rast(M, crs="EPSG:25833")
#' FD <- FLOWobj(DEM)
#' print(validatealignment(M, DEM, FD))
#' }
#'
#' @export
validatealignment <- function(...) {
  args <- list(...)
  log_gri <- sapply(args, inherits, "GRIDobj")
  log_ras <- sapply(args, inherits, "SpatRaster")
  if (any(log_gri)) {
    idx_crs <- which(log_gri)[1]
    ini <- args[[idx_crs]]$raster
  } else if (any(log_ras)) {
    idx_crs <- which(log_ras)[1]
    ini <- args[[idx_crs]]
  } else {
    idx_crs <- 1
    ini <- args[[idx_crs]]
  }

  args <- args[-idx_crs]
  check <- logical(length(args))

  for (idx in seq_along(args)) {
    if (inherits(args[[idx]], "GRIDobj")) {
      check[idx] <- terra::compareGeom(ini, args[[idx]]$raster)
    } else if (inherits(args[[idx]], "SpatRaster")) {
      check[idx] <- terra::compareGeom(ini, args[[idx]])
    } else {
      check[idx] <- all(dim_cr(ini) == dim_cr(args[[idx]]))
    }
  }
  all(check)
}