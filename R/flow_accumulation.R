#' Compute flow accumulation (upslope area)
#'
#' @description
#' Computes the flow accumulation for a given flow network stored as FLOWobj
#' using optional weights. The flow accumulation represents the amount of
#' flow each cell receives from its upstream neighbors.
#'
#' @param fd FLOWobj
#'
#' The FLOWobj that will be the basis of the computation.
#'
#' @param weights numeric array or matrix, optional
#'
#' An array of the same shape as the flow grid representing weights for each
#' cell, or a constant float value used as the weight for all cells.
#' If `weights=1.0` (default), the flow accumulation is unweighted.
#' If an ndarray is provided, it must match the shape of the flow grid., by
#' default 1.0
#'
#' @return GRIDobj
#'
#' A new GRIDobj containing the flow accumulation grid.
#'
#' @examples
#'
#' data(srtm_bigtujunga30m_utm11)
#' DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
#' FD <- FLOWobj(DEM)
#' FA <- flow_accumulation(FD)
#'
#' @export

flow_accumulation <- function(fd,
                              weights = 1.0) {
  # Input checks
  if (!inherits(fd, "FLOWobj")) {
    stop("fd must be a FLOWobj.")
  }

  # Input delineations
  dims <- dim_cr(fd)
  edge_count <- length(fd$source)

  # Compute flow routing using libtopotoolbox
  output <- as.single(ezgetnal(fd, weights))
  result <- .C(
    "wrap_traverse_down_f32_add_mul",
    accR = output, # float
    fractionR = as.single(rep(1, edge_count)), # float
    sourceR = as.integer(fd$source), # ptrdiff_t
    targetR = as.integer(fd$target), # ptrdiff_t
    edge_countR = as.integer(edge_count), # ptrdiff_t
    NAOK = TRUE
  )$accR

  # Write result into SpatRaster (for now)
  fa <- fd$raster
  terra::values(fa) <- result
  GRIDobj(fa)
}
