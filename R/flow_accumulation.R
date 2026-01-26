#' Compute accumulation of flow
#' 
#' @description
#' Computes the flow accumulation for a given flow network using optional
#' weights. The flow accumulation represents the amount of flow each cell
#' receives from its upstream neighbors.
#' 
#' @param FD FLOWobj
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
#' A new GridObject containing the flow accumulation grid.
#' 
#' @examples
#' \dontrun{
#' DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
#' DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
#' FD <- FLOWobj(DEM)
#' FA <- flow_accumulation(FD)
#' }
#' 
#' @import terra
#' 
#' @export

flow_accumulation <- function(FD,
                              weights = 1.0) {
  # Input checks
  if (!inherits(FD, "FLOWobj")) {
    stop("FD must be a FLOWobj.")
  }
  
  # Input delineations
  dims <- dim_cr(FD)
  edge_count <- length(FD$source)
  
  # Compute flow routing using libtopotoolbox
  output <- single(prod(dims))
  result <- .C("wrap_flow_accumulation_edgelist",
               accR = as.single(output), # float
               sourceR = as.integer(FD$source), # ptrdiff_t
               targetR = as.integer(FD$target), # ptrdiff_t
               fractionR = as.single(rep(1, edge_count)), # float
               weightsR = as.single(ezgetnal(FD, weights)), # float
               edge_countR = as.integer(edge_count), # ptrdiff_t
               dimsR = as.integer(dims), # ptrdiff_t
               NAOK = TRUE)$accR
  
  # Write result into SpatRaster (for now)
  FA <- FD$raster
  terra::values(FA) <- result
  return(FA)
}