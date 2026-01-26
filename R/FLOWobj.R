#' Create instance of a FLOWobj
#' 
#' @description
#' `FLOWobj`is the constructor for the FLOWobj. It takes a GRIDobj from
#' topotoolboxr or a SpatRaster from terra as input, computes flow direction
#' information and saves them as an FLOWobj.
#' 
#' @param DEM GRIDobj
#' 
#' The GRIDobj that will be the basis of the computation.
#' 
#' @param bc numeric array or matrix, optional
#' 
#' Boundary conditions for sink filling. `bc` should match the shape of the DEM.
#' Values of 1 indicate pixels that should be fixed to their values in the
#' original DEM and values of 0 indicate pixels that should be filled.
#' 
#' @param hybrid logical, optional
#' 
#' Should hybrid reconstruction algorithm be used to fill sinks? Defaults to
#' True. Hybrid reconstruction is faster but requires additional memory be
#' allocated for a queue.
#' 
#' @return FLOWobj
#' 
#' An object containing vectors of source and target pixels for each edge and
#' an empty SpatRaster storing the spatial information of the original DEM.
#' 
#' @details
#' Large intermediate arrays are created during the initialization process,
#' which could lead to issues when using very large DEMs.
#' 
#' @examples
#' \dontrun{
#' DEM <- terra::rast(system.file("ex/elev.tif",package="terra"))
#' DEM <- terra::project(DEM,"EPSG:32632",res=90.0)
#' FD <- FLOWobj(DEM)
#' }
#' 
#' @import terra
#' 
#' @export

FLOWobj <- function(DEM,
                    bc=NULL,
                    hybrid=TRUE) {
  # Input checks
  if (!inherits(DEM, "GRIDobj") && !inherits(DEM, "SpatRaster")) {
    stop("DEM must be either a GRIDobj or a SpatRaster from terra.")
  }
  
  # Initial computations
  dem <- get_grid_data(DEM)
  output <- single(length(dem$z)) # Preallocate output for all computations
  restore_nans <- FALSE
  
  # fillsinks
  if (!is.null(bc)){ ## Check requirements for user boundary conditions
    if((is.matrix(bc)|is.array(bc))&!all(bc %in% c(0,1))) {
      # Check if bc is (matrix OR array) AND all values either 0 or 1
      stop("bc must be either NULL, a matrix or an array containing 0s and 1s.")
    }
    if (!validatealignment(DEM, bc)) {
      # Check if dimensions match
      stop("DEM and bc do not align.")
    }
    bc <- t(bc)
  } else { # If the user does not provide bcs they are manually created
    bc <- matrix(0,dem$dims[1],dem$dims[2])
    # Missing data and borders are fixed to their original value
    nans <- is.na(dem$z)
    
    bc[nans] <- 1
    bc[1,] <- 1
    bc[,1] <- 1
    bc[nrow(bc),] <- 1
    bc[,ncol(bc)] <- 1
    
    dem$z[nans] <- -Inf
    restore_nans <- TRUE
  }
  ## Check requirements for hybrid (algorithm choice)
  if (!is.logical(hybrid)){
    stop("hybrid must be logical.")
  }
  
  ## Fillsink computation
  if (hybrid){
    results <- .C("wrap_fillsinks_hybrid",
                  outputR=as.single(output), # float
                  as.single(dem$z), # float
                  as.integer(bc), # uint8_t
                  as.integer(dem$dims), # ptrdiff_t
                  NAOK = TRUE
                  )$outputR
  } else {
    results <- .C("wrap_fillsinks",
                  outputR=as.single(output), # float
                  as.single(dem$z), # float
                  as.integer(bc), # uint8_t
                  as.integer(dem$dims), # ptrdiff_t
                  NAOK = TRUE
                  )$outputR
  }
  filled <- results
  
  if (restore_nans) {
    dem$z[nans] <- NA
    filled[nans] <- NA
  }

  # identifyflats
  results <- .C("wrap_identifyflats",
                outputR=as.integer(output), # int32_t
                as.single(filled), # float
                as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)$outputR
  flats <- results
  
  # gwdt_computecosts
  results <- .C("wrap_gwdt_computecosts",
                costsR = as.single(output),
                flatsR = as.integer(flats),
                original_demR = as.single(dem$z),
                filled_demR = as.single(filled),
                dimsR = as.integer(dem$dims),
                NAOK = TRUE)$costsR
  
  # gwdt
  results <- .C("wrap_gwdt",
                distR = as.single(output), # float
                costsR = as.single(results), # float
                flatsR = as.integer(flats), # int32_t
                dimsR = as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)$distR
  
  # flow_routing_d8_carve
  results <- .C("wrap_flow_routing_d8_carve",
                sourceR = as.integer(output), # ptrdiff_t
                directionR = as.integer(output), # uint8_t
                demR = as.single(filled), # float
                distR = as.single(results), # float, results = distR from gwdt
                flatsR = as.integer(flats), # uint32_t
                dimsR = as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)

  # Write output relevant for FLOWobj into SpatRaster
  NODE <- DEM
  terra::values(NODE) <- results$sourceR
  DIRECTION <- DEM
  terra::values(DIRECTION) <- results$directionR
  
  # flow_routing_d8_edgelist
  results <- .C("wrap_flow_routing_d8_edgelist",
                edge_countR = integer(1),
                sourceR = as.integer(output), # ptrdiff_t
                targetR = as.integer(output), # ptrdiff_t
                nodeR = as.integer(results$sourceR), # ptrdiff_t
                directionR = as.integer(results$directionR), # uint8_t
                dimsR = as.integer(dem$dims), # ptrdiff_t
                NAOK = TRUE)
  
  # Store spatial metadata
  DEMpty <- terra::rast(nrows = terra::nrow(DEM),
                        ncols = terra::ncol(DEM),
                        ext = terra::ext(DEM),
                        crs = terra::crs(DEM),
                        nlyrs = terra::nlyr(DEM))
  
  # Create list representation of FLOWobj
  FD <- list(source = results$sourceR[1:results$edge_countR],
             target = results$targetR[1:results$edge_countR],
             direction = DIRECTION,
             stream = NODE,
             #type = , # currently topotoolboxr only computes single flow directions
             raster = DEMpty)
  
  # Assign class and return FLOWobj
  class(FD) <- "FLOWobj"
  return(FD)
}

#' FLOWobj dimensions
#'
#' @description
#' `dim_cr.FLOWobj` computes the  dimensions of the underlying grid of the
#' FLOWobj in the correct order for libtopotoolbox.
#'
#' @param x FLOWobj
#'
#' FLOWobj for which to copmute the dimensions
#'
#' @return numeric vector
#'
#' Dimensions of the grid
dim_cr.FLOWobj <- function(x){
  r <- x$raster
  c(terra::ncol(r), terra::nrow(r))
}

#' Unravel indices
#' 
#' @description
#' `unravel_index.FLOWobj` unravels the provided linear indices so they can be
#' used to index grids.
#' 
#' @param TTobj FLOWobj
#' 
#' FLOWobj for which to compute the grid indices
#' 
#' @param idxs vector or matrix or array
#' 
#' Flat indices to convert to grid indices
#' 
#' @return n x 2 matrix
#' 
#' Row indices and column indices of the provided flat indices
#' 
#' @export
unravel_index.FLOWobj <- function(TTobj, idxs) {
  # Input checks
  if (!(is.vector(idxs) || is.matrix(idxs) || (is.array(idxs) && length(dim(idxs)) == 1))) {
    stop("Input 'idxs' must be a one-dimensional vector, a matrix, or an array.")
  }
  dims <- dim_cr(TTobj)
  
  # Calculate the strides for each dimension (row-major)
  strides <- c(1, cumprod(dims[-length(dims)]))
  
  # Use outer division and modulo vectorized for each dimension
  grid_indices <- sapply(rev(strides), function(s) {
    grid_idx <- (idxs %/% s) %% dims[length(strides) - which(rev(strides) == s) + 1]
  })
  return(grid_indices)
}

#' Unravel source indices
#' 
#' @description
#' `source_indices.FLOWobj` computes row and column indices of the sources of
#' each edge in the flow network.
#' 
#' @param TTobj FLOWobj
#' 
#' FLOWobj for which to compute the grid indices of the source pixels
#' 
#' @return matrix
#' 
#' Row indices and column indices of the source pixels
#' 
#' @export
source_indices.FLOWobj <- function(TTobj){
  return(unravel_index(TTobj, TTobj$source))
}

#' Unravel target indices
#' 
#' @description
#' ``target_indices.FLOWobj` computes row and column indices of the targets of
#' each edge in the flow network.
#' 
#' @param TTobj FLOWobj
#' 
#' FLOWobj for which to compute the grid indices of the target pixels
#' 
#' @return n x 2 matrix
#' 
#' Row indices and column indices of the target pixels
#' 
#' @export
target_indices.FLOWobj <- function(TTobj){
  return(unravel_index(TTobj, TTobj$target))
}

#' FLOWobj node attribute list
#' 
#' @description
#' Retrieve a node attribute list
#' 
#' @param TTobj FLOWobj
#' The FLOWobj for which to retrieve the node attribute list
#' 
#' @param k GridObject or matrix or array or scalar
#' 
#' The object from which node values will be extracted. If
#' `k` is a `GridObject` or an `ndarray` with the same shape
#' as this `FlowObject`, then a copy is returned. If it is a
#' scalar, an `ndarray` with the appropriate shape, filled
#' with `k`, is returned.
#' 
#' @return GRIDobj or array
#' 
#' @export
ezgetnal.FLOWobj <- function(TTobj, k) {
  if (is.atomic(k) & length(k) == 1) {
    return(array(k, dim = length(get_grid_data(TTobj$stream)$z)))
  }
  return(k)
}
