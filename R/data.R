#' Path to 30 m SRTM elevation data for Big Tujunga catchment
#'
#' Character string containing the file path to the 30 m resolution Shuttle Radar 
#' Topography Mission (SRTM) elevation data TIFF file for the Big Tujunga 
#' catchment in California, located in `inst/extdata/`.
#'
#' @format ## `srtm_bigtujunga30m_utm11`
#' A single length character string giving the full path to the GEOTIFF file:
#' \describe{
#'   \item{file}{Path to `srtm_bigtujunga30m_utm11.tif` (643 rows, 1197 columns, 1 layer, WGS 84 / UTM zone 11N EPSG:32611)}
#' }
#' @usage data(srtm_bigtujunga30m_utm11)
#' @examples
#' \dontrun{
#' data(srtm_bigtujunga30m_utm11)
#' library(topotoolboxr)
#' GRIDobj(srtm_bigtujunga30m_utm11)
#' }
"srtm_bigtujunga30m_utm11"
