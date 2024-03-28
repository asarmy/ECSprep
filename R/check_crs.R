#' Check the coordinate reference system of an sf object. It should be 4326 for
#' our application.
#'
#' @param sf_object The sf object.
#'
#' @return None; the function will stop execution and throw an error if the CRS
#'   of sf_object is not 4326.
#' @export
check_crs <- function(sf_object) {

  crs <- sf::st_crs(sf_object)

  if (is.null(crs$epsg) || crs$epsg != 4326) {
    stop("Error: Coordinate reference system check failed. The sf object's CRS must EPSG:4326.")
  } else {
    cat("Coordinate reference system check passed.\n")
  }
}
