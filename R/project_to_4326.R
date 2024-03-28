#' Check the coordinate reference system of an sf object; it should be
#' EPSG:4326; covert if needed.
#'
#' @param sf_object The sf object.
#'
#' @return An sf object in EPSG:4326.
#' @export
project_to_4326 <- function(sf_object) {
  crs <- sf::st_crs(sf_object)$epsg
  if (is.null(crs) || crs != 4326) {
    sf_object_transformed <- sf::st_transform(sf_object, crs = 4326)
    cat("The sf object was projected to EPSG:4326.\n")
    return(sf_object_transformed)
  } else {
    return(sf_object)
  }
}
