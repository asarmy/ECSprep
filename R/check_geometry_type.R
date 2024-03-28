#' Check the geometry of an sf object.
#'
#' @param sf_object The sf object.
#' @param geom_category A string describing the expected geometry type; only
#'   'line' or 'point' are allowed.
#'
#' @return None; the function will stop execution and throw an error if the
#'   geometry type of sf_object does not match the expected type.
#' @export
check_geometry_type <- function(sf_object, geom_category) {
  # Define mapping from simple types to specific geometry types
  geom_type_mapping <- list(line = c("LINESTRING", "MULTILINESTRING"),
                            point = c("POINT", "MULTIPOINT"))

  # Check if the specified category is valid
  if (!tolower(geom_category) %in% names(geom_type_mapping)) {
    stop("Error: Invalid geometry category. Please use one of the following: ",
         paste(names(geom_type_mapping), collapse = ", "), ".")
  }

  # Retrieve the valid types for the specified category
  valid_types <- geom_type_mapping[[geom_category]]

  # Perform the geometry type check
  geom_types <- unique(sf::st_geometry_type(sf_object))

  if (!all(geom_types %in% valid_types)) {
    stop(paste("Error: Geometry check failed. The sf object's geometry type must be one of:",
               paste(valid_types,collapse = ", "), ". Found types:",
               paste(geom_types, collapse = ", "), "."))
  }

  cat("Geometry check passed.\n")
}
