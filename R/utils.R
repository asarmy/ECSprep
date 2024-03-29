#' Check the coordinate reference system of an sf object. It should be 4326 for
#' our application.
#'
#' @param sf_object The sf object.
#'
#' @return None; the function will stop execution and throw an error if the CRS
#'   of sf_object is not 4326.
#' @keywords internal
#' @export
check_crs <- function(sf_object) {

  crs <- sf::st_crs(sf_object)

  if (is.null(crs$epsg) || crs$epsg != 4326) {
    stop("Error: Coordinate reference system check failed. The sf object's CRS must EPSG:4326.")
  } else {
    cat("Coordinate reference system check passed.\n")
  }
}


#' Check the geometry of an sf object.
#'
#' @param sf_object The sf object.
#' @param geom_category A string describing the expected geometry type; only
#'   'line' or 'point' are allowed.
#'
#' @return None; the function will stop execution and throw an error if the
#'   geometry type of sf_object does not match the expected type.
#' @keywords internal
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


#' Check that the values in the Rank attribute are valid; allowable values are
#' None, Primary, Principal, Secondary, Distributed. Note that Primary and
#' Secondary will be replaced with Principal and Distributed, respectively, for
#' standardization purposes.
#'
#' @param sf_object The sf object.
#'
#' @return The sf object with rank cleaned-up as needed. The function will stop
#'   execution and throw an error if an invalid Rank is found.
#' @keywords internal
#' @export
check_ranking <- function(sf_object) {
  # Standardizing rank nomenclature; update ranks and track changes
  original_rank <- sf_object$Rank

  sf_object <- sf_object %>%
    dplyr::mutate(Rank = dplyr::case_when(
      Rank == "Primary" ~ "Principal",
      Rank == "Secondary" ~ "Distributed",
      TRUE ~ as.character(Rank)
    ))

  if (!all(original_rank == sf_object$Rank)) {
    warning("Some values in `Rank` were renamed for the purposes of standardization.\n")
  }

  # Check for invalid entries
  valid_options <- c("None", "Principal", "Distributed")
  invalid_entries <- !sf_object$Rank %in% valid_options

  if (any(invalid_entries)) {
    invalid_values <- unique(sf_object$Rank[invalid_entries])
    stop("Error: The following invalid `Rank` values were found: ",
         paste(invalid_values, collapse = ", "), ".")
  } else {
    cat("Rank check passed.\n")
  }

  return(sf_object)
}


#' Check the coordinate reference system of an sf object; it should be
#' EPSG:4326; covert if needed.
#'
#' @param sf_object The sf object.
#'
#' @return An sf object in EPSG:4326.
#' @keywords internal
#' @export
project_to_4326 <- function(sf_object) {
  crs <- sf::st_crs(sf_object)$epsg
  if (is.null(crs) || crs != 4326) {
    sf_object_transformed <- sf::st_transform(sf_object, crs = 4326)
    warning("The sf object was projected to EPSG:4326.\n")
    return(sf_object_transformed)
  } else {
    return(sf_object)
  }
}


#' Convert rupture line work to vertex format for the Lavrentiadis & Abrahamson ECS tool.
#'
#' @param sf_object The sf object with LINESTRING geometry.
#'
#' @return A data frame with the rupture lines converted to vertices.
#' @keywords internal
#' @export
rups2verts <- function(sf_object) {
  # Check that the object is line-type
  check_geometry_type(sf_object, "line")

  # Convert the object to EPSG:4326 if it is not already in that CRS
  sf_object <- project_to_4326(sf_object)

  # Add the RUP_ID field if it doesn't exist
  if (!"RUP_ID" %in% names(sf_object)) {
    sf_object$RUP_ID <- seq_len(nrow(sf_object))
  }

  # Add the Rank field if it doesn't exist
  if (!"Rank" %in% names(sf_object)) {
    sf_object$Rank <- "None"
    warning("Rank attribute is missing. It was added with default values of `None`.\n")
  }

  # Convert to line work to vertices
  # Add column for NODE_ID, which is vertex order per RUP_ID
  # Add Latitude, Longitude columns
  # Clean up: drop geometry field, convert to data frame
  df_vertices <- suppressWarnings(sf::st_cast(sf_object, "POINT")) %>%
    dplyr::group_by(RUP_ID) %>%
    dplyr::mutate(NODE_ID = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Latitude = sf::st_coordinates(.)[, "Y"],
                  Longitude = sf::st_coordinates(.)[, "X"]) %>%
    sf::st_set_geometry(NULL) %>%
    as.data.frame()

  return(df_vertices)
}
