#' Convert rupture line work to vertex format for the Lavrentiadis & Abrahamson ECS tool.
#'
#' @param sf_object The sf object with LINESTRING geometry.
#'
#' @return A data frame with the rupture lines converted to vertices.
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
