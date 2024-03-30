#' Check CRS of an sf Object
#'
#' Verifies that the coordinate reference system (CRS) of an 'sf' object is set
#' to EPSG:4326, which is required for specific geographical applications. This
#' CRS corresponds to WGS 84, a global geographic coordinate system. The
#' function halts execution with an error if the CRS is not EPSG:4326.
#'
#' @param sf_object The 'sf' object to check, representing geographical features
#'   and their attributes.
#'
#' @return Does not return a value but halts execution with an error if the CRS
#'   of 'sf_object' is not EPSG:4326.
#'
#' @keywords internal
#' @export
check_crs <- function(sf_object) {
  crs <- sf::st_crs(sf_object)

  if (is.null(crs$epsg) || crs$epsg != 4326) {
    stop(paste0("Error: Coordinate reference system check failed. ",
                "The sf object's CRS must be EPSG:4326, but found EPSG:", crs$epsg, ".\n"))
  }

  cat("Coordinate reference system check passed.\n")
}


#' Check Geometry Type of an sf Object
#'
#' Validates whether the geometry of an 'sf' object matches the specified
#' category. This function is designed to ensure that spatial operations are
#' performed on the appropriate geometry types, specifically 'line' and 'point'
#' geometries.
#'
#' @param sf_object An 'sf' object, which represents spatial features.
#' @param geom_category A string indicating the expected geometry category.
#'   Accepts 'line' for LINESTRING and MULTILINESTRING geometries, or 'point'
#'   for POINT and MULTIPOINT geometries.
#'
#' @return Does not return a value but halts execution with an error message if
#'   the 'sf_object''s geometry does not match the expected category.
#'
#' @keywords internal
#' @export
check_geometry_type <- function(sf_object, geom_category) {
  # Define mapping from simple types to specific geometry types
  geom_category <- tolower(geom_category)
  geom_type_mapping <- list(line = c("LINESTRING", "MULTILINESTRING"),
                            point = c("POINT", "MULTIPOINT"))

  # Check if the specified category is valid
  if (!geom_category %in% names(geom_type_mapping)) {
    stop("Error: Invalid geometry category '", geom_category, "'. Please use one of the following: ",
         paste(names(geom_type_mapping), collapse = ", "), ".\n")
  }

  # Retrieve the valid types for the specified category
  valid_types <- geom_type_mapping[[geom_category]]

  # Perform the geometry type check
  geom_types <- unique(sf::st_geometry_type(sf_object))

  if (!all(geom_types %in% valid_types)) {
    stop("Error: Geometry check failed. Expected '", geom_category, "' but found types: ",
         paste(geom_types, collapse = ", "), ".\n")
  }

  cat("Geometry check passed.\n")
}



#' Standardize and Validate 'Rank' Attribute in an sf Object
#'
#' Ensures that the 'Rank' attribute of an 'sf' object contains only valid
#' values and standardizes them by replacing "Primary" with "Principal" and
#' "Secondary" with "Distributed". It checks for any invalid 'Rank' values and
#' stops execution with an error if any are found.
#'
#' @param sf_object An 'sf' object with a 'Rank' attribute.
#'
#' @return Returns the modified 'sf' object with the 'Rank' attribute
#'   standardized and validated. If invalid 'Rank' values are present, execution
#'   is halted with an error message.
#'
#' @keywords internal
#' @export
check_ranking <- function(sf_object) {
  # Check that the 'Rank' column exists
  if (!"Rank" %in% names(sf_object)) {
    stop("The 'Rank' column is missing from the sf_object.")
  }

  # Update ranks as needed to standardize nomenclature
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
    stop("Error: Invalid 'Rank' values found: ",
         paste(invalid_values, collapse = ", "),
         ". Valid options are 'None', 'Principal', 'Distributed'.")
  } else {
    cat("Rank check passed.\n")
  }

  return(sf_object)
}



#' Transform sf Object to EPSG:4326 CRS
#'
#' Checks the coordinate reference system (CRS) of an 'sf' object and transforms
#' it to EPSG:4326 if it is not already in this CRS. EPSG:4326 corresponds to
#' WGS 84, a widely used geographical coordinate system.
#'
#' @param sf_object An 'sf' object representing spatial features. The function
#'   checks and, if necessary, converts the CRS of this object to EPSG:4326.
#'
#' @return Returns the 'sf' object in the EPSG:4326 coordinate reference system.
#'   If the original object is already in EPSG:4326, it is returned unchanged.
#'
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


#' Convert rupture line work to a data frame of vertices with necessary
#' attributes for the ECS Tool
#'
#' Transforms an 'sf' object containing LINESTRING geometries into a data frame
#' of vertices. Each vertex is assigned a unique node ID within its rupture line
#' (RUP_ID) and includes latitude and longitude coordinates. This function is
#' tailored for preparing input data for the Lavrentiadis & Abrahamson ECS tool
#' by structuring the 'sf' object's line work into a vertex format in a data
#' frame.
#'
#' @param sf_object An 'sf' object, expected to contain LINESTRING geometries.
#'
#' @return Returns a data frame where each row represents a vertex from the
#'   original LINESTRING geometries. The data frame includes columns for RUP_ID
#'   (rupture line ID), NODE_ID (node order within each RUP_ID), and
#'   Latitude/Longitude coordinates.
#'
#' @keywords internal
#' @export
rups2verts <- function(sf_object) {
  # Check that the object is line-type
  check_geometry_type(sf_object, "line")

  # Add the RUP_ID field if it doesn't exist
  if (!"RUP_ID" %in% names(sf_object)) {
    sf_object$RUP_ID <- seq_len(nrow(sf_object))
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


#' Convert measurement sites to a data frame with necessary attributes for the
#' ECS Tool
#'
#' Transforms an 'sf' object containing POINT geometries and associated
#' displacement measurements into a data frame. This function is tailored for
#' preparing input data for the Lavrentiadis & Abrahamson ECS tool .
#'
#' @param sf_object An 'sf' object, expected to contain POINT geometries
#'   representing measurement sites.
#' @param displ_meas_col The name of the column within 'sf_object' that contains
#'   fault displacement measurements. This column is converted to numeric, if
#'   not already, and its values are used to populate the 'displacement' column
#'   in the output.
#'
#' @return Returns a data frame derived from the input 'sf' object. The data
#'   frame includes the original data (except for displacement values that are
#'   negative, NaN, or NA), a renamed 'displacement' column, point IDs
#'   ('PT_ID'), and coordinates ('Latitude', 'Longitude').
#'
#' @keywords internal
#' @export
process_points <- function(sf_object, displ_meas_col) {
  # Ensure the geometry is of type "point"
  check_geometry_type(sf_object, "point")

  # Check that the specified displacement measurement field exists
  if (!displ_meas_col %in% names(sf_object)) {
    stop("Error: The specified displacement measurement column could not be found: ", displ_meas_col, "\n")
  }

  # Convert displacement column to numeric if it's not already
  if (!is.numeric(sf_object[[displ_meas_col]])) {
    sf_object[[displ_meas_col]] <- as.numeric(as.character(sf_object[[displ_meas_col]]))

    if (any(is.na(sf_object[[displ_meas_col]]))) {
      warning(paste0("NAs introduced by coercion; some values in '", displ_meas_col, "' could not be converted to numeric.\n"))
    }
  }

  # Drop rows with negative values in displacement column
  if (any(sf_object[[displ_meas_col]] < 0, na.rm = TRUE)) {
    sf_object <- sf_object[sf_object[[displ_meas_col]] >= 0, ]

    warning(paste0("Rows with negative values in '", displ_meas_col, "' have been removed.\n"))
  }

  # Drop rows with NaN values in displacement column
  if (any(is.nan(sf_object[[displ_meas_col]]), na.rm = TRUE)) {
    # Filter out NaN values
    sf_object <- sf_object[!is.nan(sf_object[[displ_meas_col]]), ]

    warning(paste0("Rows with NaN values in '", displ_meas_col, "' have been removed.\n"))
  }

  # Drop rows with NA values in displacement column
  if (any(is.na(sf_object[[displ_meas_col]]), na.rm = TRUE)) {
    # Filter out NA values
    sf_object <- sf_object[!is.na(sf_object[[displ_meas_col]]), ]

    warning(paste0("Rows with NA values in '", displ_meas_col, "' have been removed.\n"))
  }

  # Rename the displacement column for standardization
  sf_object$displacement <- sf_object[[displ_meas_col]]

  # Add the PT_ID field if it doesn't exist
  if (!"PT_ID" %in% names(sf_object)) {
    sf_object$PT_ID <- seq_len(nrow(sf_object))
  }

  # Add Latitude, Longitude columns
  # Clean up: drop geometry field, drop original displ field, convert to data frame
  df_pts <- sf_object %>%
    dplyr::mutate(Latitude = sf::st_coordinates(.)[, "Y"],
                  Longitude = sf::st_coordinates(.)[, "X"]) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(-displ_meas_col) %>%
    as.data.frame()

  return(df_pts)
}
