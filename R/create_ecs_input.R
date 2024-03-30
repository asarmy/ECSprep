#' Create ECS Input File from Esri Shapefile
#'
#' Processes an Esri shapefile, either of line or point geometry, for input to
#' the Lavrentiadis & Abrahamson ECS tool. The function transforms the
#' shapefile's CRS to EPSG:4326, adds a default 'Rank' field if missing, and
#' formats the data for ECS tool input. Output is saved as a CSV file in the
#' specified or default directory.
#'
#' @param in_filepath The path to the input Esri shapefile, recommended to use
#'   'file.path()' for construction.
#' @param geometry_type The type of geometry, 'line' for rupture lines or
#'   'point' for displacement measurement sites.
#' @param output_folder (Optional) Directory for saving the output CSV file;
#'   defaults to 'ECSprep-outputs' in the input file directory.
#'
#' @return Outputs a CSV file to the specified directory and prints the file
#'   location. Does not return any value from the function itself.
#' @export
#' @examples
#' create_ecs_input(file.path("path", "to", "your_ruptures.shp"), "line")
#' create_ecs_input(file.path("path", "to", "your_measurements.shp"), "point", file.path("path", "to", "output"))
#'
#' @keywords internal
create_ecs_input <- function(in_filepath, geometry_type, output_folder = NULL) {

  # Check that the input filepath is correct
  if (!file.exists(in_filepath)) {
    stop("Error: The specified input filepath `", in_filepath, "` cannot be found.\n")
  }

  # Check that a shapefile was entered
  file_extension <- tools::file_ext(in_filepath)

  if (tolower(file_extension) != "shp") {
    stop("Error: Only Esri Shapefiles, with extension `.shp` (case-insensitive), are allowed; a file of type .",
         file_extension, " was entered.\n")
  }

  # Import file using sf library
  sf_object <- sf::st_read(in_filepath, quiet = TRUE)

  # Convert the object to EPSG:4326 if it is not already in that CRS
  sf_object <- project_to_4326(sf_object)

  # Add the Rank field if it doesn't exist
  if (!"Rank" %in% names(sf_object)) {
    sf_object$Rank <- "None"
    warning("Rank attribute is missing. It was added with default values of `None`.\n")
  }

  # Check the rankings
  sf_object <- check_ranking(sf_object)

  # Create the data frame in the correct format
  if (geometry_type == "line") {
    dataframe <- rups2verts(sf_object)
  } else if (geometry_type == "point") {
    dataframe <- process_points(sf_object)
  } else {
    stop("Error: The `geoemtry_type` must be 'line' or 'point' but ", geometry_type, " was entered.\n")
  }

  # Set up output path and file name
  prefix <- ifelse(geometry_type == "line", "rupture_vertices-", "displacement_sites-")
  filename_out <- tools::file_path_sans_ext(basename(in_filepath))
  filename_out <- paste0(prefix, filename_out, ".csv")

  if (is.null(output_folder)) {
    output_folder <- file.path(dirname(in_filepath), "ECSprep-outputs")
  } else {
    if (nchar(tools::file_ext(output_folder)) > 0) {
      warning("Output filename overridden in the specified directory.\n")
      output_folder <- dirname(output_folder)
    }
  }

  # Save the data frame
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  }
  write.csv(dataframe, file.path(output_folder, filename_out), row.names = FALSE)
  cat("The data frame was saved in:\n", output_folder, "\n")

}

