#' Create ECS Hazard Sites Input File from Esri Shapefile
#'
#' Processes an Esri shapefile with point geometry for input to the Lavrentiadis
#' & Abrahamson ECS tool. The function transforms the shapefile's CRS to
#' EPSG:4326 and formats the data for ECS tool input. The output is saved as a
#' CSV file in the specified or default directory.
#'
#' @param in_filepath The path to the input Esri shapefile, recommended to use
#'   'file.path()' for construction.
#' @param output_folder (Optional) Directory for saving the output CSV file;
#'   defaults to 'ECSprep-outputs' in the input file directory.
#'
#' @return Outputs a CSV file to the specified directory and prints the file
#'   location. Does not return any value from the function itself.
#'
#' @export
create_ecs_input_haz_sites <- function(in_filepath, output_folder = NULL) {

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

  # Check that a CRS (i.e., a PRJ file) is provided with the SHP
  check_crs(sf_object)

  # Convert the object to EPSG:4326 if it is not already in that CRS
  sf_object <- project_to_4326(sf_object)

  # Analysis
  dataframe <- process_hazsite(sf_object)

  # Set up output path and file name
  prefix <- "hazard_sites-"
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
  out_filepath <- file.path(output_folder, filename_out)
  write.csv(dataframe, file.path(output_folder, filename_out), row.names = FALSE)
  cat("The data frame was saved in:\n", output_folder, "\n")

}

