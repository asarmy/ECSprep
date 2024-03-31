test_that("the points are formatted correctly", {
  # Load the points shapefile and project it to EPSG:4326
  sf_object <- sf::st_read(test_path("data", "galway_lake_sites_epsg32611.shp"), quiet = TRUE)
  sf_object <- sf::st_transform(sf_object, crs = 4326)

  # Format the results into the data frame
  test_result <- process_points(sf_object)

  # Load the expected vertices data frame generated in ArcGIS
  expected_result <- read.csv(test_path("data", "galway_lake_sites_epsg4326.csv"))

  # Define the columns to check
  columns <- c("PT_ID",  "Latitude", "Longitude", "displ")

  # Check the columns exist in both data frames
  expect_true(all(columns %in% names(test_result)))
  expect_true(all(columns %in% names(expected_result)))

  # Compare values in the columns
  tol <- 0.0001
  for (col in columns) {
    expect_equal(test_result[[col]], expected_result[[col]],
                 tolerance = tol,
                 info = paste("Column values not within tolerance for", col))
  }
})


test_that("an error is thrown when the displacement column is not found", {
  expect_error({
    sf_object <- sf::st_read(test_path("data", "galway_lake_sites_epsg32611.shp"), quiet = TRUE)
    sf_object <- sf_object %>% dplyr::select(-displ)

    process_points(sf_object)
  })
})


test_that("negative displacement values, e.g. -999, are dropped", {
  # Load the points shapefile and project it to EPSG:4326
  sf_object <- sf::st_read(test_path("data", "galway_lake_sites_epsg32611.shp"), quiet = TRUE)

  # Make one of the measurements -999
  sf_object[2, "displ"] <- -999
  expect_warning({process_points(sf_object)},
                 "Rows with negative values in 'displ' have been removed.")
})


test_that("NaN displacement values are dropped", {
  # Load the points shapefile and project it to EPSG:4326
  sf_object <- sf::st_read(test_path("data", "galway_lake_sites_epsg32611.shp"), quiet = TRUE)

  # Make one of the measurements NaN
  sf_object[2, "displ"] <- NaN
  expect_warning({process_points(sf_object)},
                 "Rows with NaN values in 'displ' have been removed.")
})


test_that("NA displacement values are dropped", {
  # Load the points shapefile and project it to EPSG:4326
  sf_object <- sf::st_read(test_path("data", "galway_lake_sites_epsg32611.shp"), quiet = TRUE)

  # Make one of the measurements NA
  sf_object[2, "displ"] <- NA
  expect_warning({process_points(sf_object)},
                 "Rows with NA values in 'displ' have been removed.")
})


