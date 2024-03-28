test_that("the ruptures are converted to vertices correctly when the shapefile is in EPSG:4326", {
  # Load the ruptures shapefile and covert it to a vertices data frame
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
  test_result <- rups2verts(sf_object)

  # Load the expected vertices data frame generated in ArcGIS
  expected_result <- read.csv(test_path("data", "neftegorsk_vertices.csv"))

  # Define the columns to check
  columns <- c("RUP_ID", "NODE_ID", "Latitude", "Longitude")

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


test_that("the ruptures are converted to vertices correctly when the shapefile is in a different projection", {
  # Load the ruptures shapefile and convert it to a vertices data frame
  # Expect a warning because the Rank column is not present; that is okay
  sf_object <- sf::st_read(test_path("data", "example_ruptures_epsg32611.shp"), quiet = TRUE)
  test_result <- suppressWarnings(rups2verts(sf_object))

  # Load the expected vertices data frame generated in ArcGIS
  expected_result <- read.csv(test_path("data", "example_ruptures_vertices.csv"))

  # Define the columns to check
  columns <- c("RUP_ID", "NODE_ID", "Latitude", "Longitude")

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
