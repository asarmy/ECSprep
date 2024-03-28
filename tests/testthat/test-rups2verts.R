test_that("the ruptures are coverted to vertices correctly", {
  # Load the ruptures shapefile and covert it to a vertices data frame
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures.shp"), quiet = TRUE)
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


