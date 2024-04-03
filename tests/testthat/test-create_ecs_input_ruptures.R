test_that("the ruptures are converted to vertices correctly", {
  # Create the CSV file
  # Warning expected because the rankings will be standardized
  suppressWarnings(create_ecs_input_ruptures(test_path("data", "neftegorsk_ruptures_epsg4326.shp")))

  # This is the default filepath and naming convention
  outpath <- test_path("data", "ECSprep-outputs", "rupture_vertices-neftegorsk_ruptures_epsg4326.csv")

  test_result <- read.csv(outpath)

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


test_that("error is issued when input file cannot be found", {
  expect_error({
    create_ecs_input_ruptures(test_path("data", "not a real file.shp"))
  })
})


test_that("error is issued when input file is not SHP", {
  expect_error({
    create_ecs_input_ruptures(test_path("data", "blah.txt"))
  })
})


test_that("error is issued when input file does not have a CRS", {
  expect_error({
    create_ecs_input_ruptures(test_path("data", "data_no_crs.shp"))
  })
})
