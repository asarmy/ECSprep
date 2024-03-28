test_that("Test the geometry check for case that should pass.", {
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures.shp"), quiet = TRUE)
  expect_output(check_geometry_type(sf_object, "line"), "Geometry check passed.")
})


test_that("Test the geometry check for case that should return an error.", {
  expect_error({
    sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures.shp"), quiet = TRUE)
    check_geometry_type(sf_object, "point")
  })
})
