test_that("geometry check passes when it should", {
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
  expect_output(check_geometry_type(sf_object, "line"), "Geometry check passed.")
})


test_that("geometry check returns an error when it should", {
  expect_error({
    sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
    check_geometry_type(sf_object, "point")
  })
})
