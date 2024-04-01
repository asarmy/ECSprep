test_that("coordinate reference system check passes when it should", {
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
  expect_output(check_crs(sf_object), "Coordinate reference system check passed.")
})


test_that("coordinate reference system returns an error when it should", {
  expect_error({
    sf_object <- sf::st_read(test_path("data", "data_no_crs.shp"), quiet = TRUE)
    check_crs(sf_object)
  })
})
