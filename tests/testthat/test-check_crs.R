test_that("coordinate reference system check passes when it should", {
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures.shp"), quiet = TRUE)
  expect_output(check_crs(sf_object), "Coordinate reference system check passed.")
})


test_that("coordinate reference system returns an error when it should", {
  expect_error({
    sf_object <- sf::st_read(test_path("data", "galway_lake_sites.shp"), quiet = TRUE)
    check_crs(sf_object)
  })
})
