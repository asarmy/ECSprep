test_that("the coordinate reference system is converted to EPSG:4326", {
  sf_object <- sf::st_read(test_path("data", "example_ruptures_epsg32611.shp"), quiet = TRUE)
  sf_object_converted <- project_to_4326(sf_object)
  crs <- sf::st_crs(sf_object_converted)$epsg
  expect_equal(crs, 4326)
})

test_that("the coordinate reference system remains EPSG:4326", {
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
  sf_object_converted <- project_to_4326(sf_object)
  crs <- sf::st_crs(sf_object_converted)$epsg
  expect_equal(crs, 4326)
})
