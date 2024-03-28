


test_that("multiplication works", {

  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures.shp"), quiet = TRUE)

  rups2verts(sf_object)
  expect_equal(2 * 2, 4)
})
