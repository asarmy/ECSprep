test_that("rank check issues warning when it renames the ranks", {
  expect_warning({
    sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
    check_ranking(sf_object)
  })
})

test_that("rank check passes when it should", {
  sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
  expect_output(suppressWarnings(check_ranking(sf_object)), "Rank check passed.")
})


test_that("rank check returns an error when it should", {
  expect_error({
    sf_object <- sf::st_read(test_path("data", "neftegorsk_ruptures_epsg4326.shp"), quiet = TRUE)
    sf_object$Rank <- "meow"
    check_ranking(sf_object)
  })
})
