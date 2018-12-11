context("create_joyplot function")

test_that("errors generated for bad input", {
  data("christmas_playlists")
  expect_error(create_joyplot(christmas_playlists, valencee, c("red", "green")), "invalid parameter defined")
  expect_error(create_joyplot(christmas_playlists, key, c("red", "green")), "input variable must be numeric")
})
