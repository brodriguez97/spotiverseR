context("filter_by_ss function")

test_that("errors generated for bad input", {
  data(definitions)
  expect_error(
    filter_by_ss(data = christmas_playlists, col = playlist_name, substring = "jazz")
    ,
    "no rows identified - substring is case sensative"
  )
  expect_error(
    filter_by_ss(data = christmas_playlists, col = playlist, substring = "jazz")
    ,
    "column cannot be identified - try a valid column name"
  )
})
