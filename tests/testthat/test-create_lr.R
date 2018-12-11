context("create_lr function")

test_that("errors generated for bad input", {
  data("christmas_playlists")
  expect_error(create_lr(
    data = christmas_playlists,
    param = tempoo,
    param2 = valence,
    points = "darkgreen",
    line = "red",
    theme = "light",
    multi = F
  ), "invalid param defined")
  expect_error(create_lr(
    data = christmas_playlists,
    param = tempo,
    param2 = valencee,
    points = "darkgreen",
    line = "red",
    theme = "light",
    multi = F
  ), "invalid param2 defined")
  expect_error(create_lr(
    data = christmas_playlists,
    param = tempo,
    param2 = valence,
    points = "darkgreen",
    line = "red",
    theme = "rainbow",
    multi = F
  ), "theme must be light or dark")
  expect_error(create_lr(
    data = christmas_playlists,
    param = key,
    param2 = valence,
    points = "darkgreen",
    line = "red",
    theme = "light",
    multi = F
  ), "input variable must be numeric")
})
