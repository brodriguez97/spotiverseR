context("create_wordcloud function")

test_that("errors generated for bad input", {
  expect_error(
    create_wordcloud(data = christmas_playlists, "Christmas Classicsh", c("yeah", "like"), del_file = T),
    "playlist doesn't exist"
  )
})
