context("define_feat function")

test_that("errors generated for bad input", {
  data(definitions)
  expect_error(
    define_feat(liveliness),
    "invalid input, try feat_names()"
  )
  expect_error(
    define_feat("liveliness"),
    "invalid input, try feat_names()"
  )
  expect_error(
    define_feat(0),
    "invalid input, try feat_names()"
  )
})
