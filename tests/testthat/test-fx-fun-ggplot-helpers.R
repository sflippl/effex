context("fx-fun-ggplot-helpers")

test_that("required_aes", {
  expect_equal(required_aes(ggplot2::geom_point()), c("x", "y"))
  expect_equal(required_aes(ggplot2::geom_ribbon()), c("x", "ymin", "ymax"))
})

test_that("null_helper", {
  expect_equal(null_helper(1, "a"), 1)
  expect_equal(null_helper(NULL, "a"), "a")
  expect_equal(null_helper(list(NULL), "a"), NULL)
})
