context("Collection elements")

test_that("Keys are recognized", {
  library(dplyr)
  df <- data.frame(a = 1:10, b = 2 * (1:10))
  c_el <- change_key(df, key = "a")
  expect_identical(key(c_el), "a")
  expect_null(key(df))
  f <- function(a, c) 2 * a
  expect_identical(key(f), c("a", "c"))
  key(f) <- "a"
  expect_identical(key(f), "a")
  expect_error(key(f) <- "b")
  expect_error(key(df) <- "c")
  c_el <- mutate(c_el, c = 1)
  expect_error(key(df) <- "c")
  key(c_el) <- c("a", "c")
  expect_true("c" %in% key(c_el))
})
