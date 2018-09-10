context("fx-fun-default")

test_that("fxext_default", {
  mf <- new_metaframe(name = character(0))
  expect_error(fxext_default(data.frame(), mf, fxd("default")))
  expect_equal(fxext_default(data.frame(), mf, fxd("default"), mustWork = FALSE),
               NULL)
})

test_that("fxint_default", {
  mf <- new_metaframe(name = character(0))
  df <- data.frame()
  metaframe(df) <- mf
  expect_error(fxint_default(df, NULL))
  expect_error(fxint_default(df, LETTERS))
  expect_equal(fxint_default(df, NULL, mustWork = FALSE), NULL)
})

test_that("fx_default", {
  mf <- new_metaframe(name = character(0))
  df <- data.frame()
  df2 <- df
  metaframe(df) <- mf
  expect_equal(fx_default(df), fx_default(df2))
})
