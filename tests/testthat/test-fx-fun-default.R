context("fx-fun-default")

test_that("fxe_default", {
  mf <- new_metaframe(name = character(0))
  expect_error(fxe_default(data.frame(), mf, fxd("default")))
  expect_equal(fxe_default(data.frame(), mf, fxd("default"), mustWork = FALSE),
               NULL)
})

test_that("fxi_default", {
  mf <- new_metaframe(name = character(0))
  df <- data.frame()
  metaframe(df) <- mf
  expect_error(fxi_default(df, NULL))
  expect_error(fxi_default(df, LETTERS))
  expect_equal(fxi_default(df, NULL, mustWork = FALSE), NULL)
})

test_that("fx_default", {
  mf <- new_metaframe(name = character(0))
  df <- data.frame()
  df2 <- df
  metaframe(df) <- mf
  expect_equal(fx_default(df), fx_default(df2))
})
