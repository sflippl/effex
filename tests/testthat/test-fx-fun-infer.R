context("fx-fun-infer")

test_that("fxext_infer", {
  mf <- new_metaframe(name = character(0))
  expect_error(fxext_infer(data.frame(), mf, fxd("infer")))
  expect_equal(fxext_infer(data.frame(), mf, fxd("infer"), mustWork = FALSE),
               NULL)
})

test_that("fxint_infer", {
  mf <- new_metaframe(name = character(0))
  df <- data.frame()
  metaframe(df) <- mf
  expect_error(fxint_infer(df, NULL))
  expect_error(fxint_infer(df, LETTERS))
  expect_equal(fxint_infer(df, NULL, mustWork = FALSE), NULL)
})

test_that("fx_infer", {
  mf <- new_metaframe(name = character(0))
  df <- data.frame()
  df2 <- df
  metaframe(df) <- mf
  expect_equal(fx_infer(df), fx_infer(df2))
})
