context("fx-fun-infer-fxInfo")

df <- dplyr::tibble(a = 1:10,
                    b = factor(LETTERS[1:10]),
                    c = c(as.list(1:5), as.list(LETTERS[6:10])))

test_that("Infer the name", {
  expect_equal(metaframe(fx_infer(df, columns = "fxInfo_name"))$fxInfo_name,
               names(df))
})
