context("fx-fun-info")

test_that("fx_info", {
  expect_error(fx_info(data.frame(), ""))
  df <- data.frame(column_name = 1:10)
  metaframe(df) <- new_metaframe(name = "column_name", fxInfo_new_topic = "tst")
  expect_equal(fx_info(df, "new_topic")$`New Topic`, "tst")
})
