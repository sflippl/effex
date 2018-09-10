context("fx-fun-helpers")

test_that("lst_mf_args", {
  mf <- as_metaframe(dplyr::tibble(
    name = "column_name",
    arg1 = TRUE,
    arg2 = list(list(1, 2)),
    arg3 = "a"
  ))
  expect_equal(lst_mf_args(mf), list(name = "column_name",
                                     arg1 = TRUE,
                                     arg2 = list(1, 2),
                                     arg3 = "a"))
})
