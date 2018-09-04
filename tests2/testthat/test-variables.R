context("variables")

test_that("Internal setup works", {
  empty_vars <- tectr:::variable()
  expect_true(is_collection_df(empty_vars))
  expect_true(is.data.frame(empty_vars))
  expect_true(key(empty_vars) == new_key(c("col", "df_name", "df_key")))
  vars1 <- tectr:::variable(ind_name = "indicator1",
                    col = "indicator_one",
                    df_key = c("key_one"))
  expect_identical(vars1, bind_coll_rows(empty_vars, vars1))
})
