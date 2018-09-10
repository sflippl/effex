context("fx-fun-info-statistics")

test_that("stat_names", {
  expect_equal(
    stat_names(list(a = mean, "b", c = "d")),
    letters[1:3]
  )
  expect_equal(stat_names(c("a", "b", c = "d")), letters[1:3])
  expect_error(stat_names(list(mean)))
  expect_error(stat_names(list(a = "a", a = "b")))
})

test_that("fx_info", {
  expect_equal(
    fx_info(data.frame(column_name = 1:9), "stats", statistics = "mean"),
    dplyr::tibble(name = "column_name", mean = 5)
  )
  expect_equal(
    fx_info(data.frame(column_name = 1:9),
            c("stats", "name"), statistics = "mean"),
    dplyr::tibble(name = "column_name", mean = 5, Name = "column name")
  )
  expect_equal(
    fx_info(data.frame(column_name = 1:9), "stats", statistics = "quantile") %>%
      names,
    c("name", paste0("quantile: ", seq(0, 100, 25), "%"))
  )
  expect_equal(
    fx_info(data.frame(column_name = 1:9), "stats",
            statistics =
              list(quantile = function(x, ...)
                quantile(x, probs = seq(0, 1, 0.2)))) %>% names,
    c("name", paste0("quantile: ", seq(0, 100, 20), "%"))
  )
  expect_equal(
    fx_info(data.frame(col1 = c(0, 1), col2 = c(1, 2)), "stats", "mean"),
    dplyr::tibble(name = c("col1", "col2"), mean = c(0.5, 1.5))
  )
  expect_identical(
    fx_info(data.frame(col1 = c(0,1)), "stats", "range"),
    dplyr::tibble(name = "col1", range = list(c(0, 1)))
  )
  expect_identical(
    fx_info(data.frame(col1 = 1:10, col2 = 1), "stats", "table")$table,
    list(table(1:10), table(rep(1, 10)))
  )
})
