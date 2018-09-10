context("fx-fun-output")

test_that("General form", {
  expect_error(fx_output(data.frame(), form = NULL))
})

test_that("Standard and asis form", {
  info_tst <- dplyr::tibble(
    name = c("column_name", "column_name2"),
    ` ` = c("Column Name", "A second Column Name")
  )
  expect_equal(fx_output(info_tst), dplyr::select(info_tst, -name))
  expect_equal(fx_output(info_tst, "asis"), info_tst)
})

test_that("Collapse form", {
  ex <- dplyr::tibble(name = c("column_name", "column_name2"),
                       mean = 5, median = c(4, 6))
  expect_equal(fx_output(ex, "collapse"),
               c("mean: 5, median: 4", "mean: 5, median: 6"))
  expect_equal(fx_output(ex, "collapse", out_format = "latex"),
               c("mean: 5, median: 4", "mean: 5, median: 6"))
  expect_equal(fx_output(ex, "collapse", cell_scheme = "{value} ({name})",
                         cell_sep = "\n"),
               c("5 (mean)\n4 (median)", "5 (mean)\n6 (median)"))
})

test_that("Table form", {
  ex <- dplyr::tibble(name = c("column_name", "column_name2"),
                      mean = 5, median = c(4, 6))
  expect_equal(fx_output(ex, "table"), knitr::kable(ex, format = "rst"))
  expect_equal(fx_output(ex, "table", out_format = "markdown"),
               knitr::kable(ex, format = "markdown"))
  expect_equal(fx_output(ex, "table", out_format = "html"),
               knitr::kable(ex, format = "html") %>%
                 kableExtra::kable_styling())
})

test_that("Report form", {
  ex3 <- dplyr::tibble(name = c("col1", "col2", "col3", "col4"),
                       chapter = c("ch1", "ch1", "ch2", "ch2"),
                       section = c("sec1.1", "sec1.2", "sec2.1", "sec2.1"),
                       mean = 5) %>%
    dplyr::group_by(chapter, section)
})
