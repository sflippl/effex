context("test-fx-fun-info-title")

test_that("Basic title inference", {
  ex <- data.frame(column_name = 1:9)
  expect_equal(fx_info(ex, "title")[[" "]], "column name")
  expect_equal(fx_info(rbind(ex, data.frame(column_name = NA)), "title")[[" "]],
               "column name (NAs: 1)")
  ex <- fx_infer(ex)
  metaframe(ex) <- metaframe(ex) %>%
    dplyr::mutate(fxInfo_title_na.show = TRUE, fxInfo_title_n.show = TRUE) %>%
    as_metaframe()
  expect_equal(fx_info(ex, "title")[[" "]], "column name (NAs: 0, n: 9)")
  metaframe(ex) <- metaframe(ex) %>%
    dplyr::mutate(fxInfo_title_na.show = FALSE, fxInfo_title_n.show = FALSE) %>%
    as_metaframe()
  expect_equal(fx_info(ex, "title")[[" "]], "column name")
  metaframe(ex) <- metaframe(ex) %>%
    dplyr::mutate(fxInfo_title_n.show = TRUE,
                  fxInfo_title_stats = list(list(Median = "median"))) %>%
    as_metaframe()
  expect_equal(fx_info(ex, "title")[[" "]], "column name (n: 9, Median: 5)")
  metaframe(ex) <- metaframe(ex) %>%
    dplyr::mutate(fxInfo_title_unit.show = TRUE, fxInfo_unit = "$") %>%
    as_metaframe()
  expect_equal(fx_info(ex, "title")[[" "]], "column name (n: 9, Median: 5) [$]")
  metaframe(ex) <- metaframe(ex) %>% dplyr::mutate(
    fxInfo_title_fun = list(
      function(fxInfo_name, ...)
        stringr::str_to_upper(fxInfo_name)
    )
  ) %>% as_metaframe
  expect_equal(fx_info(ex, "title")[[" "]], "COLUMN NAME")
  metaframe(ex) <- metaframe(ex) %>%
    dplyr::mutate(fxInfo_title = "Column 1") %>%
    as_metaframe()
  expect_equal(fx_info(ex, "title")[[" "]], "Column 1")
})
