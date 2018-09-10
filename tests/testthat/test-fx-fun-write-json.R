context("fx-fun-write-json")

test_that("save_filenames", {
  expect_equal(save_filenames("list/vectors?"), "list or vectors")
  expect_equal(save_filenames("list\\vectors|"), "list vectors")
  expect_equal(save_filenames(".list\"vectors\""), "list'vectors'")
})

test_that("json_file_data", {
  data <- dplyr::tibble(high_level = c(rep(1, 5), rep(2, 5)), low_level = 1:10,
                 chars = "a")
  path <- "dummy/path"
  file_data <- json_file_data(data, path)
  expect_equal(tidyr::nest(file_data)$.file_path, "dummy/path/")
  expect_true(dplyr::is_grouped_df(file_data))
  file_data <- json_file_data(dplyr::group_by(data, high_level), path)
  expect_equal(tidyr::nest(file_data)$.file_path,
               c("dummy/path/1/1", "dummy/path/2/2"))
  file_data <-
    json_file_data(dplyr::group_by(data, high_level, low_level), path)
  expect_equal(nrow(tidyr::nest(file_data)), 10)
})

test_that("fx_write_json and fx_read", {
  data <- dplyr::tibble(high_level = c(rep(1, 5), rep(2, 5)),
                        low_level = 1:10,
                        name = 1:10,
                        chars = "a")
  path <- tempfile()
  expect_equal(fx_write_json(data, path), data)
  written_data <- fx_write_json(data, path, chars = "chars")
  expect_true(all(stringr::str_detect(written_data[["chars"]],
                                      "chars\\.json$")))
  expect_equal(read_filepath(written_data$chars),
               dplyr::select(data, name, chars))
  path <- tempfile()
  written_data <- fx_write_json(dplyr::group_by(data, high_level), path,
                                chars = "chars")
  expect_equal(length(unique(written_data$chars)), 2)
  expect_equal(read_filepath(written_data$chars),
               dplyr::select(data, name, chars))
  path <- tempfile()
  written_data <- fx_write_json(dplyr::group_by(data, high_level, low_level),
                                path, chars = "chars")
  expect_equal(read_filepath(written_data$chars),
               dplyr::select(data, name, chars))
  path <- tempfile()
  written_data <- fx_write_json(dplyr::group_by(data, high_level, low_level),
                                path, chars = "chars", low_level = "low_level")
  expect_equal(read_filepath(written_data$chars),
               dplyr::select(data, name, chars))
  path <- tempfile()
  written_data <- fx_write_json(dplyr::group_by(data, high_level, low_level),
                                path, chars = c("chars", "low_level"))
  expect_equal(read_filepath(written_data$chars),
               dplyr::select(data, name, chars, low_level))
  expect_equal(fx_read(written_data), data)
  expect_equal(fx_read(data), data)
})
