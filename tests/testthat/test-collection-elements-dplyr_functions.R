context("collection-elements-dplyr_functions")

test_that("select", {
  library(dplyr)
  df <- data.frame(a = 1:10, b = 2 * (1:10))
  c_df <- collection_df(df)
  expect_identical(select(c_df, a), collection_df(select(df, a)))
  key(c_df) <- "a"
  expect_identical(c_df, select(c_df, b))
  expect_error(select(c_df, a = b))
  expect_error(select(c_df, b = a, b))
  expect_identical(select(c_df, a), collection_df(select(df, a), "a"))
})

test_that("mutate", {
  library(dplyr)
  df <- data.frame(a = 1:10, b = 2 * (1:10))
  c_df <- collection_df(df)
  key(c_df) <- "a"
  expect_error(mutate(c_df, a = NULL))
  expect_error(mutate(c_df, a = 1))
  expect_identical(mutate(c_df, c = 1),
                   collection_df(mutate(df, c = 1), key = "a"))
})

test_that("arrange, filter and transmute", {
  library(dplyr)
  df <- data.frame(a = 1:10, b = 2 * (1:10))
  c_df <- collection_df(df)
  expect_identical(collection_df(arrange(df, desc(b)), "b"),
                   arrange(change_key(c_df, "b"), desc(b)))
  expect_identical(collection_df(filter(df, b <= 10), "b"),
                   filter(change_key(c_df, "b"), b <= 10))
  key(c_df) <- "a"
  expect_identical(key(transmute(c_df, c = letters[b])), "a")
})

test_that("join", {
  x <- data.frame(a = 1:10, b = 2 * (1:10))
  y <- data.frame(b = 2 * (1:10), c = letters[1:10])
  c_x <- collection_df(x, "a")
  c_y <- collection_df(y, "b")
  expect_error(package_required("blarbthispackagewillneverexist"))
  expect_error(inner_join.collection_df(c_x, c_y))
  library(dplyr)
  expect_identical(join_key(c_x, c_y, "b"), c("a", "b"))
  c_x_i_c_y <- inner_join(c_x, c_y, by = "b")
  expect_identical(c_x_i_c_y, inner_join(x, y, by = "b") %>%
                     collection_df(c("a", "b")))
  expect_identical(c_x_i_c_y, inner_join(c_x, c_y))
  expect_identical(inner_join(c_x, c_y, by = c("a" = "b")) %>% key, "a")
  expect_identical(left_join(c_x, c_y, by = c("a" = "b")),
                   collection_df(left_join(x, y, by = c("a" = "b")), "a"))
  expect_identical(right_join(c_x, c_y, by = c("a" = "b")),
                   collection_df(right_join(x, y, by = c("a" = "b")), "a"))
  expect_identical(full_join(c_x, c_y, by = c("a" = "b")),
                   collection_df(full_join(x, y, by = c("a" = "b")), "a"))
  expect_identical(key(semi_join(c_x, c_y, by = c("a" = "b"))), "a")
  expect_identical(key(anti_join(c_x, c_y, by = c("a" = "b"))), "a")
  key(c_x) <- NULL
  expect_identical(right_join(c_x, c_y, by = c("a" = "b")),
                   collection_df(right_join(x, y, by = c("a" = "b")), NULL))
})
