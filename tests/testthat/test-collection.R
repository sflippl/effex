context("collection")

test_that("collections can be built", {
  expect_identical(length(collection()), 0L)
  c_x <- collection_df(data.frame(a = 5:15, b = letters[5:15]))
  expect_error(collection(c_x))
  c_y <- collection_df(data.frame(c = 1:11, b = letters[5:15]))
  expect_identical(length(collection(a = c_x, b = c_y)), 2L)
  expect_error(collection(c_x, c_y))
  expect_identical(length(collection(change_key(c_x, "a"),
                                     change_key(c_y, "b"))),
                   2L)
  expect_identical(length(collection(change_key(c_x, "b"),
                                     change_key(c_y, "b"))),
                   1L)
})

test_that("Some minor fails", {
  expect_error(namekey(collection_df(data.frame(a = 1, b = 1), key = "a")))
  expect_error(collection(1))
})

test_that("Collection collapse", {
  c_x <- collection_df(data.frame(a = 5:15, b = letters[5:15],
                                  stringsAsFactors = FALSE),
                       key = "a")
  c_y <- collection_df(data.frame(c = 1:11, b = letters[5:15],
                                  stringsAsFactors = FALSE),
                       key = "b")
  cl <- collection(a = c_x, b = c_y)
  names(cl) <- "a"
  expect_identical(length(cl), 2L)
  key(cl) <- list("b", "b")
  expect_identical(length(cl), 1L)
  expect_identical(names(cl), "a")
  expect_identical(key(cl), list("b"))
  cl <- join(cl, collection_df(tibble(b = "e", a = 1, c = 1), key = "b"),
             name = "con")
  expect_error(namekey(cl) <- tibble(name = "", key = list("b")))
  cl <- collection(a = c_x, b = c_y)
  c_z <- collection_df(data.frame(c = 1:11, b = letters[5:15]), key = "c")
})
