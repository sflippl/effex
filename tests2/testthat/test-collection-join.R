context("collection-join")

test_that("joining collection elements", {
  c_x <- collection_df(data.frame(a = 5:15, b = letters[5:15],
                                  stringsAsFactors = FALSE), "a")
  c_y <- collection_df(data.frame(a = 1:10, b = letters[1:10],
                                  stringsAsFactors = FALSE), "a")
  expect_identical(arrange(join(c_x, c_y), a),
                   collection_df(data.frame(a = 1:15, b = letters[1:15],
                                            stringsAsFactors = FALSE), "a"))
  c_x <- mutate(c_x, b = LETTERS[5:15])
  expect_error(join(c_x, c_y))
  c_x <- mutate(c_x, b = letters[5:15])
  key(c_x) <- NULL
  expect_error(join(c_x, c_y))
  key(c_y) <- NULL
  expect_error(join(c_x, c_y))
  expect_error(join(c_x, 1))
})

test_that("joining collections", {
  c_z <- collection_df(data.frame(c = 1:11, b = letters[5:15]), key = "c")
  c_x <- collection_df(data.frame(a = 5:15, b = letters[5:15],
                                  stringsAsFactors = FALSE),
                       key = "a")
  c_y <- collection_df(data.frame(c = 1:11, b = letters[5:15],
                                  stringsAsFactors = FALSE),
                       key = "b")
  cl <- collection(a = c_x, b = c_y)
  expect_identical(join(cl, collection(b = c_y, c = c_z)),
                   collection(a = c_x, b = c_y, c = c_z))
  expect_identical(join(cl, c_z), join(c_z, cl))
  expect_error(join(cl, 1))
  expect_identical(join(collection(c_z), cl, name = ""),
                   collection(c_z, c_x, c_y))
})
