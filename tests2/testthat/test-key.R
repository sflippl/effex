context("key")

test_that("Building keys", {
  expect_equal(new_key(), structure(list(), class = c("key", "list")))
  expect_error(new_key(1))
  expect_equal(length(new_key(c("a"), c("b"), c("a", "b"))), 3)
})

test_that("Key to char and char to key", {
  expect_equal(key_to_char(new_key(c("a", "b"))), "a, b")
  expect_equal(key_to_char(new_key(NULL)), "")
  expect_equal(char_to_key(""), new_key(NULL))
  expect_true(char_to_key(key_to_char(new_key(c("a", "b")))) ==
               new_key(c("a", "b")))
})
