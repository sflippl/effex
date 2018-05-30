context("variable")

test_that("variable initiation works", {
  expect_equal(variable(),
               structure(tibble(), class = c("variable", class(tibble()))))
  expect_error(variable(id = NULL, a = 1:3))
  tst <- variable(id = 1:10, square = (1:10)^2)
  expect_true(is_id(tst$id))
  expect_false(is_id(tst$square))
  expect_true(inherits(tst$square, "square"))
  expect_equal(ids(tst), "id")
  tst2 <- variable(id = tibble(family = 1:10,
                               role = rep(c("daughter", "son"), 5)),
                   age = 11:20,
                   income = c(rep(0, 5), rep(-1000, 3), rep(1000, 2)))
  expect_true(is_id(tst2$family))
  expect_equal(ids(tst2), c("family", "role"))
  expect_false(is_id(tst2$age))
})
