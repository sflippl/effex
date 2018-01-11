test_that("is_variable() works", {
  expect_equal(is_variable(Content("Intelligence")), TRUE)
  expect_equal(is_variable(2), FALSE)
  expect_equal(is_content(Content("Intelligence")), TRUE)
  expect_equal(is_content(2), FALSE)
  expect_warning({
    x <- 2
    class(x) <- "content"
    is_content(x)
  })
})
