context("Set and get architectures.")

library(rlang)

test_that("Changing architectures works.", {
  expect_equal(get_architecture(), NULL)
  arc1 <- architecture(verbose = TRUE)
  expect_message(set_architecture(arc1), "arc1")
  expect_equal(get_architecture(), expr(arc1))
  arc2 <- architecture(verbose = FALSE)
  expect_equal(set_architecture(arc2), arc2)
  expect_equal(eval(get_architecture()), arc1)
  expect_equal(set_architecture(), NULL)
})

test_that("We cannot set improper objects.", {
  expect_error(set_architecture(1))
  env <- new.env()
  expect_error(set_architecture(env))
  expect_error(set_architecture(architecture(verbose = TRUE)))
})
