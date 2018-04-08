context("Architecture.")

test_that("Creating Architectures.", {
  archie <- architecture(verbose = FALSE)
  expect_equal(is_verbose(archie), FALSE)
  expect_equal(is_architecture(archie), TRUE)
  expect_equal(is_architecture(new.env()), FALSE)
  archie <- set_verbose(archie, TRUE)
  expect_equal(is_verbose(archie), TRUE)
  expect_warning(as_architecture(archie, verbose = TRUE))
  expect_error(as_architecture(1, verbose = TRUE))
})
