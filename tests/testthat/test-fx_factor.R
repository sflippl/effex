context("fx_factor")

test_that("Constructing factors", {
  x <- list("a", 1, "b", "a", 1)
  x_int <- c(1:3, 1:2)
  x_levels <- list("a", 1, "b")
  x_fxf <- as_fx_factor(x_int, x_levels)
  expect_true(is_fx_factor(x_fxf))
  expect_equal(levels(x_fxf), x_levels)
  expect_error(as_fx_factor(x, x_levels))
  expect_error(as_fx_factor(c(x_int, 4), x_levels))
  expect_error(as_fx_factor(x_int, levels = rep(x_levels, 2)))
})

test_that("Inferring factors", {
  x <- list("a", 1, "b", "a", 1)
  x_int <- c(1:3, 1:2)
  x_levels <- list("a", 1, "b")
  expect_equal(fx_factor(x), as_fx_factor(x_int, x_levels))
  expect_error(fx_factor(list(geom_point(), geom_point())))
  x_levels <- list("a", 1, "b", "c")
  expect_equal(fx_factor(x, levels = x_levels), as_fx_factor(x_int, x_levels))
  expect_error(supressWarnings(fx_factor(x, levels = character(0))))
  x_fxf <- fx_factor(x)
  levels(x_fxf) <- x_levels
  expect_equal(fx_factor(x, levels = x_levels), x_fxf)
})

test_that("Evaluating factors", {
  tst <- ggplot2::geom_point()
  fxf <- as_fx_factor(rep(1L, 3), levels = list(tst))
  expect_true(inherits(fx_evaluate(fxf)[[1]]$geom, "GeomPoint"))
  expect_equal(fx_evaluate(1), 1)
  tbl <- dplyr::tibble(
    x = rep(1L:2L, 2),
    fxf = fx_factor(rep(1L:2L, 2))
  )
  expect_equal(fx_evaluate(tbl),
               dplyr::tibble(x = rep(1L:2L, 2), fxf = rep(1:2, 2)))
  expect_true(tbl %>% fx_default() %>% fx_evaluate() %>% has_metaframe())
})

test_that("Subsetting factors", {
  x <- fx_factor(list("a", 1, "b", "a", 1))
  expect_equal(as.integer(x[1:3]), 1:3)
  expect_true(is_fx_factor(x[1:3]))
  expect_equal(x[[1]], "a")
})
