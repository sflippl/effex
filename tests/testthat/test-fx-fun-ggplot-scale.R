context("fx-fun-ggplot-scale")

test_that("Basic scales", {
  expect_error(fxe_layer_scale(fxGeom(""), AesName("")))
  sca1 <- fxe_layer_scale(fxGeom(""), AesName("x"), data = data.frame(),
                            fxGeom_title = "Test",
                            fxGeom_limits = c(NA_real_, NA_real_))
  expect_equal(sca1[[1]], ggplot2::labs(x = "Test"))
  expect_equal(fxe_layer_scale(fxGeom(""), AesName("x"), data = data.frame(),
                                fxGeom_title = list(NULL),
                                fxGeom_limits = c(NA_real_, NA_real_))[[1]],
              ggplot2::labs(x = NULL))
})

test_that("Continuous scale", {
  sca <- fxe_layer_scale(fxGeom("Continuous"), AesName("x"), data.frame(),
                           fxGeom_limits = c(0, 1))
  expect_true(inherits(sca[[2]], "Scale"))
  expect_true(inherits(sca[[2]]$range, "Range"))
  expect_error(fxe_layer_scale(fxGeom("Continuous"), AesName("geom"),
                               data.frame(), fxGeom_limits = c(0, 1)))
  data <- dplyr::tibble(column_name = 1:10)
  metaframe(data) <- dplyr::tibble(
    name = "column_name",
    fxGeom_pal.colour_fill = list(scales::brewer_pal())
  ) %>% as_metaframe()
  data <- fx_default(data, columns = fx_ggplot_columns)
  sca <- fxi_layer_single(data, ggplot2::aes(fill = column_name), "fill")
  expect_equal(sca[[2]]$palette(5), scales::brewer_pal()(5))
})

test_that("Discrete scale", {
  sca <- fxe_layer_scale(fxGeom("Discrete"), AesName("x"), data.frame(),
                           fxGeom_limits = LETTERS[1:3])
  expect_true(inherits(sca[[2]], "Scale"))
  expect_true(inherits(sca[[2]]$range, "Range"))
})
