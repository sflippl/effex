context("fx-fun-ggplot.R")
library(ggplot2)

test_that("fx_ggplot works", {
  df <- dplyr::tibble(column_name = 1:10)
  mf <- new_metaframe(name = "column_name",
                      fxGeom_class = "")
  metaframe(df) <- dplyr::mutate(mf,
                                 fxGeom_nominations = list(
                                   dplyr::tibble(geom = list("bar"),
                                                 position = list("stack"),
                                                 stat = list("bin"),
                                                 params = list(list(bins = 5))))) %>%
    as_metaframe
  expect_silent(fx_ggplot(df, aes(x = column_name)))
})

test_that("fxint_layer_single works", {
  df <- data.frame(column_name = 1:10)
  mf <- new_metaframe(name = "column_name",
                      label = "Column Name",
                      fxGeom_class = "")
  metaframe(df) <- mf
  expect_equal(class(fxint_layer_single(df, aes(x = column_name), "x")),
               "function")
})

test_that("fxext_layer_single works", {
  expect_error(fxext_layer_single(1, 2))
  expect_identical(fxext_layer_single(fxGeom(), AesName("")), identity)
})

test_that("fxint_layer_complete works", {
  df <- dplyr::tibble(column_name = 1:10)
  mf <- new_metaframe(name = "column_name",
                      fxGeom_class = "")
  metaframe(df) <- mf
  expect_error(fxint_layer_complete(df, aes(x = column_name)))
  metaframe(df) <- dplyr::mutate(mf,
      fxGeom_nominations = list(
        dplyr::tibble(geom = list("bar"),
                      position = list("stack"),
                      stat = list("bin"),
                      params = list(list(bins = 5))))) %>%
    as_metaframe
  lay <- ggplot2::layer(geom = "bar", position = "stack", stat = "bin",
                        params = list(bins = 5))
  expect_equal(fxint_layer_complete(df, aes(x = column_name)),
               function(p) p + lay)
})

test_that("fxext_layer_complete_nominate works", {
  expect_identical(fxext_layer_complete_nominate(fxGeom(""), AesName("x")),
                   dplyr::tibble(geom = list(), stat = list(),
                                 position = list(), params = list()))
  expect_error(fxext_layer_complete_nominate(fxGeom(""), AesName("x"),
                                             fxGeom_nominations = data.frame()))
  expect_error(fxext_layer_complete_nominate(fxGeom(""), AesName("x"),
    fxGeom_nominations =
      dplyr::tibble(geom = list(), stat = list(), position = list(),
                  additional = list())))
})

test_that("fxext_layer_complete_vote works", {
  geoms <- dplyr::tibble(geom = list("point", "histogram"),
                         stat = list("identity"),
                         position = list("identity"),
                         params = rep(list(list()), 2))
  fxGeom_vetos <- function(geoms, ...)
    dplyr::filter(geoms,
                  purrr::map_lgl(geom, ~ . != "histogram"))
  expect_identical(
    fxext_layer_complete_vote(geoms, fxGeom(""), AesName("x"),
                              fxGeom_vetos = fxGeom_vetos),
    dplyr::tibble(geom = list("point"),
                  stat = list("identity"),
                  position = list("identity"),
                  params = list(list()))
  )
})
