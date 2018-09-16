context("fx-fun-ggplot")
library(ggplot2)

test_that("fx_ggplot works", {
  df <- dplyr::tibble(column_name = 1:10)
  mf <- new_metaframe(name = "column_name",
                      fxGeom_class = "")
  metaframe(df) <- dplyr::mutate(mf,
                                 fxGeom_nominations = list(list(
                                   nomination(geom_bar())))) %>%
    as_metaframe
  expect_silent(fx_ggplot(df, aes(x = column_name)))
})

test_that("fxi_layer_single works", {
  df <- data.frame(column_name = 1:10) %>%
    fx_default(columns = fx_ggplot_columns)
  expect_equal(class(fxi_layer_single(data = df, aes(x = column_name), "x")),
               "list")
})

test_that("fxe_layer_single works", {
  expect_error(fxe_layer_single(1, 2))
})

test_that("fxi_layer_complete_nominate", {
  expect_equal(fxi_layer_complete_nominate(data = data.frame(),
                                             mf = data.frame(
                                               aes = character(0),
                                               name = character(0),
                                               fxGeom_class = character(0)
                                             )), list())
  nom1 <- list(
    nomination(geom_point(), geom_line())
  )
  expect_equal(
    fxi_layer_complete_nominate(
      data = data.frame(),
      mf = dplyr::tibble(
        aes = "x",
        name = "column_name",
        fxGeom_class = "",
        fxGeom_nominations = list(nom1)
      )
    ), nom1
  )
  expect_error(fxi_layer_complete_nominate(
    data = data.frame(),
    mf = dplyr::tibble(aes = "x", name = "column_name", fxGeom_class = "",
                       fxGeom_nominations = list(1))
  ))
})

test_that("fxi_layer_complete_vote", {
  expect_error(fxi_layer_complete_vote(nominations = list(),
                                         mf = data.frame(
                                           aes = character(0),
                                           name = character(0),
                                           fxGeom_class = character(0))))
  nom1 <- list(
    nomination(geom_point(), geom_line())
  )
  expect_equal(
    fxi_layer_complete_vote(
      nominations = list(nom1),
      mf = dplyr::tibble(aes = "x",
                         name = "column_name",
                         fxGeom_class = "",
                         fxGeom_nominations = list(nom1)),
      data = data.frame(column_name = 1:10)
    ), nom1
  )
  nom2 <- list(
    nomination(geom_point()),
    nomination(geom_line())
  )
  fxGeom_veto <- function(nomination, ...)
    any(purrr::map_lgl(nom_layers(nomination), ~ inherits(.$geom, "GeomLine")))
  fxGeom_vote <- function(nomination, ...) {
    if(any(purrr::map_lgl(nom_layers(nomination), ~ inherits(.$geom, "GeomLine"))))
      return(1)
    return(0)
  }
  expect_equal(
    fxi_layer_complete_vote(
      nominations = nom2,
      mf = dplyr::tibble(aes = "x", name = "column_name", fxGeom_class = "",
                         fxGeom_veto = list(fxGeom_veto)),
      data = data.frame(column_name = 1:10)
    ), nom2[[1]]
  )
  expect_equal(
    fxi_layer_complete_vote(
      nominations = nom2,
      mf = dplyr::tibble(aes = "x", name = "column_name", fxGeom_class = "",
                         fxGeom_vote = list(fxGeom_vote)),
      data = data.frame(column_name = 1:10)
    ), nom2[[2]]
  )
  expect_equal(
    fxi_layer_complete_vote(
      nominations = nom2,
      mf = dplyr::tibble(aes = "x", name = "column_name", fxGeom_class = "",
                        fxGeom_veto = list(fxGeom_veto),
                        fxGeom_vote = list(fxGeom_vote)),
      data = data.frame(column_name = 1:10)
    ), nom2[[1]]
  )
})

test_that("fxi_layer_complete works", {
  df <- dplyr::tibble(column_name = 1:10)
  mf <- new_metaframe(name = "column_name",
                      fxGeom_class = "")
  metaframe(df) <- mf
  expect_error(fxi_layer_complete(df, aes(x = column_name)))
})

test_that("fxe_layer_complete_nominate works", {
  expect_identical(fxe_layer_complete_nominate(fxGeom(""), AesName("x")),
                   list())
  expect_error(fxe_layer_complete_nominate(fxGeom(""), AesName("x"),
                                             fxGeom_nominations = 1))
})

test_that("fxi_labeller works", {
  expect_error(
    ex <-
      data.frame(column_name = 1:10) %>%
      fx_default(columns = "fxGeom_class") %>%
      fxi_labeller(vars(column_name))
  )
  ex <-
    data.frame(column_name = LETTERS[1:10], stringsAsFactors = FALSE) %>%
    fx_default(columns = fx_ggplot_columns) %>%
    fxi_labeller(vars(column_name))
  expect_equal(ex(data.frame(column_name = "C"))$column_name, "C")
  ex <-
    data.frame(column_name = LETTERS[1:10]) %>%
    fx_default(columns = c("fxGeom_class", "fxGeom_limits")) %>%
    mutate_mf(fxGeom_breaks = list(letters[1:10])) %>%
    fxi_labeller(vars(column_name))
  expect_equal(
    ex(data.frame(column_name = "C"))$column_name,
    "c")
})
