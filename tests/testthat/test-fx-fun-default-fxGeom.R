context("fx-fun-default-fxGeom")

test_that("fx_default_fxGeom_class", {
  requireNamespace("sf", quietly = TRUE)
  df <- dplyr::tibble(
    Cont = 1:10,
    Disc = LETTERS[1:10],
    Disc2 = factor(LETTERS[1:10]),
    List = as.list(1:10),
    Spat = sf::st_sfc(rep(list(NULL), 10))
  )
  mf <- new_metaframe(name = c(names(df), "no_column"))
  cls <- fx_default_fxGeom_class(df, mf)
  expect_equal(cls, c("Continuous", "Discrete", "Discrete", "", "Spatial", ""))
  expect_equal(fxext_default(df, mf, fxd("default", "fxGeom_class")), cls)
  metaframe(df) <- mf
  expect_equal(fx_default(df, columns = "fxGeom_class") %>% metaframe,
               dplyr::mutate(mf[1:5, , drop = FALSE],
                             fxGeom_class = cls[1:5]) %>%
                 as_metaframe)
})
