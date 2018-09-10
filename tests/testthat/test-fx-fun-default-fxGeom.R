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
  expect_equal(fxe_default(df, mf, fxd("default", "fxGeom_class")), cls)
  metaframe(df) <- mf
  expect_equal(fx_default(df, columns = "fxGeom_class") %>% metaframe,
               dplyr::mutate(mf[1:5, , drop = FALSE],
                             fxGeom_class = cls[1:5]) %>%
                 as_metaframe)
})

test_that("fx_default_fxGeom_limits", {
  df <- dplyr::tibble(
    num = c(1:3, 1:3),
    char = LETTERS[c(1:3, 1:3)],
    fac = factor(LETTERS[c(1:3, 1:3)], levels = LETTERS[1:4]),
    lst = as.list(c(1:3, 1:3))
  )
  tst <- fx_default_fxGeom_limits(df)
  expect_equal(tst,
               list(c(1,3), LETTERS[1:3], LETTERS[1:4], NULL))
})

test_that("fx_default_fxGeom_trans", {
  set.seed(1)
  norm <- rnorm(1000)
  square <- norm^2
  exp <- exp(norm)
  df <- dplyr::tibble(
    norm = rnorm(100),
    square = rnorm(100)^2,
    exp = exp(rnorm(100)),
    chars = rep("a", 100)
  )
  expect_silent(tst <- fx_default_fxGeom_trans(df, fxGeom_trans_simple = FALSE))
  expect_equal(tst[c(1, 4)],
               list("identity", "identity"))
  expect_true(inherits(tst[[2]], "trans"))
  expect_true(inherits(tst[[3]], "trans"))
  tst2 <- fx_default_fxGeom_trans(df, fxGeom_trans_p.threshold = 0,
                                  fxGeom_trans_simple = FALSE)
  expect_equal(unlist(tst2), rep("identity", 4))
  tst3 <- fx_default_fxGeom_trans(df)
  expect_equal(tst3, c("identity", "sqrt", "log10", "identity"))
})

test_that("fx_default_fxGeom_pal", {
  data <- dplyr::tibble(col1 = 1:10, col2 = LETTERS[1:10])
  pal <- fx_default_fxGeom_pal(data, aes_name = "x")
  expect_equal(pal, list(identity, identity))
  pal <- fx_default_fxGeom_pal(data, aes_name = "fill")
  expect_equal(pal,
               list(scales::seq_gradient_pal("#132B43", "#56B1F7", "Lab"),
                    scales::hue_pal()))
  pal <- fx_default_fxGeom_pal(data, aes_name = "fill")
  expect_equal(pal,
               list(scales::seq_gradient_pal("#132B43", "#56B1F7", "Lab"),
                    scales::hue_pal()))
  expect_error(fx_default_fxGeom_pal(data, aes_name = "alpha"))
  pal <- fx_default_fxGeom_pal(data[, 1, drop = FALSE], aes_name = "size")
  expect_equal(pal[[1]], scales::area_pal())
  pal <- fx_default_fxGeom_pal(data[, 1, drop = FALSE], aes_name = "alpha")
  expect_equal(pal[[1]], scales::rescale_pal())
  pal <- fx_default_fxGeom_pal(data[, 2, drop = FALSE], aes_name = "shape")
  expect_equal(pal[[1]], scales::shape_pal())
})
