context("fx-fun-ggplot-fxGeomContinuous")

test_that("Vetos and votes have the correct classes", {
  noms <- list(nomination(ggplot2::geom_point()),
               nomination(ggplot2::geom_bar()),
               nomination(ggplot2::geom_boxplot()))
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.logical(
        fxe_layer_complete_veto(nom, fxGeom("Continuous"), AesName("x"),
                                data.frame())
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.logical(
        fxe_layer_complete_veto(nom, fxGeom("Continuous"), AesName("y"),
                                data.frame())
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.numeric(
        fxe_layer_complete_vote(nom, fxGeom("Continuous"), AesName("x"),
                                data.frame())
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.numeric(
        fxe_layer_complete_vote(nom, fxGeom("Continuous"), AesName("y"),
                                data.frame())
      ))
  )
})

test_that("get_alpha works", {
  expect_equal(get_alpha(NULL, NULL, NULL, 3000), 0.525)
  expect_equal(get_alpha(NULL, NULL, NULL, 1000), 1)
  expect_equal(get_alpha(NULL, NULL, NULL, 1), 1)
  expect_equal(get_alpha(NULL, NULL, NULL, 5000), 0.05 + 0.95 * 0.25)
})

test_that("colour and fill veto correctly", {
  expect_true(fxe_layer_complete_veto(
    nomination(ggplot2::geom_density2d()),
               fxGeom("Continuous"), AesName("colour"), data.frame()
  ))
  expect_true(fxe_layer_complete_veto(
    nomination(ggplot2::geom_density2d()),
               fxGeom("Continuous"), AesName("fill"), data.frame()
  ))
})
