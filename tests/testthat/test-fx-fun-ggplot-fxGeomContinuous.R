context("fx-fun-ggplot-fxGeomContinuous")

test_that("Vetos and votes have the correct classes", {
  noms <- list(nomination(ggplot2::geom_point()),
               nomination(ggplot2::geom_bar()),
               nomination(ggplot2::geom_boxplot()))
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.logical(
        fxe_layer_complete_veto(nom, fxGeom("Continuous"), AesName("x"))
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.logical(
        fxe_layer_complete_veto(nom, fxGeom("Continuous"), AesName("y"))
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.numeric(
        fxe_layer_complete_vote(nom, fxGeom("Continuous"), AesName("x"))
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.numeric(
        fxe_layer_complete_vote(nom, fxGeom("Continuous"), AesName("y"))
      ))
  )
})
