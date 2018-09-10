context("fx-fun-ggplot-fxGeomDiscrete")

test_that("Vetos and votes have the correct classes", {
  noms <- list(nomination(ggplot2::geom_point()),
               nomination(ggplot2::geom_bar()),
               nomination(ggplot2::geom_boxplot()))
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.logical(
        fxe_layer_complete_veto(nom, fxGeom("Discrete"), AesName("x"))
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.logical(
        fxe_layer_complete_veto(nom, fxGeom("Discrete"), AesName("y"))
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.numeric(
        fxe_layer_complete_vote(nom, fxGeom("Discrete"), AesName("x"))
      ))
  )
  purrr::walk(
    noms,
    function(nom)
      expect_true(is.numeric(
        fxe_layer_complete_vote(nom, fxGeom("Discrete"), AesName("y"))
      ))
  )
})
