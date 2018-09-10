context("fxext-layer-complete")

test_that("nominate", {
  nom <- fxext_layer_complete_nominate(fxGeom("Continuous"), AesName("x"),
                                       data.frame())
  purrr::walk(nom, ~ expect_true(is_nomination(.)))
  nom <- fxext_layer_complete_nominate(fxGeom("Continuous"), AesName("y"),
                                       data.frame())
  purrr::walk(nom, ~ expect_true(is_nomination(.)))
  nom <- fxext_layer_complete_nominate(fxGeom("Discrete"), AesName("x"),
                                       data.frame())
  purrr::walk(nom, ~ expect_true(is_nomination(.)))
  nom <- fxext_layer_complete_nominate(fxGeom("Discrete"), AesName("y"),
                                       data.frame())
  purrr::walk(nom, ~ expect_true(is_nomination(.)))
})
