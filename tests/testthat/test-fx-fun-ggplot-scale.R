context("fx-fun-ggplot-scale")

test_that("Basic scales", {
  expect_error(fxext_layer_scale(fxGeom(""), AesName("")))
})
