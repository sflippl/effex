context("fx-fun-ggplot-fxGeomSpatial")

test_that("fxGeomSpatial stops with inappropriate aesthetics", {
  expect_error(fxe_layer_single(fxGeom("Spatial"), AesName("x")))
})

test_that("fxGeomSpatial can add a coordinate system.", {
  tst <- fxe_layer_single(fxGeom("Spatial"), AesName("geom"),
                          fxGeom_xlim = c(10,60))
  expect_equal(tst[[1]]$limits[["x"]], c(10, 60))
})

test_that("fxGeomSpatial nominates geom_sf", {
  tst <- fxe_layer_complete_nominate(fxGeom("Spatial"),
                                     AesName("geom"), data.frame())
  expect_true(inherits(nom_layers(tst[[1]])[[1]]$geom, "GeomSf"))
})

test_that("fxGeomSpatial vetoes inappropriate nominations", {
  good <- nomination(ggplot2::geom_sf())
  bad <- nomination(ggplot2::geom_point())
  expect_equal(
    fxe_layer_complete_veto(
      good, fxGeom("Spatial"), AesName("geom"), data.frame()
    ),
    FALSE
  )
  expect_equal(
    fxe_layer_complete_veto(
      bad, fxGeom("Spatial"), AesName("geom"), data.frame()
    ),
    TRUE
  )
})
