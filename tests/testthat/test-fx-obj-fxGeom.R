context("fx-obj-fxGeom")

test_that("fxGeom instantiation", {
  expect_true(is(fxGeom(), "fxGeom"))
  expect_false(is(fxGeom(), "fxGeomContinuous"))
  expect_true(is(fxGeom("Continuous"), "fxGeomContinuous"))
  expect_error(fxGeom("x"))
})
