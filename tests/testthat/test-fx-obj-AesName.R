context("fx-obj-AesName")

test_that("AesName instantiation", {
  expect_true(is(AesName(""), "AesName"))
  expect_true(is(AesName("x"), "xAesName"))
  expect_false(is(AesName("m"), "mAesName"))
  expect_true(is(AesName("m"), "AesName"))
})
