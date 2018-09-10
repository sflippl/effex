context("fx-obj-infer")

test_that("fxInfer objects are valid", {
  ex <- new_FxInfer(subclass = "fxGeom_class")
  expect_equal(class(ex), c("fxGeom_classFxInfer", "FxInfer"))
  expect_equal(subclass(ex), "fxGeom_class")
  expect_error(new_FxInfer(subclass = LETTERS))
  expect_error(
    subclass(structure(list(), class = paste0(c(LETTERS, ""), "FxInfer")))
  )
  expect_error(subclass(list()))
  ex2 <- new_FxInfer()
  expect_equal(subclass(ex2), NULL)
})
