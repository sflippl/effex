context("Test structure class")

test_that("Correct creation", {
  expect_warning(Structure(Content("Intelligence"),
                           C_Relation("predicts"),
                           edges = data.frame(from = 1, to = 1)))
  expect_output(Structure(Content("Intelligence"),
                          C_Relation("predicts"),
                          edges = data.frame(from = 1, to = 1,
                                             name = "predict")),
                "Edge Data: 1 x 3")
  expect_error(Structure(Content("Intelligence"),
                         NULL, NULL))
})
