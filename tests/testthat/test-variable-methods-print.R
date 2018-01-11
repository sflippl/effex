context("Testing the print methods for variables.")

test_that("print.Content works", {
  expect_output(print(Content("Intelligence", "Dab")), "Content Variable Intelligence")
  expect_output(print(Content("Intelligence", "Dab",
                              "1" = "1", "2" = "2", "3" = "3",
                              "4" = "4", "5" = "5", "6" = "6")),
                "Printed 5 out of 6 features.")
})
