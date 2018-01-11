context("Testing the print methods for variables.")

test_that("print.Content works", {
  expect_output(print(Content("Intelligence", "Dab")), "content Intelligence")
  expect_output(print(Content("Intelligence", "Dab",
                              "1" = "1", "2" = "2", "3" = "3",
                              "4" = "4", "5" = "5", "6" = "6")),
                "Printed 5 out of 6 features.")
  expect_output(print(Content(c("Intelligence", "Arrogance"),
                              c("Dab", "Hating Dabs")), parse = 1),
                "Printed 1 out of 2 contents")
})
