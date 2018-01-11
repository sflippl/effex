context("Supplementary print functions")

test_that("print_in_lines_input", {
  expect_equal(print_in_lines_input("Content variable Intelligenz",
                                    nchars_per_line = c(1, 2),
                                    nlines = 10,
                                    nchars = NULL),
               c(0, 1, 3, 4, 6, 7, 9, 10, 12, 13, 15))
  expect_error(print_in_lines_input("Content variable Intelligenz",
                                    nchars_per_line = c(1, 2),
                                    nlines = 10,
                                    nchars = 10))
  expect_equal(print_in_lines_input("Content variable Intelligenz"),
               c(0, nchar("Content variable Intelligenz")))
  expect_equal(print_in_lines_input("Content variable Intelligenz",
                                    nchars = 20),
               c(0, 20))
})

test_that("print_in_lines", {
  expect_equal(print_in_lines("print"), "print")
  expect_equal(print_in_lines("Blablablablubblabla",
                              nchars_per_line = 6,
                              nlines = 2,
                              parsesign = ".."),
               "Blabla\nblab..")
  expect_equal(print_in_lines("Blabla",
                              nchars_per_line = 1,
                              nlines = 6,
                              linebreak = ""),
               "Blabla")
})
