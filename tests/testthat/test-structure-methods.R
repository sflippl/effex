context("Structure Methods")

test_that("rbind_newcol() works", {
  expect_output(colnames(ncbind_rows(data.frame(a = 1), data.frame(b = 1))),
                c("a", "b"))
  expect_output(rbind_newcol(data.frame(a = 1), data.frame(b = 1)),
                rbind(data.frame(a = 1), data.frame(b = 1)))
})
