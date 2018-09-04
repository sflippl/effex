context("effex-ggplot")

test_that("multiplication works", {
  expect_silent(
    fx_ggplot(mtcars, fx_mtcars, aes(x = mpg, y = disp, colour = cyl),
              aes(geoms = geom_standard, labels = description,
                  indicators = indicators)) +
      theme_minimal()
  )
})
