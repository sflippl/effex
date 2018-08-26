context("effex-plot")

inds_mtcars <- dplyr::tibble(
  ind_name = colnames(mtcars),
  var_name = colnames(mtcars),
  description = c(
    "Miles/(US) gallon",
    "Number of cylinders",
    "Displacement (cu.in.)",
    "Gross horsepower",
    "Rear axle ratio",
    "Weight (1000 lbs)",
    "1/4 mile time",
    "Engine (0 = V-shaped, 1 = straight)",
    "Transmission (0 = automatic, 1 = manual)",
    "Number of forward gears",
    "Number of carburetors"
  )
) %>% mutate(
  x_geom = if_else(var_name %in% c("cyl", "vs", "am", "gear", "carb"),
  "bar", "density")
)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
