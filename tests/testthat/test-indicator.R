context("indicator")

test_that("Form indicators", {
  expect_equal(indicator(name = "gdp_growth", source = "vdem"),
               structure(list(structure("gdp_growth",
                                        class = c("vdem", "indicator_el",
                                                  class("gdp_growth")))),
                         class = c("indicator", class(list()))))
  expect_equal(indicator(name = c("gdp_growth",
                                  "population"),
                         source = "vdem"),
               structure(list(structure(c("gdp_growth", "population"),
                                        class = c("vdem", "indicator_el",
                                                  "character"))),
                         class = c("indicator", class(list()))))
  expect_equal(length(indicator(name = c("gdp_growth", "population"),
                                source = c("vdem", "worldbank"))),
               2)
  expect_error(indicator(name = c("gdp_growth", "population", "population"),
                         source = c("vdem", "worldbank")))
  expect_equal(indicator(NULL),
               structure(list(), class = c("indicator", "list")))
  expect_equal(indicator(df = data.frame(
    name = as.character("gdp_growth"),
    source = as.character("vdem"),
    file = as.character("this"),
    stringsAsFactors = FALSE)),
               indicator(name = "gdp_growth", source = "vdem", file = "this"))
  expect_error(indicator(name = "gdp_growth", df = tibble(source = "vdem")))
  expect_equal(indicator(name = "gdp_growth", source = "vdem",
                         file = c("this", "that")),
               structure(list(structure("gdp_growth",
                                        class = c("vdem", "indicator_el",
                                                  "character"),
                                        file = "this"),
                              structure("gdp_growth",
                                        class = c("vdem", "indicator_el",
                                                  "character"),
                                        file = "that")),
                         class = c("indicator", "list")))
})

test_that("Coerce Indicators", {
  expect_error(as_indicator(1))
  expect_equal(as_indicator(name = "gdp_growth", source = "vdem"),
               indicator(name = "gdp_growth", source = "vdem"))
  expect_equal(as_indicator("vdem-gdp_growth"),
               indicator(name = "gdp_growth", source = "vdem"))
  expect_equal(as_indicator("vdem-gdp__growth"),
               indicator(name = "gdp__growth", source = "vdem"))
  expect_equal(as_indicator("vdem__gdp__growth"),
               indicator(name = "gdp__growth", source = "vdem"))
  expect_equal(as_indicator(c("vdem__population", "population"),
                            file = c("this", "that")),
               indicator(name = "population", source = c("vdem", ""),
                         file = c("this", "that")))
})

test_that("Indicator Sources", {
  expect_equal(ind_source(as_indicator("vdem-gdp_growth")), "vdem")
  expect_equal(ind_source(as_indicator(c("gdp_growth",
                                         "vdem-gdp_growth",
                                         "vdem-population"))),
               c("composite", "vdem"))
})
