library(tidyverse)

fx_diamonds <- tibble(
  ind_name = colnames(diamonds),
  var_name = colnames(diamonds),
  label = c(
    "Weight in Carat",
    "Quality of the Cut",
    "Colour",
    "Clarity",
    "Total Depth Percentage",
    "Width of top relative to the widest point",
    "Price (US Dollars)",
    "Length in x-direction (mm)",
    "Length in y-direction (mm)",
    "Length in z-direction (mm)"
  )
) %>% dplyr::mutate(
  class = dplyr::if_else(
    ind_name %in% c("price", "x", "y", "z", "depth", "table", "carat"),
    "continuous", "discrete"
  ),
  limits = purrr::map2(ind_name, class, function(x, y) {
    if(y == "continuous") return(range(diamonds[[x]]))
    else return(sort(levels(diamonds[[x]])))
  })
)

f_fxpGeom <- function(indicator_row) {
  if(indicator_row$class == "continuous") {
    ret <- fxContinuousGeom(limits = indicator_row$limits[[1]])
  }
  else ret <- fxDiscreteGeom(limits = indicator_row$limits[[1]])
  ret
}

fxpGeom <- fxpGeom(f_fxpGeom_mtcars)

devtools::use_data(fx_diamonds, fxpGeom, overwrite = TRUE)

