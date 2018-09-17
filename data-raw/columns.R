fx_info_columns <- c(
  "fxInfo_name"
)

devtools::use_data(fx_info_columns, overwrite = TRUE)

fx_ggplot_columns <- c(
  "fxGeom_class",
  "fxGeom_limits",
  "fxGeom_trans",
  fx_info_columns
)

devtools::use_data(fx_ggplot_columns, overwrite = TRUE)
