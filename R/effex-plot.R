#' Effex: Plot
#'
#' This effex allows you to specify a mapping and the details get filled
#' in automatically by the indicator.
#'
#' @param data
#' @param indicator
#' @param metadata Defunct for now.
#' @param mapping Which dimensions do you wish to visualize by which indicators?
#'
#' @export

effex_plot <- function(data, indicator, metadata = NULL, mapping) {
  assertthat::assert_that(inherits(mapping, "uneval"))
  indicator <- filter_indicator(indicator,
                                purrr::map_chr(mapping,
                                               ~as.character(
                                                 rlang::quo_get_expr(.))))
  data <- dplyr::select(data, !!indicator[["var_name"]])
  eval(
    parse(text = paste("ggplot(data) +", effex_plot_spec(indicator, mapping)))
  )
}

effex_plot_spec <- function(indicator, mapping) {
  x_name <- as.character(rlang::quo_get_expr(mapping[["x"]]))
  x_geom <- dplyr::filter(indicator, ind_name == x_name) %>%
    magrittr::extract2("x_geom")
  x_label <- dplyr::filter(indicator, ind_name == x_name) %>%
    magrittr::extract2("description")
  glue::glue("geom_{x_geom}(aes(x = {x_name})) + xlab(\"{x_label}\")")
}

filter_indicator <- function(indicator, var_names) {
  ind_names <- dplyr::filter(indicator, var_name %in% var_names) %>%
    magrittr::extract2("ind_name") %>%
    unique
  dplyr::filter(indicator, ind_name %in% ind_names)
}
