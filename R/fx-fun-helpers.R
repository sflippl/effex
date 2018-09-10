lst_mf_args <- function(mf) {
  rlang::as_list(mf) %>%
    purrr::map(1)
}
