#' Effex: ggplot
#'
#' This effex lets you build visualization plots that depend on the specified
#' aesthetics dynamically.
#'
#' @param data The data that is visualized.
#' @param indicator The indicators that specify the effex.
#' @param aes_mapping A mapping of [effex_aes()](aesthetics) to indicators. This
#' should work in the same way as the selection mechanism that can for instance
#' be found in [dplyr::select()]. `effex_aes(x = starts_with("v2"))` would
#' therefore be valid.
#' @param ind_names the effex that defines the names of the indicators
#' @param geoms the effex that defines the geoms.
#' @param ... other non-crucial (but possibly important, e. g. description)
#' effex.
#'
#' @export

fx_ggplot <- function(data, indicator, aes_mapping,
                      ind_names, geom, ...) {
  # I will add a preprocessing step of the data here later. This will make it
  # possible to use the metadata and to specify several indicators for one
  # dimension.
  if(is(ind_names, "fxIndName"))
    ind_names <- rlang::eval_tidy(ind_names@ind_name, data = indicator)
  assertthat::assert_that(is.character(ind_names))
  # We only need those indicator rows that are given in the aes_mapping.
  indicator <- dplyr::filter(indicator, ind_names %in% get_inds(aes_mapping))
  ind_names <- ind_names[ind_names %in% get_inds(aes_mapping)]
  p <- ggplot(data, aes_mapping)
  # At first, we get the single add-ons of every dimension:
  for(nam in names(aes_mapping)) {
    p <- fx_layer_single_ind(indicator, nam, aes_mapping[[nam]],
                             ind_names, geom = geom, ...)(p)
  }
  # Then, we get the final layer(s) that are determined by the complete
  # aes_mapping.
  p <- fx_layer_complete_ind(indicator, aes_mapping,
                             ind_names, geom = geom, ...)(p)
  return(p)
}

#' Get the indicator names of an aesthetic
#'
#' This function will later be extended such that it encompasses selection of
#' several indicators.

get_inds <- function(mapping) {
  purrr::map(mapping, ~ as.character(rlang::quo_get_expr(.))) %>%
    purrr::reduce(c)
}

#' @rdname def_fx_layer_single_ind

fx_layer_single_ind <- function(indicator, aes_name, aes_quo, ind_names, ...) {
  fxps <- rlang::dots_list(...)
  if(length(fxps) == 0) return(identity)
  indicator <-
    dplyr::filter(indicator,
                  ind_names == as.character(rlang::quo_get_expr(aes_quo)))
  ind_names <-
    ind_names[ind_names == as.character(rlang::quo_get_expr(aes_quo))]
  if(nrow(indicator) == 0) return(identity)
  assertthat::assert_that(nrow(indicator) == 1)
  # Initiate the deploy mechanism. As there are several arguments, we need S4
  # classes. These are defined in effex-ggplot-s4.R.
  dep_aes_name <- aesName(aes = aes_name)
  f <- function(gg) {
    for(fx_name in names(fxps)) {
      gg <- def_fx_layer_single_ind(dep_aes_name,
                                    fxName(fx = fx_name),
                                    createFx(fxps[[fx_name]],
                                             indicator)[[1]])(gg)
    }
    return(gg)
  }
  return(f)
}

#' Define layers dependent on a single aesthetic
#'
#' These are the layers that are always present if a certain combination of
#' aesthetic and effex element is present. Below are listed all implemented
#' methods in `tectr`:
#'
#' @param aes_name The name of the aesthetic, previously processed by
#' [aesName()] to allow method dispatch.
#' @param fx_name The name of the effex element, previously processsed by
#' [fxName()] to allow method dispatch.
#' @param fx The corresponding fx element.
#' @param indicator The indicator information data frame.
#'
#' @details Method dispatch over both `fx_name` and `fx` might seem like
#' overkill, as they will often possess the identical signature. However, in
#' some cases, a subclass might allow for new processing methods.
#'
#' @return A function that processes a `ggplot` object. Default is the identity
#' function.
#'
#' @export

setGeneric("def_fx_layer_single_ind",
           function(aes_name, fx_name, fx, indicator)
             standardGeneric("def_fx_layer_single_ind"))

# Methods can be found in 'effex-ggplot-layer-single_ind.R'

#' @rdname def_fx_layer_complete_ind

fx_layer_complete_ind <- function(indicator, mapping, ind_names, ...) {
  fxps <- rlang::dots_list(...)
  if(length(fxps) == 0) return(identity)
  f <- function(gg) {
    for(fx_name in names(fxps)) {
      fx_args_tbl <- dplyr::tibble(fx = createFx(fxps[[fx_name]], indicator),
                                   ind_name = ind_names) %>%
        dplyr::inner_join(
          dplyr::tibble(aes = names(mapping), ind_name = get_inds(mapping)),
          by = "ind_name")
      fx_args <- fx_args_tbl$fx
      names(fx_args) <- fx_args_tbl$aes
      f <- do.call("def_fx_layer_complete_ind",
                   rlang::list2(fx_name = fxName(fx = fx_name), !!!fx_args))
      gg <- f(gg)
    }
    return(gg)
  }
  return(f)
}

#' Define layers dependent on all aesthetics
#'
#' These are the layers that depend on all aesthetics. If certain aesthetics are
#' not supplied or aesthetics of another class are given, this changes the
#' result. Due to the limitations of the S4 dispatch method, the aesthetics over
#' which it is possible to dispatch must be supplied in the generic. The
#' implemented methods are listed below:
#'
#' @param fx_name The `fxName` over which to dispatch
#' @param ... other possible parameters. Not available for dispatch but can be
#' used in certain methods.
#'
#' @return A function that processes a `ggplot` object. Default is the identity
#' function.
#'
#' @export

setGeneric("def_fx_layer_complete_ind",
           function(
             fx_name, ...,
             x, y, alpha, colour, fill, group, shape, size, stroke
           ) standardGeneric("def_fx_layer_complete_ind"))

# Methods can be found in 'effex-ggplot-layer-complete_ind.R'
