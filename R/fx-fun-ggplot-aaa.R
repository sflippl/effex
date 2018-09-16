#' Effex Function: Visualization
#'
#' This function creates an effectively and explicitly specified visualization
#' of several variables in line with the syntax of [ggplot2].
#'
#' @section Required metaframe columns:
#'
#' * `fxGeom_class`: What class does the geometry have?
#' * `fxGeom_limits`: The range of the variables
#' * `fxGeom_trans`: The scale transformation
#' * `fxInfo_name`: The name of the variable
#'
#' @section Extension Mechanism:
#'
#' Extensions may be defined over the following functions:
#'
#' * [fxe_layer_single()]: This function adds independent elements whose nature
#' only depends on a single aesthetic to the plot. Usually, it is split up into:
#'     + [fxe_layer_scale()]: This function explicitly changes the independent
#'     scale element.
#'     + [fxe_layer_single_other()]: This function adds any other independent
#'     plot elements.
#' * the sub-functions of [fxi_layer_complete()]: This function implements a
#' voting system to
#' determine those plot elements which depend on all aesthetics.
#' * [fxe_labeller()]: This function returns the [ggplot2::labeller()] for a
#' possible facetting variable. It is again an independent plot element.
#'
#' @param data A dataframe with [metaframe()] as an attribute
#' @param mapping An aesthetic mapping
#' @param facet_vars Possibly variables, given via [ggplot2::vars()], to
#' facet after
#' @param ... Parameters to give on to [fx_default()]
#'
#' @examples
#' fx_ggplot(mtcars, aes(x = mpg))
#' fx_ggplot(diamonds, aes(x = carat, y = price))
#'
#' @export

fx_ggplot <- function(data, mapping, facet_vars = vars(), ...) {
  data <-
    data %>%
    fx_default(columns = fx_ggplot_columns, ...) %>%
    fx_evaluate()
  layers <- c(
    # At first, we add the geometric information along one dimension
    purrr::map(names(mapping), ~ fxi_layer_single(data, mapping, .)) %>%
      unlist(recursive = FALSE),
    # Then, we add the geometric information that is dependent on all dimensions
    fxi_layer_complete(data, mapping),
    if(length(facet_vars) != 0)
      facet_wrap(facet_vars, labeller = fxi_labeller(data, facet_vars))
    else NULL
  )
  ggplot(data, mapping) + layers
}

fxi_layer_single <- function(data, mapping, nam) {
  mf <- metaframe(data) %>%
    dplyr::filter(name == as.character(rlang::quo_get_expr(mapping[[nam]])))
  assertthat::assert_that(nrow(mf) == 1, "fxGeom_class" %in% colnames(mf))
  fx_geom <- fxGeom(mf[["fxGeom_class"]])
  aes_name <- AesName(nam)
  mf <- lst_mf_args(mf)
  do.call(
    fxe_layer_single,
    rlang::list2(fx_geom, aes_name, data = data, rlang::splice(mf))
  )
}

#' Effex Extendibles in `fx-ggplot`: geometric information along one dimension
#'
#' This is the extendible function for the independent elements. It is
#' dispatched along the S4 class arguments `fx_geom` and `aes_name`. Arbitrary
#' other parameters may
#' be added that should then be expected in the metaframe. It should always be
#' considered a possibility that these values are missing.
#'
#' @param fx_geom An S4 class argument which inherits from [fxGeom-class].
#' @param aes_name An S4 class argument which inherits from [AesName-class].
#'
#' @export

setGeneric(
  "fxe_layer_single",
  function(fx_geom, aes_name, data, ...) standardGeneric("fxe_layer_single")
)

#' @export
#'
#' @param fx_geom the [fxGeom()] object
#' @param aes_name the [aesName()] object
#'
#' @describeIn fxe_layer_single The standard mechanism simply adds the result of
#' [fxe_layer_scale()] and [fxe_layer_single_other()] together.

setMethod(
  "fxe_layer_single",
  signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
  function(fx_geom, aes_name, data, ...) {
    args <- rlang::list2(fx_geom = fx_geom, aes_name = aes_name, data = data,
                         ...)
    c(do.call("fxe_layer_scale", args), do.call("fxe_layer_other", args))
  }
)

#' Effex Internals in `fx_ggplot`: combine all different dimensions
#'
#' This function generates the function to add an arbitrary number of elements
#' which depend on all specified layers.
#'
#' @section Voting System:
#' This function employs a *voting system* to determine a suitable
#' [nomination()] of elements.
#'
#' * [fxe_layer_nominate()] gathers nominations
#' * [fxe_layer_veto()] determines the vetos
#' * [fxe_layer_vote()] gathers the votes
#'
#' You can extend all three of these functions by S4 dispatch over the aesthetic
#' and the geometry class.
#'
#' @param data the data with the metaframe
#' @param mapping the aesthetic mapping

fxi_layer_complete <- function(data, mapping) {
  mf <- metaframe(data) %>%
    dplyr::mutate(name = as.character(name)) %>%
    dplyr::inner_join(
      dplyr::tibble(aes = names(mapping), name = get_inds(mapping)),
      by = "name")
  assertthat::assert_that("fxGeom_class" %in% colnames(mf))
  nominations <- fxi_layer_complete_nominate(data, mf)
  # Remove layers without the required aesthetics - not possible for now.
  # Removing duplicates does not make sense because every layer is a reference
  # class and therefore unique.
  # Veto the inapplicable nominees and give the other nominees the corresponding
  # votes.
  chosen_nomination <- fxi_layer_complete_vote(nominations, mf, data) %>%
    unlist(recursive = FALSE)
  chosen_nomination
}

fxi_layer_complete_nominate <- function(data, mf) {
  lst_fxGeom <- purrr::map(mf[["fxGeom_class"]], fxGeom)
  lst_aesName <- purrr::map(mf[["aes"]], AesName)
  nominations <- list()
  for(i in seq_len(nrow(mf))) {
    # Prepare any other arguments for splicing
    mf_args <- dplyr::select(mf[i, ], -aes) %>% lst_mf_args()
    # Let each aesthetic nominate its potential candidates
    new_nominations <- do.call("fxe_layer_complete_nominate",
                              rlang::list2(fx_geom = lst_fxGeom[[i]],
                                           aes_name = lst_aesName[[i]],
                                           data = data,
                                           !!!mf_args))
    purrr::walk(new_nominations, ~ assertthat::assert_that(is_nomination(.)))
    nominations <- c(nominations, new_nominations)
  }
  nominations
}

fxi_layer_complete_vote <- function(nominations, mf, data) {
  lst_fxGeom <- purrr::map(mf[["fxGeom_class"]], fxGeom)
  lst_aesName <- purrr::map(mf[["aes"]], AesName)
  assertthat::assert_that(length(nominations) != 0,
                          msg = "No nominations have been provided.")

  # Vetoes ----

  lgl_veto <- FALSE
  for(i in seq_len(nrow(mf))) {
    mf_args <- dplyr::select(mf[i, ], -aes) %>%
      rlang::as_list(.) %>%
      purrr::map(1)
    lgl_veto <- lgl_veto |
      purrr::map_lgl(
      nominations,
      ~ do.call("fxe_layer_complete_veto",
                rlang::list2(nomination = ., fx_geom = lst_fxGeom[[i]],
                             aes_name = lst_aesName[[i]], data = data,
                             !!!mf_args))
    )
  }
  nominations <- nominations[!lgl_veto]

  assertthat::assert_that(length(nominations) != 0,
                          msg = "Aesthetics incompatible.")

  # Votes ----

  votes <- rep(0, length(nominations))

  for(i in seq_len(nrow(mf))) {
    mf_args <- dplyr::select(mf[i, ], -aes) %>%
      rlang::as_list(.) %>%
      purrr::map(1)
    votes <- votes +
      purrr::map_dbl(
        nominations,
        ~ do.call("fxe_layer_complete_vote",
                  rlang::list2(nomination = ., fx_geom = lst_fxGeom[[i]],
                               aes_name = lst_aesName[[i]], data = data,
                               !!!mf_args))
      )
  }
  max_votes <- which(votes == max(votes))
  max_vote <- max_votes[1]
  nominations[[max_vote]]
}

#' Get names from a mapping

get_inds <- function(x) {
  purrr::map_chr(x, ~ as.character(rlang::quo_get_expr(.)))
}

#' Effex Extendibles in `fx_ggplot`: nominate layers
#'
#' @inheritParams fxe_layer_single
#'
#' This function returns element [nomination()]s for a combination of a geometry
#' class and an aesthetic.
#'
#' @seealso [fxi_layer_complete()]
#'
#' @export

setGeneric("fxe_layer_complete_nominate",
           function(fx_geom, aes_name, data, ...)
             standardGeneric("fxe_layer_complete_nominate"))

#' @export
#'
#' @param fxGeom_nominations Allows nominating additional elements.
#'
#' @describeIn fxe_layer_complete_nominate returns the list of all nomination
#' which have been specified manually within the metaframe. If the argument is
#' `NULL`, the function returns an empty list.

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            if(is.null(fxGeom_nominations))
              fxGeom_nominations <- list()
            assertthat::assert_that(is.list(fxGeom_nominations))
            purrr::walk(fxGeom_nominations, is_nomination)
            fxGeom_nominations
          })

#' @rdname fxe_layer_complete_nominate
#'
#' @param nomination the [nomination()] which is to be considered for a veto
#' @inheritParams fxe_layer_single
#'
#' @return
#' `fxe_layer_complete_veto` returns a logical value that indicates whether
#' this layer should be vetoed, i. e. removed from the nominations list.
#'
#' @export

setGeneric("fxe_layer_complete_veto",
           function(nomination, fx_geom, aes_name, data, ...)
             standardGeneric("fxe_layer_complete_veto"))

#' @export fxGeom_veto either `NULL` (the default) or a function which returns
#' whether this element should be vetoed.
#'
#' @param fxGeom_veto either `NULL` or a function which vetoes nominations by
#' returning a boolean for a given nomination.
#'
#' @describeIn fxe_layer_complete_veto returns either `FALSE` or, if supplied
#' with `fxGeom_veto` from the metaframe column, it applies that function.

setMethod("fxe_layer_complete_veto",
          signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_veto = NULL) {
            if(is.null(fxGeom_veto)) return(FALSE)
            assertthat::assert_that(is.function(fxGeom_veto))
            fxGeom_veto(nomination = nomination, aes_name = aes_name,
                        ...)
          })

#' Effex Extendibles in `fx_ggplot`: Vote for the nominations
#'
#' This function gathers the votes for the different nomination.
#'
#' @return A number which represents the amount of votes for a certain
#' nomination.
#'
#' @inheritParams fxe_layer_complete_veto
#'
#' @export

setGeneric("fxe_layer_complete_vote",
           function(nomination, fx_geom, aes_name, data, ...)
             standardGeneric("fxe_layer_complete_vote"))

#' @param fxGeom_vote Either `NULL` (the default) or a function which returns
#' the votes as a numerical value for a given nomination.
#'
#' @describeIn fxe_layer_complete_vote applies supplied functions to determine
#' additional votes
#'
#' @export

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
          function(nomination, fx_geom, aes_name, data, ...,
                   fxGeom_vote = NULL) {
            if(is.function(fxGeom_vote))
              return(fxGeom_vote(nomination, fx_geom = fx_geom,
                                  aes_name = aes_name, data = data))
            assertthat::assert_that(
              is.null(fxGeom_vote),
              msg = "fxGeom_vote must by type NULL or function."
            )
            0
          })

fxi_labeller <- function(data, facet_vars) {
  vars <- get_inds(facet_vars)
  lst <-
    vars %>%
    purrr::map(
      function(var) {
        mf <-
          metaframe(data) %>%
          dplyr::filter(name == var)
        cls <- mf$fxGeom_class
        fx_geom <- fxGeom(cls)
        mf_args <-
          mf %>%
          dplyr::select(-fxGeom_class) %>%
          lst_mf_args()
        do.call(fxe_labeller,
                rlang::list2(fxGeom_class = fx_geom, !!!mf_args))
      }
    ) %>%
    magrittr::set_names(vars) %>%
    rlang::dots_splice()
  do.call("labeller", lst, envir = asNamespace("ggplot2"))
}

#' Effex Extendibles in `fx_ggplot`: facetting labellers
#'
#' @return a valid argument to [ggplot2::labeller()]. Supplies [fx_ggplot()]
#' with the labels for the facetting variable
#'
#' @export

setGeneric("fxe_labeller",
           function(fxGeom_class, name, ...) standardGeneric("fxe_labeller"))

#' @describeIn fxe_labeller If the breaks are specified, these are used for the
#' labeller. If not the default argument [ggplot2::label_value()] is returned.
#'
#' @export

setMethod("fxe_labeller",
          signature = c(fxGeom_class = "fxGeomDiscrete"),
          function(fxGeom_class, name, fxGeom_limits,
                   fxGeom_breaks = NULL, ...) {
            if(!is.null(fxGeom_breaks)) {
              names(fxGeom_breaks) <- fxGeom_limits
              return(fxGeom_breaks)
            }
            ggplot2::label_value
          })
