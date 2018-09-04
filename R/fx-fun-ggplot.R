#' Effex Function: Visualization
#'
#' This function creates an effectively and explicitly specified visualization
#' of several variables in line with the syntax of [ggplot2].
#'
#' Required columns in the metaframe:
#'
#' * `fxGeom_class`: What class does the geometry have?
#'
#' Columns that are used:
#'
#' * `fxGeom_nominations`: A tibble of nominated layers (see
#' [fxint_layer_complete()])
#' * `fxGeom_vetos`: A function that returns the nominations data frame without
#' the vetoed layers
#' * `fxGeom_votes`: A function that returns the nominations data frame with the
#' new votes
#'
#' @param data A dataframe with [metaframe()] as an attribute
#' @param mapping An aesthetic mapping
#'
#' @export

fx_ggplot <- function(data, mapping) {
  p <- ggplot(data, mapping)

  # At first, we add the geometric information along one dimension
  for(nam in names(mapping)) {
    p <- fxint_layer_single(data, mapping, nam)(p)
  }

  # Then, we add the geometric information that depends on all dimensions
  p <- fxint_layer_complete(data, mapping)(p)
}

#' Effex Internals: `fx-ggplot` - geometric information along one dimension

fxint_layer_single <- function(data, mapping, nam) {
  mf <- metaframe(data) %>%
    dplyr::filter(name == as.character(rlang::quo_get_expr(mapping[[nam]])))
  assertthat::assert_that(nrow(mf) == 1, "fxGeom_class" %in% colnames(mf))
  fx_geom <- fxGeom(mf[["fxGeom_class"]])
  aes_name <- AesName(nam)
  mf <- rlang::as_list(mf) %>%
    purrr::map(unlist)
  fxext_layer_single(fx_geom, aes_name, data, rlang::dots_splice(mf))
}

#' Effex Extendibles: `fx-ggplot` - geometric information along one dimension
#'
#' This is the extendible function for the single layer. It is dispatched
#' along the arguments `fx_geom` and `aes_name`. Arbitrary other arguments may
#' be added that should then be expected in the metaframe. It should always be
#' considered a possibility that these values are missing.
#'
#' @export

setGeneric(
  "fxext_layer_single",
  function(fx_geom, aes_name, data, ...) standardGeneric("fxext_layer_single")
)

#' @export

setMethod(
  "fxext_layer_single",
  signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
  function(fx_geom, aes_name, data, ...) return(identity)
)

#' Effex Internals: `fx_ggplot` - combine all different dimensions
#'
#' `fxint_layer_complete` is mainly driven by two extendible functions:
#' [fxext_layer_complete_nominate()] which allows you to supply a data frame
#' with new nominations in the corresponding column for geom, stat, position and
#' params, and [fxext_layer_complete_vote()] which allows you to give votes
#' depending on the geom, stat, position and params by modifying the `votes`
#' columns so that it adds your votes. If a certain combination is incompatible
#' with the data type, you may remove it.
#'
#' @param data the data with the metaframe
#' @param mapping the aesthetic mapping

fxint_layer_complete <- function(data, mapping) {
  mf <- metaframe(data) %>%
    dplyr::mutate(name = as.character(name)) %>%
    dplyr::inner_join(
      dplyr::tibble(aes = names(mapping), name = get_inds(mapping)),
      by = "name")
  assertthat::assert_that("fxGeom_class" %in% colnames(mf))
  lst_fxGeom <- purrr::map(mf[["fxGeom_class"]], fxGeom)
  lst_aesName <- purrr::map(mf[["aes"]], AesName)
  mf_len <- nrow(mf)
  geoms <- dplyr::tibble(geom = list(), stat = list(), position = list(),
                         params = list())
  for(i in seq_len(mf_len)) {
    # Prepare any other arguments for splicing
    mf_args <- dplyr::select(mf[i, ], -aes, -name, -fxGeom_class) %>%
      rlang::as_list(.) %>%
      purrr::map(1)
    # Unlist cannot be recursive because this would destroy list columns.
    # Let each aesthetic nominate its potential candidates
    geoms <- dplyr::bind_rows(
      geoms,
      do.call("fxext_layer_complete_nominate",
              rlang::list2(fx_geom = lst_fxGeom[[i]],
                           aes_name = lst_aesName[[i]],
                           data = data,
                           !!!mf_args))
    )
  }
  # Remove duplicates und initialize the votes
  geoms <- unique(geoms) %>% dplyr::mutate(votes = 0)
  # Veto the inapplicable nominees and give the other nominees the corresponding
  # votes.
  for(i in seq_len(mf_len)) {
    mf_args <- rlang::as_list(mf[i, ]) %>% purrr::map(unlist)
    geoms <- fxext_layer_complete_vote(
      geoms, lst_fxGeom[[i]], lst_aesName[[i]],
      data, rlang::splice(mf_args)
    )
  }
  geoms <- dplyr::arrange(geoms, dplyr::desc(votes))
  if(nrow(geoms) == 0) stop("Aesthetics incompatible.")
  lay <- ggplot2::layer(geom = geoms$geom[[1]],
                        stat = geoms$stat[[1]],
                        position = geoms$position[[1]],
                        params = geoms$params[[1]])
  function(p) p + lay
}

#' Get names from a mapping

get_inds <- function(x) {
  purrr::map_chr(x, ~ as.character(rlang::quo_get_expr(.)))
}

#' Effex Extendibles: `fx_ggplot` - nominate layers
#'
#' @inherit fxint_layer_complete description
#'
#' @seealso fxint_layer_complete
#'
#' @export

setGeneric("fxext_layer_complete_nominate",
           function(fx_geom, aes_name, data, ...)
             standardGeneric("fxext_layer_complete_nominate"))

#' @export

setMethod("fxext_layer_complete_nominate",
          signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
          function(fx_geom, aes_name, data, ..., fxGeom_nominations = NULL) {
            if(is.null(fxGeom_nominations))
              fxGeom_nominations <-
                dplyr::tibble(geom = list(), stat = list(), position = list(),
                              params = list())
            stopifnot(setequal(names(fxGeom_nominations),
                      c("geom", "stat", "position", "params")))
            fxGeom_nominations
          })

#' @rdname fxext_layer_complete_nominate
#'
#' @param geoms a data.frame with the columns `geom`, `stat` and `position` which
#' contain the nominations and `votes` which contain the votes that have been
#' given so far.
#'
#' @export

setGeneric("fxext_layer_complete_vote",
           function(geoms, fx_geom, aes_name, data, ...)
             standardGeneric("fxext_layer_complete_vote"))

#' @export

setMethod("fxext_layer_complete_vote",
          signature = c(fx_geom = "fxGeom", aes_name = "AesName"),
          function(geoms, fx_geom, aes_name, data, ...,
                   fxGeom_vetos = NULL, fxGeom_votes = NULL) {
            if(is.function(fxGeom_vetos)) geoms <- fxGeom_vetos(geoms, ...)
            else if(!is.null(fxGeom_vetos))
              stop("fxGeom_vetos must be NULL or a function")
            if(is.function(fxGeom_votes)) geoms <- fxGeom_votes(geoms, ...)
            else if(!is.null(fxGeom_votes))
              stop("fxGeom_votes must be NULL or a function")
            geoms
          })
