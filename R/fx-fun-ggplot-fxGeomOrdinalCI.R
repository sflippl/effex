#' @export
#'
#' @describeIn fxe_layer_complete_nominate
#'     + a line plot together with a shaded area for the confidence
#'     interval
#'     + a scatter plot with uncertainty lines

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomOrdinalCI", aes_name = "yAesName"),
          function(fx_geom, aes_name, data, ...,
                   fxGeom_assoc_vars = NULL, fxGeom_errorbar.threshold = NULL) {
            nxt <- fxe_layer_complete_nominate(
              fxGeom("Ordinal"), aes_name, data, ...,
              fxGeom_assoc_vars = fxGeom_assoc_vars,
              fxGeom_errorbar.threshold = fxGeom_errorbar.threshold)
            if(is.null(fxGeom_errorbar.threshold))
              fxGeom_errorbar.threshold <- 200
            n_row <- nrow(data)
            upper_quo <- fxGeom_assoc_vars[["upper"]]
            lower_quo <- fxGeom_assoc_vars[["lower"]]
            new_mapping <- ggplot2::aes()
            bool_errorbar <- FALSE
            # if high and low mappings are well defined, define them and set
            # bool_errorbar to true.
            if(!is.null(upper_quo)) {
              upper_var <-
                  upper_quo %>%
                  rlang::quo_get_expr() %>%
                  as.character()
              if(upper_var %in% names(data)) {
                if(!is.null(lower_quo)) {
                  lower_var <-
                      lower_quo %>%
                      rlang::quo_get_expr() %>%
                      as.character()
                  if(lower_var %in% names(data)) {
                    new_mapping <-
                      fxGeom_assoc_vars[c("upper", "lower")] %>%
                      magrittr::set_names(c("ymax", "ymin")) %>%
                      c(ggplot2::aes(fill = NULL, colour = NULL))
                    class(new_mapping) <- "uneval"
                    bool_errorbar <- TRUE
                  }
                }
              }
            }
            if(bool_errorbar) {
              ci_nom <-
                list(
                  nomination(
                    ggplot2::geom_line(),
                    ggplot2::geom_ribbon(new_mapping, alpha = 0.1)
                  ),
                  nomination(
                    ggplot2::geom_step(),
                    ggplot2::geom_ribbon(new_mapping, alpha = 0.1)
                  )
                )
            }
            else
              ci_nom <- NULL
            if(bool_errorbar & n_row < fxGeom_errorbar.threshold) {
              errorbar_nom <-
                list(
                  nomination(
                    ggplot2::geom_point(),
                    ggplot2::geom_linerange(mapping = new_mapping)
                  )
                )
            }
            else
              errorbar_nom <- NULL
            c(ci_nom, errorbar_nom, nxt)
          })

#' @export
#'
#' @describeIn fxe_layer_complete_vote
#'     + [ggplot2::geom_ribbon()]: 1
#'     + [ggplot2::geom_linerange()]: 1

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomOrdinalCI", aes_name = "yAesName"),
          function(nomination, fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_vote(
              nomination, fxGeom("Ordinal"), aes_name, data, ...
            )
            bool_ribbon <-
              nomination %>%
              nom_layers() %>%
              purrr::map_lgl( ~ inherits(.$geom, "GeomRibbon")) %>%
              any
            bool_linerange <-
              nomination %>%
              nom_layers() %>%
              purrr::map_lgl( ~ inherits(.$geom, "GeomLinerange")) %>%
              any
            if(bool_linerange) return(3 + nxt)
            if(bool_ribbon) return(2 + nxt)
            nxt
          })
