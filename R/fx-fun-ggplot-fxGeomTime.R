#' @export
#'
#' @param fxGeom_shadow.threshold What is the threshold for the number of groups
#' so that the different lines are greyed out and a smoother is fitted? Default
#' is 10
#'
#' @describeIn fxe_layer_complete_nominate
#'     + a line plot that is possibly grouped according to the group aesthetic
#'     in `fxGeom_assoc_vars`. If there are more than 10 groups, the individual
#'     lines become transparent and a smooth function is fitted
#'     + a step plot with the same specifications

setMethod("fxe_layer_complete_nominate",
          signature = c(fx_geom = "fxGeomTime", aes_name = "xAesName"),
          function(fx_geom, aes_name, data, ...,
                   fxGeom_assoc_vars = NULL,
                   fxGeom_shadow.threshold = NULL, fxGeom_shadow.alpha = NULL) {
            nxt <- fxe_layer_complete_nominate(
              fxGeom("Continuous"), aes_name, data, ...,
              fxGeom_assoc_vars = fxGeom_assoc_vars,
              fxGeom_shadow.threshold = fxGeom_shadow.threshold,
              fxGeom_shadow.alpha = fxGeom_shadow.alpha
            )
            if(is.null(fxGeom_shadow.threshold)) fxGeom_shadow.threshold <- 10
            if(is.null(fxGeom_shadow.alpha)) fxGeom_shadow.alpha <- 0.05
            # get the number of groups
            grouping_quo <- fxGeom_assoc_vars[["group"]]
            if(is.null(grouping_quo)) {
              n_groups <- 1
              new_mapping <- ggplot2::aes()
            }
            else {
              grouping_var <-
                grouping_quo %>%
                rlang::quo_get_expr() %>%
                as.character()
              if(grouping_var %in% names(data)) {
                n_groups <-
                  data[[grouping_var]] %>%
                  unique() %>%
                  length()
                new_mapping <- fxGeom_assoc_vars["group"]
              }
              else {
                n_groups <- 1
                new_mapping <- ggplot2::aes()
              }
            }
            if(n_groups >= fxGeom_shadow.threshold) {
              nom <- list(
                nomination(
                  ggplot2::geom_line(mapping = new_mapping,
                                     alpha = fxGeom_shadow.alpha),
                  ggplot2::geom_smooth()
                ),
                nomination(
                  ggplot2::geom_step(mapping = new_mapping,
                                     alpha = fxGeom_shadow.alpha,
                                     na.rm = TRUE),
                  ggplot2::geom_smooth()
                )
              )
            }
            else {
              nom <- list(
                nomination(
                  ggplot2::geom_line(mapping = new_mapping)
                ),
                nomination(
                  ggplot2::geom_step(mapping = new_mapping)
                )
              )
            }
            c(nom, nxt)
          })

#' @export
#'
#' @describeIn fxe_layer_complete_vote
#'     + a line plot: 4
#'     * a path plot which is not a line plot: 3
#'     * `geom_smooth()`: 2

setMethod("fxe_layer_complete_vote",
          signature = c(fx_geom = "fxGeomTime", aes_name = "xAesName"),
          function(nomination, fx_geom, aes_name, data, ...) {
            nxt <- fxe_layer_complete_vote(
              nomination, fxGeom("Continuous"), aes_name, data, ...
            )
            bool_path <-
              nomination %>%
              nom_layers() %>%
              purrr::map_lgl( ~ inherits(.$geom, "GeomPath")) %>%
              any
            # When in doubt, choose a line, not a step
            bool_line <-
              nomination %>%
              nom_layers() %>%
              purrr::map_lgl( ~ inherits(.$geom, "GeomLine")) %>%
              any
            bool_smooth <-
              nomination %>%
              nom_layers() %>%
              purrr::map_lgl( ~ inherits(.$geom, "GeomSmooth")) %>%
              any
            bool_line * 1 + bool_path * 3 + bool_smooth * 2 + nxt
          })
