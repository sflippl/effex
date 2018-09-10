#' Effex Function: Render Output
#'
#' This function transforms the tibble which [fx_info()] yields (and which may
#' be modified afterwards) into a certain form.
#'
#' @param info The info tibble
#' @param form What form should it take. `fx_output` can be extended via this
#' argument (see [fxext_output()])
#' @param out_format What format should the output be in? Default argument is
#' "rst" which means an R object. For now, the other valid values are
#' "markdown", "html" and "latex".
#' @param out_file Should the object be saved somewhere? If yes, provide the
#' path - the object will still be returned in the end.
#' @param ... arguments for the [fxext_output()] methods
#'
#' @export

fx_output <-
  function(info, form = "", out_format = "rst", ...) {
  ret <- fxext_output(info = info, form = fxd("output", form),
                      out_format = out_format, out_file = out_file, ...)

  ret
  }

#' @rdname fx_output
#'
#' @export

fxext_output <- function(info, form, out_format, ...)
  UseMethod("fxext_output", form)

#' @export

fxext_output.fxd_output <- function(info, form, out_format, ...)
  stop("No method for form ", fxd_subclass(form), "provided")

#' @rdname fx_output
#'
#' @section Forms:
#' * `form = ""`: This standard does nothing except removing the name column
#' which should generally only be used for internal purposes. It is only
#' compatible with `out_format = NULL`.
#'
#' @export

fxext_output.fxd_output_ <- function(info, form, out_format, ...) {
  fx_output(info = dplyr::select(info, -name), form = "asis",
            out_format = out_format, out_file = out_file, ...)
}

#' @rdname fx_output
#'
#' @section Forms:
#' * `form = "asis"`: This form changes absolutely nothing. It only works with
#' `out_format = NULL`, as well.
#'
#' @export

fxext_output.fxd_output_asis <- function(info, form, out_format, ...) {
  info
}

#' @rdname fx_output
#'
#' @section Forms:
#' * `form = "table"`: This form generates a table and
#' works with all output formats. It is mainly powered by [knitr::kable()] and
#' you can pass additional options to both `kable` and
#' [kableExtra::kable_styling()] via `...`
#'
#' @export

fxext_output.fxd_output_table <-
  function(info, form, out_format, out_file, digits = 2, ...) {
    ret <- knitr::kable(info, format = out_format, digits = digits, ...)
    if(out_format %in% c("html", "latex"))
      ret <- kableExtra::kable_styling(ret, ...)
    ret
  }

#' @rdname fx_output
#'
#' @section Forms:
#' * `form = "collapse"`: This form generates one line for every row of the
#' tibble. `...` are passed on to [format()].
#'
#' @param cell_scheme a `character` that specifies how every cell, in connection
#' with its column name is styled. `name` enclosed by the `character`s specified
#' in `.open` and `.close` (see the documentation of [glue::glue()]) refers
#' to the column name and `value` to the cell content. Note
#' that you will have to specify `cell_scheme` if you change `.open` or
#' `.close`.
#'
#' @param cell_spec
#'
#' @export

fxext_output.fxd_output_collapse <-
  function(info, form, out_format, out_file,
           cell_scheme = if(out_format == "latex") "<<name>>: <<value>>"
                         else "{name}: {value}",
           digits = 2, cell_sep = ", ",
           .open = if(out_format == "latex") "<<" else "{",
           .close = if(out_format == "latex") ">>" else "}",
           ...) {
    # We first generate the list that will be fed into glue to generate the
    # vector.
    glue_input <- paste0(.open, ".x", .close, cell_sep,
                         .open, ".y", .close)
    ret <- purrr::map(
      names(dplyr::select(info, -name)),
      function(name) {
        value <- format(info[[name]], digits = digits, ...)
        glue::glue(cell_scheme, .open = .open, .close = .close)
      }
    ) %>%
      purrr::reduce(~ glue::glue(glue_input,
                                 .open = .open, .close = .close))
    ret
  }
