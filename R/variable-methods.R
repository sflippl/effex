# Print-------------------------------------------------------------------------

#' Print Variables
#'
#' `print` provides a clearer approach to a `variable`.
#' `print.content` prints the title of the character and a parsed definition as
#' well as the first five attributes.
#'
#' @examples
#' Content(title = "test", definition = "We are conducting a test",
#'         supp = "there is another attribute that is too long to be
#'                 displayed in one line. We therefore will parse it to the
#'                 appropriate length.")
#'
#' @param parse number of contents that are printed or logical `FALSE` that
#' indicates that all contents are to be printed.
#'
#' @export

print.content <- function(x, parse = 5, ...) {
  n <- length(x)
  logic <- parse > 0 & n > parse
  if(logic) n <- parse
  lapply(x[seq_len(n)], print, ...)
  if(logic) cat("Printed", n, "out of", length(x), "contents.\n")
}

print.atom_content <- function(x, parse = 5, ...) {
  title <- print_in_lines(as.character(x), nchars = 63)
  cat(class(x)[1], title, "\n")
  # One line has 80 characters. The first 340 characters of a definition are
  # printed (five lines as we will write "Definition: " in front of them).
  definition <- print_in_lines(attr(x, "Definition"),
                               nchars_per_line = 68,
                               nlines = 5,
                               linebreak = paste(c("\n", rep(" ", 12)),
                                                 collapse = ""))
  cat("Definition:", definition, "\n")
  attribs <- attributes(x)
  nams <- names(attribs)[!(names(attribs) %in% c("Definition", "class"))]
  for(i in seq_len(min(length(nams), 5))) {
    lngth <- min(nchar(nams[i]), 80)
    cat(print_in_lines(nams[i], nchars = 80), ": ",
        print_in_lines(attribs[[nams[i]]],
                       nchars = 80 - lngth), "\n", sep = "")
  }
  cat("Printed", min(length(nams), 5), "out of", length(nams), "features.\n")
  invisible(x)
}
