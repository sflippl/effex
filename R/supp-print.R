#' Supplementary print functions
#'
#' Help with printing different classes. `print_in_lines()` generates a
#' character, in a specified number of lines with specified characters per line.
#' Print with `cat()`.
#'
#' @param x character or coerced to character.
#' @param nchars number of characters to be printed
#' @param nlines number of lines to be printed
#' @param nchars_per_line number of characters in one line
#' @param linebreak what should be used as the breaking character? Default is
#'                  `"\n"`.
#' @param parsesign how should the function signal a parsed character? Default
#'                  is `"..."`. Should not be longer than a few characters and
#'                  can be omitted with `""`.
#'
#' @examples
#' cat(print_in_lines("Weird lyrics with weird linebreak",
#'                    nchars_per_line = 5,
#'                    nlines = 4))
#'
#' @importFrom assertthat assert_that

print_in_lines <- function(x,
                           nchars_per_line = NULL,
                           nlines = 1,
                           nchars = NULL,
                           linebreak = "\n",
                           parsesign = "...") {
  if(length(x) > 1) sapply(as.list(x), print_in_lines)

  else {
    # Assert numeric, one length input (nchars_per_line can have length(nlines))
    assert_that(is.numeric(c(nchars_per_line, nlines, nchars)))

    # Input correction

    charbreaks <- print_in_lines_input(x, nchars_per_line, nlines, nchars)

    # I still need to include some optional breaks only at specific characters!

    # Add the parsesign in the last characters of x

    nchars <- charbreaks[length(charbreaks)]

    if(nchars < nchar(x)) {
      parse_len <- nchar(parsesign)
      substr(x, nchars - parse_len + 1, nchars) <- parsesign
    }

    ret <- character(0)

    for(i in seq_len(length(charbreaks) - 1)) {
      ret[i] <- substring(text = x,
                          first = charbreaks[i] + 1,
                          last = charbreaks[i+1])
    }

    paste(ret[ret != ""], collapse = linebreak)
  }
}

#' @rdname print_in_lines
#'
#' @description `print_in_lines_input()` assures appropriate input of the
#' numeric specifications.

print_in_lines_input <- function(x,
                                 nchars_per_line = NULL,
                                 nlines = 1,
                                 nchars = NULL) {
  if(is.null(nchars_per_line)) {
    if(is.null(nchars)) nchars <- nchar(x)
    nchars_per_line <- ceiling(nchars / nlines)
  }
  nchars_per_line <- rep_len(nchars_per_line, nlines)
  if(is.null(nchars)) nchars <- sum(nchars_per_line)
  else assert_that(nchars == sum(nchars_per_line))
  nchars_per_line <- c(0, cumsum(nchars_per_line))
  nchars_per_line
}
