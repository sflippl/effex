#' Filepaths
#'
#' Filepaths keep metaframes manageable by externalizing semantic information to
#' files and capturing their columns with the attribute field.
#'
#' @param path The path to the file
#' @param fields The fields within the file
#' @param reader The reader of the files. A function that takes `file` for the
#' filename and optionally additional arguments. For compatibility, it is
#' recommended to allow dots arguments.
#'
#' @export

new_filepath <- function(path, fields, reader) {
  assertthat::assert_that(is.function(reader))
  structure(path, class = c("filepath", class(path)),
            fields = fields, reader = reader)
}

#' @rdname new_filepath
#'
#' @export

fields <- function(x) attr(x, "fields")

#' @rdname new_filepath
#'
#' @export

reader <- function(x) attr(x, "reader")

#' @rdname new_filepath
#'
#' @export

is_filepath <- function(x) inherits(x, "filepath")

#' @describeIn new_filepath takes a filepath and uses the reader-function to
#' read all files in as a data frame. It can be identified with the supplied
#' filepath by the `name`-column.
#'
#' @export

read_filepath <- function(filepath, ...)
  purrr::map_dfr(unique(filepath), ~ reader(filepath)(file = ., ...))

#' @rdname new_filepath
#'
#' @export

as_filepath <- function(x) {
  if(is_filepath(x)) return(x)
  assertthat::assert_that(all(c("fields", "reader") %in% names(attributes(x))),
                          is.function(reader(x)))
  class(x) <- c("filepath", class(x))
  x
}

#' @rdname new_filepath
#'
#' @export

`[.filepath` <- function(x, i) {
  fields <- fields(x)
  reader <- reader(x)
  x <- new_filepath(NextMethod(), fields, reader)
  x
}
