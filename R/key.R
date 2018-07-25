#' Keys
#'
#' Keys are an essential component of [collection_df()]s. They are a list of
#' character vectors and can therefore use a few extra operations that are
#' undefined.
#'
#' @param ... several character vectors.
#'
#' @export

new_key <- function(...) {
  ret <- list(...)
  assertthat::assert_that(all(vapply(ret,
                                     function(x)
                                       is.character(x) | is.null(x),
                                     logical(1))))
  ret <- lapply(ret, sort)
  class(ret) <- c("key", "list")
  ret
}

#' @rdname new_key
#'new
#' @param x an object.
#'
#' @export

is_key <- function(x) inherits(x, "key")

#' @rdname new_key
#'
#' @export

as_key <- function(x) {
  UseMethod("as_key")
}

#' @export

as_key.key <- function(x) x

#' @export

as_key.character <- function(x) {
  if(length(x) == 0) return(new_key())
  new_key(x)
}

#' @export

as_key.list <- function(x) {
  do.call("new_key", x)
}

#' @export

as_key.NULL <- function(x) {
  new_key()
}

#' @export

c.key <- function(...) {
  as_key(NextMethod())
}

#' @export

 `[.key` <- function(x, i) {
   as_key(NextMethod())
 }

#' Compare keys
#'
#' Keys can be compared with all common comparison operators. In this context,
#' they are simply treated as sets. `==` returns whether they are equal and `<`
#' and `<=` return whether `e1` is an exclusive respectively inclusive subset of
#' `e2`.
#'
#' @name compare_keys
#'
#' @export

`==.key` <- function(e1, e2) {
  if(length(e1) == 0 | length(e2) == 0) return(logical(0))
  ret <- mapply(setequal, e1, e2)
  assertthat::assert_that(is.logical(ret))
  ret
}

#' @rdname compare_keys
#'
#' @export

`!=.key` <- function(e1, e2) !(e1 == e2)

#' @rdname compare_keys
#'
#' @export

`<=.key` <- function(e1, e2) {
  ret <- mapply(function(x1, x2) {
    all(x1 %in% x2)
  }, e1, e2)
  assertthat::assert_that(is.logical(ret))
  ret
}

#' @rdname compare_keys
#'
#' @export

`<.key` <- function(e1, e2) (e1 <= e2) & (e1 != e2)

#' @rdname compare_keys
#'
#' @export

`>=.key` <- function(e1, e2) e2 <= e1

#' @rdname compare_keys
#'
#' @export

`>.key` <- function(e1, e2) e2 < e1

#' @rdname new_key
#'
#' @export

format.key <- function(x, ...) {
  if(length(x) == 0) return("key(0)")
  out <- NextMethod()
  out[x == new_key(NULL)] <- "no key"
  out
}

#' @rdname new_key
#'
#' @export

print.key <- function(x) {
  out <- format(x)
  print(out)
  invisible(x)
}

#' Key as character
#'
#' For the purpose of joining columns, keys may be transformed to `character`
#' vectors. For this reason, ", " may currently not be used in a column name.
#' I do not expect this to be a problem. Should one arise, I will change the
#' separating characters.
#'
#' @param x the key, as `key` or as `character`.
#'
#' @export

key_to_char <- function(x) vapply(
  X = x,
  FUN = function(x) paste0(sort(x), collapse = ", "),
  FUN.VALUE = character(1)
)

#' @export

char_to_key <- function(x) as_key(
  stringr::str_split(x, pattern = stringr::fixed(", ")) %>%
    lapply(function(x) {
      if(identical(x, "")) return(NULL)
      x
    }
  )
)

#' #' @importFrom pillar is_vector_s3
#' #' @export
#'
#' is_vector_s3.key <- function(x) TRUE
#'
#' #' @importFrom pillar obj_sum
#' #' @export
#'
#' obj_sum.key <- function(x) rep("key", length(x))
#'
#' @importFrom pillar type_sum
#'
#' @export

type_sum.key <- function(x) "key"

#' @importFrom pillar pillar_shaft
#'
#' @export

pillar_shaft.key <- function(x, ...) {
  out <- paste0(
    format(x),
    pillar::style_subtle(paste0(" [", vapply(x, length, integer(1)), "]"))
  )
  out[x == new_key(NULL)] <- crayon::red("no key")
  pillar::new_pillar_shaft_simple(out, align = "left", min_width = 5)
}
