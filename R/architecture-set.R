tectr_env <- new.env()
tectr_env$.arc <- NULL

#' Standard architecture
#'
#' The `set_architecture`/`get_architecture` framework allows you to use
#' architectures more effectively without having to supply the architecture of
#' interest every time.
#'
#' @describeIn get_architecture What is the standard architecture?
#'
#' @export

get_architecture <- function() {
  get(".arc", envir = tectr_env)
}

#' @details If you change the name of the architecture, you will have to set it
#' again as tectr has no way of knowing which architecture it should refer to.
#' tectr will therefore always simply refer to the architecture with the name
#' it has been supplied.
#'
#' @describeIn get_architecture Change the standard architecture to work in.
#'
#' @return If an architecture is supplied, that architecture is returned
#' invisibly. As a side effect standard changes to an architecture change this
#' architecture. If no architecture is supplied, an architecture with the same
#' effects is created and returned invisibly as well.
#'
#' @param archie an architecture that is set as a standard. If NULL is supplied,
#' the default architecture is deleted.
#'
#' @param ... parameters that modify the architecture or set those of the new
#' one.
#'
#' @export

set_architecture <- function(archie = NULL) {
  # Provide expression here to avoid mismatch.

  archie_expr <- enexpr(archie)

  UseMethod("set_architecture")
}

#' @rdname get_architecture
#'
#' @export

set_architecture.default <- function(archie = NULL) {
  stop("Supply a valid architecture.")
}

#' @rdname get_architecture
#'
#' @export

set_architecture.architecture <- function(archie = NULL) {
  if((as.character(archie_expr)) %in% ls(envir = parent.frame())) {
    assign(".arc", archie_expr, envir = tectr_env)
    message("Set default architecture to ", as.character(archie_expr), ".")
  } else {
    stop(as.character(archie_expr), " does not exist.")
  }
  invisible(archie)
}

#' @rdname get_architecture
#'
#' @export

set_architecture.NULL <- function(archie = NULL) {
  assign(".arc", NULL, envir = tectr_env)
  message("Set default architecture to NULL.")
  invisible(NULL)
}
