#' Effex Dispatch
#'
#' Effex Dispatch objects are intended for a simplified dispatch with S3
#' generics. They have the classes "fxd_<task>_<subclass>" where task is a
#' specific task, often fulfilled by the function "fx_<task>" (e. g. [fx_doc()])
#' and subclass stands for a specific category that is useful within the task.
#'
#' @param task a character without "_" that can specify a class
#' @param subclass a character that can specify a class (may include "_").
#' Default is NULL which indicates that a subclass is unnecessary.
#'
#' @export

fxd <- function(task, subclass = NULL) {
  assertthat::assert_that(is.character(task),
                          (is.character(subclass) | is.null(subclass)),
                          length(task) == 1, length(subclass) <= 1,
                          !stringr::str_detect(task, stringr::coll("_")))
  task_class <- c(paste("fxd", task, sep = "_"), "fxd")
  if(is.null(subclass)) class <- task_class
  else class <- c(paste("fxd", task, subclass, sep = "_"), task_class)
  structure(list(), class = class)
}

#' @rdname fxd
#'
#' @param x an object
#'
#' @export

is_fxd <- function(x) inherits(x, "fxd")

#' @rdname fxd
#'
#' @export

fxd_task <- function(x) fxd_split(x)[[1]]

#' @rdname fxd
#'
#' @export

fxd_subclass <- function(x) {
  fxd_split(x)[[2]]
}

fxd_split <- function(x) {
  assertthat::assert_that(is_fxd(x))
  split <- stringr::str_split_fixed(class(x), stringr::coll("_"), 3)
  len <- nrow(split)
  ret_task <- split[len - 1, 2]
  if(len <= 2 || split[len - 2, 1] != "fxd" ||
     split[len - 2, 2] != ret_task || split[len - 2, 3] == "")
    ret_subclass <- NULL
  else ret_subclass <- split[len - 2, 3]
  ret <- list(ret_task, ret_subclass)
}
