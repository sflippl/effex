#' @rdname tectr

tectr_server <- function(project = NULL) {
  UseMethod("tectr_server", project)
}

#' @rdname tectr

tectr_server.NULL <- function(project) {

}
