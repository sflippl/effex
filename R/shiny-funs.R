#' @import shiny

NULL

#' tectr
#'
#' The `tectr` shiny app provides the main workflow of the package.
#'
#' @param project can be a `structure`, a file where `tectr()` previously saved
#' the progress

tectr <- function(project) {
  ui <- tectr_ui()
  server <- tectr_server(project)
  shinyApp(ui = ui, server = server) %>% runApp()
}

#' @rdname tectr
#'
#' @description `tectr_content()` provides an interface for changing structures.
#'
#' @export

tectr_structure <- function(structure = Structure()) {
  ui <- tectr_structure_ui()
  server <- tectr_structure_server(structure)
  shinyApp(ui = ui, server = server) %>% runApp()
}
