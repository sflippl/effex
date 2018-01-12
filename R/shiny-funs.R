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
  shinyApp(ui = tectr_ui(), server = server) %>% runApp()
}
