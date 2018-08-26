#' @export

explore <- function(data, indicator, metadata = NULL) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Explore your Data"),
    effex_selectIndicator(indicator, aes(name = description), "ind"),
    plotOutput(outputId = "plot")
  )
  server <- function(input, output) {
    output$plot <- renderPlot({
      effex_plot(data, indicator, mapping = aes_string(x = input$ind))
    })
  }
  shinyApp(ui = ui, server = server)
}
