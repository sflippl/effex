tectr_structure_server <- function(structure) {
  function(input, output) {
    # This creates the structure that has initially been put in.
    re_structure <- eventReactive(input$initialize, structure)

    # Add Content -------------------------------------------------------------

    observeEvent(input$content_add, {
                   re_structure() <- add_content(structure, content)
                 })
    output$edge_from <- renderUI({
      selectInput(inputId = "edge_from",
                  label = "From",
                  choices = structure %>%
                    activate(nodes) %$%
                    name,
                  multiple = TRUE)
    })
    output$edge_to <- renderUI({
      selectInput(inputId = "edge_to",
                  label = "To",
                  choices = structure %>%
                    activate(nodes) %$%
                    name,
                  multiple = TRUE)
    })
  }
}
