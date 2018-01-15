tectr_structure_server <- function(structure) {
  function(input, output) {
    # This creates the structure that has initially been put in.
    re_structure <- eventReactive(input$initialize, structure)

    # Add Content -------------------------------------------------------------

    re_structure <- eventReactive(input$content_add, {
      extend_variables(re_structure(),
                       Content(input$content_name,
                       input$content_definition))
                 })

    # Add Relations -----------------------------------------------------------

    re_structure <- eventReactive(input$relation_add, {
      extend_relations(re_structure(),
                       C_Relation(input$relation_name,
                       input$relation_definition))
    })

    # Add Relations -----------------------------------------------------------

    re_structure <- eventReactive(input$edge, {
      extend_edges(re_structure(),
                   data.frame(from = input$edge_from,
                              to = input$edge_to,
                              name = input$edge_name))
    })

    output$edge_name <- renderUI({
      selectInput(inputId = "edge_name",
                  label = "relation",
                  choices = re_structure() %>%
                    attr("relations") %$%
                    name)
    })
    output$edge_from <- renderUI({
      selectInput(inputId = "edge_from",
                  label = "From",
                  choices = re_structure() %>%
                    activate(nodes) %>%
                    as.data.frame %$%
                    name,
                  multiple = TRUE)
    })
    output$edge_to <- renderUI({
      selectInput(inputId = "edge_to",
                  label = "To",
                  choices = re_structure() %>%
                    activate(nodes) %>%
                    as.data.frame %$%
                    name,
                  multiple = TRUE)
    })
    output$structure_verbatim <- renderPrint(re_structure() %>% print)
    output$structure_visual <- renderPlot(re_structure() %>% visualize_struct)
  }
}
