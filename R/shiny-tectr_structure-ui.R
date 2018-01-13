#' @rdname tectr

tectr_structure_ui <- function() {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        verticalLayout(
          wellPanel(
            actionButton(inputId = "initialize",
                         label = "Get original structure")),
          wellPanel(
            textInput(inputId = "content_name",
                      label = "Content Name"),
            textInput(inputId = "content_definition",
                      label = "Content Definition"),
            actionButton(inputId = "content_add",
                         label = "Add Content")),
          wellPanel(
            textInput(inputId = "relation_name",
                      label = "Relation Name"),
            textInput(inputId = "relation_definition",
                      label = "Relation Definition"),
            actionButton(inputId = "relation_add",
                         label = "Add Relation")),
          wellPanel(
            uiOutput(outputId = "edge_name"),
            uiOutput(outputId = "edge_from"),
            uiOutput(outputId = "edge_to"))),
        mainPanel(
          verticalLayout(
            verbatimTextOutput(outputId = "structure_verbatim"),
            plotOutput(outputId = "structure_visual"))))))
}
