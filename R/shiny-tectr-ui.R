#' @rdname tectr

tectr_ui <- function() {
  structure_panel <- sidebarLayout(
    sidebarPanel(
      verticalLayout(
        wellPanel(
          textInput(inputId = "content_name",
                    label = "Content Name"),
          textInput(inputId = "content_definition",
                    label = "Content Definition"),
          actionButton(inputId = "content_add",
                       label = "Add Content")
        ),
        well Panel(
          textInput(inputId = "relation_name",
                    label = "Relation Name"),
          textInput(inputId = "relation_definition",
                    label = "Relation Definition"),
          actionButton(inputId = "content_add",
                       label = )
        )
      )
    )
  )
  navbarPage(title = "tectr",
             tabPanel("Overview",
                      tabsetPanel(
                        tabPanel("Help",
                                 "An explanation of the tectr framework and how
                                 to use it."),
                        tabPanel("Variables",
                                 "A visualization of how the current structures
                                 look."),
                        tabPanel("Results",
                                 "What has been constructed or tested?")
                        )),
             tabPanel("Content",
                      tabsetPanel(
                        tabPanel("Structure",
                                 structure_panel),
                        tabPanel("Compounds",
                                 "How do you characterize your observations?"),
                        tabPanel("Construct and test",
                                 "What do you want to construct or test?")
                      )),
             tabPanel("Operation", "Here comes the statistics. Most of this part
                      will probably be done outside of the app - communications
                      via the RDS-file."))
}
