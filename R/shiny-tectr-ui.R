#' @rdname tectr

tectr_ui <- function() {
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
