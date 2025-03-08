ui <- shiny::fluidPage(
                 shiny::tags$script(shiny::HTML(js)),
                 shiny::titlePanel("Recovery"),
                 shiny::splitLayout(
                     shiny::textAreaInput("test.text",
                                          "Predicted condition:",
                                          placeholder="diagnosiscol1>100 & diagnosiscol2==TRUE ...",
                                          width="100%",resize="vertical"),
                     shiny::textAreaInput("cond.text",
                                          "Actual condition:",
                                          placeholder="groundtruthcol1>100 & groundtruthcol2==TRUE ...",
                                          width="100%",resize="vertical")),
                 bslib::navset_pill(
                            bslib::nav_panel("Dataset",
                                             shiny::mainPanel(
                                                        DT::dataTableOutput("data.table"))),
                            bslib::nav_panel("Distributions",
                                             "Histogram for each relevant variable, colored according to class (half+, half-, etc.)"),
                            bslib::nav_panel("Summary",
                                             "Measures: editable table as in Wikipedia")
                        )
             )
