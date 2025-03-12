summary.stat <- function(id,title,colour) {
    shiny::tags$div(shiny::tags$b(title),
                    shiny::tags$br(),
                    shiny::tags$b("Value: "),
                    shiny::textOutput(id,inline=TRUE),
                    shiny::textInput(id,NULL,placeholder=CONSTRAINT_PROMPT),
                    style=paste0("background-color: ",colour,";padding: 2.5%;margin: 0.5%;border-radius: 10px;"))
}

summary.stat.nc <- function(id,title,colour) {
    shiny::tags$div(shiny::tags$b(title),
                    shiny::tags$br(),
                    shiny::tags$b("Value: "),
                    shiny::textOutput(id,inline=TRUE),
                    style=paste0("background-color: ",colour,
                                 ";display: table;width: 100%; height: 100%;padding: 2.5%;margin: 0.5%;border-radius: 10px;"))
}

summary.placeholder <- function(colour) {
    shiny::tags$div(style=paste0("background-color: ",colour,
                                 ";display: table;width: 100%; height: 100%;padding: 2.5%;margin: 0.5%;border-radius: 10px;"))
}

ui <- shiny::fluidPage(
                 shiny::tags$script(shiny::HTML(js)),
                 shiny::titlePanel("Recovery"),
                 shiny::splitLayout(
                     shiny::textAreaInput("test.text",
                                          "Predicted condition",
                                          placeholder="diagnosiscol1>100 & diagnosiscol2==TRUE ...",
                                          width="100%",resize="vertical"),
                     shiny::textAreaInput("cond.text",
                                          "Actual condition",
                                          placeholder="groundtruthcol1>100 & groundtruthcol2==TRUE ...",
                                          width="100%",resize="vertical")),
                 bslib::navset_pill(
                            bslib::nav_panel("Dataset",
                                             "TODO:Change style of main matrix to nice table. Add other summary stats (different ui ?).Could display plot with selected rows marked, with selectable vars for xy axes. Should make input data optional in the end (dataset generation).",
                                             shiny::mainPanel(
                                                        DT::dataTableOutput("data.table"))),
                            bslib::nav_panel("Distributions",
                                             "Histogram for each relevant variable, colored according to class (half+, half-, etc.)"),
                            bslib::nav_panel("Confusion Matrix",
                                             "TODO: colors could change according to whether a constraint is present/can be met..",
                                             bslib::layout_column_wrap(
                                                        summary.stat("total.population","Total Population",LIGHTGREY),
                                                        summary.stat("predicted.positive","Predicted Positive",LIGHTGREY),
                                                        summary.stat("predicted.negative","Predicted Negative",LIGHTGREY),
                                                        summary.stat.nc("x.na","Prediction Missing",LIGHTGREY),
                                                        summary.placeholder(LIGHTGREY),
                                                        summary.placeholder(LIGHTGREY),
                                                        
                                                        summary.stat("actual.positive","Actual Positive",LIGHTGREY),
                                                        summary.stat("true.positive","True Positive",PALETTE[1]),
                                                        summary.stat("false.negative","False Negative",PALETTE[3]),
                                                        summary.stat.nc("true.na","Actually True/Prediction Missing",LIGHTGREY),
                                                        summary.stat("true.positive.rate","True Positive Rate",LIGHTGREY),
                                                        summary.stat("false.negative.rate","False Negative Rate",LIGHTGREY),

                                                        summary.stat("actual.negative","Actual Negative",LIGHTGREY),
                                                        summary.stat("false.positive","False Positive",PALETTE[2]),
                                                        summary.stat("true.negative","True Negative",PALETTE[4]),
                                                        summary.stat.nc("false.na","Actually False/Prediction Missing",LIGHTGREY),
                                                        summary.stat("false.positive.rate","False Positive Rate",LIGHTGREY),
                                                        summary.stat("true.negative.rate","True Negative Rate",LIGHTGREY),

                                                        summary.stat.nc("na.x","Actually Unknown",LIGHTGREY),
                                                        summary.stat.nc("na.positive","Actually Unknown/Prediction Positive",LIGHTGREY),
                                                        summary.stat.nc("na.negative","Actually Unknown/Prediction Negative",LIGHTGREY),
                                                        summary.stat.nc("na.na","Actually Unknown/Prediction Missing",LIGHTGREY),
                                                        summary.placeholder(LIGHTGREY),
                                                        summary.placeholder(LIGHTGREY),
                                                        
                                                        width=1/6)

                                             
                                             #shiny::splitLayout(
                                             #           summary.stat("total.population","Total Population",LIGHTGREY),
                                             #           summary.stat("predicted.positive","Predicted Positive",LIGHTGREY),
                                             #           summary.stat("predicted.negative","Predicted Negative",LIGHTGREY),
                                             #           summary.stat("youden.j","Youden's J",LIGHTGREY),
                                             #           summary.stat("prevalence.threshold","Prevalence Threshold",LIGHTGREY)),
                                             #shiny::splitLayout(
                                             #           summary.stat("actual.positive","Actual Positive",LIGHTGREY),
                                             #           summary.stat("true.positive","True Positive",PALETTE[1]),
                                             #           summary.stat("false.negative","False Negative",PALETTE[3]),
                                             #           summary.stat("true.na","Actually True/Prediction Missing",LIGHTGREY),
                                             #           summary.stat("true.positive.rate","True Positive Rate",LIGHTGREY),
                                             #           summary.stat("false.negative.rate","False Negative Rate",LIGHTGREY)),
                                             #shiny::splitLayout(
                                             #           summary.stat("actual.negative","Actual Negative",LIGHTGREY),
                                             #           summary.stat("false.positive","False Positive",PALETTE[2]),
                                             #           summary.stat("true.negative","True Negative",PALETTE[4]),
                                             #           summary.stat("false.na","Actually False/Prediction Missing",LIGHTGREY),
                                             #           summary.stat("false.positive.rate","False Positive Rate",LIGHTGREY),
                                             #           summary.stat("true.negative.rate","True Negative Rate",LIGHTGREY)),
                                             #shiny::splitLayout(
                                             #           summary.stat("prevalence","Prevalence",LIGHTGREY),
                                             #           summary.stat("na.positive","Actually Unknown/Prediction Positive",LIGHTGREY),
                                             #           summary.stat("na.negative","Actually Unknown/Prediction Negative",LIGHTGREY),
                                             #           summary.stat("na.na","Actually Unknown/Prediction Missing",LIGHTGREY),
                                             #           summary.stat("positive.predictive.value","Positive Predictive Value",LIGHTGREY),
                                             #           summary.stat("false.omission.rate","False Omission Rate",LIGHTGREY),
                                             #           summary.stat("positive.likelihood.ratio","Positive Likelihood Ratio",LIGHTGREY),
                                             #           summary.stat("negative.likelihood.ratio","Negative Likelihood Ratio",LIGHTGREY)),
                                             #shiny::splitLayout(
                                             #           summary.stat("accuracy","Accuracy",LIGHTGREY),
                                             #           summary.stat("false.discovery.rate","False Discovery Rate",LIGHTGREY),
                                             #           summary.stat("negative predictive value","Negative Predictive Value",LIGHTGREY),
                                             #           summary.stat("markedness","Markedness",LIGHTGREY),
                                             #           summary.stat("diagnostic.odds.ratio","Diagnostic Odds Ratio",LIGHTGREY)),
                                             #shiny::splitLayout(
                                             #           summary.stat("balanced.accuracy","Balanced Accuracy",LIGHTGREY),
                                             #           summary.stat("f1.score","F1 Score",LIGHTGREY),
                                             #           summary.stat("fowlkes.mallows.index","Fowlkes-Mallows Index",LIGHTGREY),
                                             #           summary.stat("matthews.correlation.coefficient",
                                             #                        "Matthews Correlation Coefficient",LIGHTGREY),
                                             #           summary.stat("jaccard.index","Jaccard Index",LIGHTGREY))
                                             )))
