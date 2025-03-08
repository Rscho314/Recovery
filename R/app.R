js <- "
$(document).on('keyup', function(e) {
  if(e.keyCode == 13) {
    Shiny.setInputValue('keyPressed', true, {priority: 'event'});
  }
});

$(document).on('focusout', function(e) {
    Shiny.setInputValue('focusOut', true, {priority: 'event'});
});
"

recovery.app <- function(data,...) {
    if(missing(data)) stop("Please provide a data.frame.")
    if(!is.data.frame(data)) stop("The data you provided is not a data.frame.")
    if("recovery.reserved.test.tf" %in% colnames(data)) stop("'recovery.reserved.test.tf' is a reserved column name")
    if("recovery.reserved.cond.tf" %in% colnames(data)) stop("'recovery.reserved.cond.tf' is a reserved column name")
    if("class" %in% colnames(data)) stop("'class' is a reserved column name")

    data$recovery.reserved.test.tf <- FALSE
    data$recovery.reserved.cond.tf <- FALSE
    data$class <- "NA"
    
    ui <- shiny::fluidPage(
                     shiny::tags$script(shiny::HTML(js)),
                     shiny::titlePanel("Recovery"),
                     shiny::mainPanel(
                                shiny::textInput("test.text",
                                                 "Predicted condition:",
                                                 placeholder="diagnosiscol1>100 & diagnosiscol2==TRUE ..."),
                                shiny::textInput("cond.text",
                                                 "Actual condition:",
                                                 placeholder="groundtruthcol1>100 & groundtruthcol2==TRUE ..."),
                                DT::dataTableOutput("data.table")))

    server <- function(input, output, session) {
        test.tf <- shiny::eventReactive(
                              input$keyPressed | input$focusOut,                          
                              {
                                  if(input$test.text != "" & input$cond.text != "") {
                                      test.expr <- str2expression(input$test.text)
                                      cond.expr <- str2expression(input$cond.text)
                                      test.cond <- within(data,{
                                          recovery.reserved.test.tf <- eval(test.expr) #this NEEDS preprocessing !
                                          recovery.reserved.cond.tf <- eval(cond.expr)})
                                      within(test.cond,{
                                          class <- factor(paste0(recovery.reserved.test.tf,
                                                                                       "|",
                                                                                       recovery.reserved.cond.tf),
                                                                                levels=c("TRUE|TRUE","TRUE|FALSE","FALSE|TRUE","FALSE|FALSE","NA|TRUE","NA|FALSE","FALSE|NA","TRUE|NA","NA|NA"),
                                                                                labels=c("T+","F+","F-","T-",rep("NA",5)))})
                                  } else {
                                      data
                                  }},
                              ignoreNULL=FALSE)
        output$data.table <- DT::renderDataTable(
                                     DT::formatStyle(
                                             DT::datatable(test.tf(),
                                                           caption=shiny::tags$caption(
                                                                                   style="caption-side: top; text-align: center;",
                                                                                   "Row colors: ",
                                                                                   shiny::em("True positive",
                                                                                             shiny::span(style=paste("color:",palette.colors(palette="Set 2")[1]),"\U2b1b\Ufe0e")),
                                                                                   shiny::em("False positive",
                                                                                             shiny::span(style=paste("color:",palette.colors(palette="Set 2")[2]),"\U2b1b\Ufe0e")),
                                                                                   shiny::em("False negative",
                                                                                             shiny::span(style=paste("color:",palette.colors(palette="Set 2")[3]),"\U2b1b\Ufe0e")),
                                                                                   shiny::em("True negative",
                                                                                             shiny::span(style=paste("color:",palette.colors(palette="Set 2")[4]),"\U2b1b\Ufe0e")),
                                                                                   shiny::em("Unknown/missing",
                                                                                             shiny::span(style="color: black","\U2b1c\Ufe0e")),
                                                                                   ),
                                                           options=list(
                                                               lengthMenu=list(c(10,25,50,-1),c("10","25","50","All")),
                                                               columnDefs=list(
                                                                   list(visible=FALSE,
                                                                        targets=c("recovery.reserved.test.tf",
                                                                                  "recovery.reserved.cond.tf"))))),
                                             "class",
                                             target="row",
                                             backgroundColor = DT::styleEqual(c("T+","F+","F-","T-",rep("NA",5)),
                                                                              c(palette.colors(palette="Set 2",n=4),
                                                                                rep("white",5)))))
    }


    shiny::shinyApp(ui, server,...)
}
