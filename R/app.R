js <- "
$(document).on('keyup', function(e) {
  if(e.keyCode == 13) {
    Shiny.setInputValue('keyPressed', true, {priority: 'event'});
  }
});
"


recovery.app <- function(data,...) {
    if(missing(data)) stop("Please provide a data.frame.")
    if(!is.data.frame(data)) stop("The data you provided is not a data.frame.")
    if("recovery.reserved.test.tf" %in% colnames(data)) stop("'recovery.reserved.test.tf' is a reserved column name")
    if("recovery.reserved.cond.tf" %in% colnames(data)) stop("'recovery.reserved.cond.tf' is a reserved column name")
    if("recovery.reserved.test.cond" %in% colnames(data)) stop("'recovery.reserved.test.cond' is a reserved column name")

    recovery.reserved.test.tf <- FALSE
    recovery.reserved.cond.tf <- FALSE
    recovery.reserved.test.cond <- 1
    
    ui <- shiny::fluidPage(
                     tags$script(HTML(js)),
                     shiny::titlePanel("Recovery"),
                     shiny::mainPanel(
                                shiny::textInput("test.text","Predicted condition:"),
                                shiny::textInput("cond.text","Actual condition:"),
                                DT::dataTableOutput("data.table")
                            )
                 )

    server <- function(input, output, session) {
        test.tf <- shiny::eventReactive(
                              input$keyPressed,
                              
                              if(!is.null(input$test.text) & !is.null(input$cond.text)) {
                                  test.expr <- str2expression(input$test.text)
                                  cond.expr <- str2expression(input$cond.text)
                                  test.cond <- within(data,{
                                      recovery.reserved.test.tf <- eval(test.expr) #this NEEDS preprocessing !
                                      recovery.reserved.cond.tf <- eval(cond.expr) #this NEEDS preprocessing !
                                  })
                                  within(test.cond,{
                                      recovery.reserved.test.cond <- as.numeric(as.factor(paste0(recovery.reserved.test.tf,
                                                                                                 recovery.reserved.cond.tf)))
                                  })
                              },
                              ignoreNULL=FALSE
                          )
        output$data.table <- DT::renderDataTable(
                                     DT::datatable(test.tf(),
                                                   options=list(lengthMenu=list(c(10,25,50,-1),c("10","25","50","All")))
                                                   ) %>% DT::formatStyle(
                                                      "recovery.reserved.test.cond",
                                                      target="row",
                                                      backgroundColor = DT::styleEqual(1:4, c('gray', 'yellow','red','green')))
            )
    }


    shiny::shinyApp(ui, server,...)
}
