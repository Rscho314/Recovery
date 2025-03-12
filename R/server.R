class.label <- function(name,color,glyph) {
    shiny::em(name,shiny::span(style=paste("color:",color),paste0(glyph,"\Ufe0e")))
}

server <- function(data) {
    #rolog::rolog_init(argv1 = commandArgs()[1])
    #rolog::rolog_ok(warn=TRUE)
    #rolog::consult("./Prolog/lists_x_dict.pl")
    #rolog::query(call("pairs_x_dict",list(a=1,b=2),expression(X)))
    #dummy <- rolog::submit()
    #rolog::clear()
    #message(str(dummy))
    function(input, output, session) {
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
                                                          levels=c("TRUE|TRUE","TRUE|FALSE","FALSE|TRUE","FALSE|FALSE",
                                                                   "NA|TRUE","NA|FALSE","FALSE|NA","TRUE|NA","NA|NA"),
                                                          labels=CLASS_LABELS)})
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
                                                                                   class.label("True positive",PALETTE[1],"\U2b1b"),
                                                                                   class.label("False positive",PALETTE[2],"\U2b1b"),
                                                                                   class.label("False negative",PALETTE[3],"\U2b1b"),
                                                                                   class.label("True negative",PALETTE[4],"\U2b1b"),
                                                                                   class.label("Unknown/missing","black","\U2b1c")),
                                                           options=list(
                                                               lengthMenu=list(c(10,25,50,-1),c("10","25","50","All")),
                                                               columnDefs=list(
                                                                   list(visible=FALSE,
                                                                        targets=c("recovery.reserved.test.tf",
                                                                                  "recovery.reserved.cond.tf"))))
                                                           ),
                                             "class",
                                             target="row",
                                             backgroundColor = DT::styleEqual(CLASS_LABELS,
                                                                              c(PALETTE[1:4],rep("white",5)))),
                                     server=TRUE)
        output$total.population <- shiny::renderText({
            "NA"})
    }
}
