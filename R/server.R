class.label <- function(name,color,glyph) {
    shiny::em(name,shiny::span(style=paste("color:",color),paste0(glyph,"\Ufe0e")))
}

server <- function(data) {
    
    #rolog setup
    rolog::rolog_init(argv1 = commandArgs()[1])
    rolog::rolog_ok(warn=TRUE,stop=FALSE)
    rolog::consult("~/Desktop/diag_swi/clpbnr_method/clpDiag/prolog/clpd.pl") #DEBUG

    #add contingency table columns to dataset
    data$recovery.reserved.test.tf <- NA
    data$recovery.reserved.cond.tf <- NA
    data$class <- NA

    shiny::onStop(function() rolog::rolog_done())
    
    function(input, output, session) {

        test.cond <- shiny::reactive({
            ll <- do.call("mapply", c(list, data, SIMPLIFY = FALSE, USE.NAMES=FALSE))
            if(input$test.text != "") {
                ast <- strast(str2lang(input$test.text))
                q <- rolog::once(call("r_formula_to_bools",ast,ll,expression(B)))
                data$recovery.reserved.test.tf <- as.logical(unlist(q))
            } else {
                data$recovery.reserved.test.tf <- NA #reset
            }
            if(input$cond.text != "") {
                ast <- strast(str2lang(input$cond.text))
                q <- rolog::once(call("r_formula_to_bools",ast,ll,expression(B)))
                data$recovery.reserved.cond.tf <- as.logical(unlist(q))
            } else {
                data$recovery.reserved.cond.tf <- NA
            }
            data <- within(data,{
                class <- factor(paste0(recovery.reserved.test.tf,"|",recovery.reserved.cond.tf),
                                levels=c("TRUE|TRUE","TRUE|FALSE","FALSE|TRUE","FALSE|FALSE",
                                         "NA|TRUE","NA|FALSE","FALSE|NA","TRUE|NA","NA|NA"),
                                labels=CLASS_LABELS)})
        }) %>% shiny::bindEvent(list(input$test.text,input$cond.text),
                                ignoreNULL=TRUE,ignoreInit=FALSE) #TODO:specify event !
        
        output$data.table <- DT::renderDataTable(
                                     DT::formatStyle(
                                             DT::datatable(test.cond(),
                                                           caption=shiny::tags$caption(
                                                                                   style="caption-side: top; text-align: center;",
                                                                                   "Row colors: ",
                                                                                   class.label("True positive",PALETTE[1],"\U2b1b"),
                                                                                   class.label("False positive",PALETTE[2],"\U2b1b"),
                                                                                   class.label("False negative",PALETTE[3],"\U2b1b"),
                                                                                   class.label("True negative",PALETTE[4],"\U2b1b"),
                                                                                   class.label("Unknown/missing","black","\U2b1c")),
                                                           options=list(
                                                               lengthMenu=list(c(10,25,50,-1),c("10","25","50","All"))
#                                                               columnDefs=list(
#                                                                   list(visible=FALSE,
#                                                                        targets=c("recovery.reserved.test.tf",
#                                                                                  "recovery.reserved.cond.tf")))
                                                           )),
                                             "class",
                                             target="row",
                                             backgroundColor = DT::styleEqual(CLASS_LABELS,
                                                                              c(PALETTE[1:4],rep("white",5)))),
                                     server=TRUE)
        output$total.population <- shiny::renderText({
            "NA"})
    }
}
