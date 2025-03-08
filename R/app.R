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

    server <- server(data)
    
    shiny::shinyApp(ui, server,...)
}
