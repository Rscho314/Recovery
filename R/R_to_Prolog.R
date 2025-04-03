#R_boolean_to_Prolog <- function(s) {
#    tokens <- sourcetools::tokenize_string(s)
#    tokens <- tokens[tokens$type != "whitespace",]
#    if(!all(tokens[tokens$type == "operator",]$value) %in%
#       BOOL_OPERATORS)
#        stop(paste0("unauthorized operator, please use only those:\n",paste(BOOL_OPERATORS)))
#    paste(tokens$value)
#}

strast <- function(ee) purrr::list_flatten(purrr::map_if(as.list(ee), is.call, strast))

#getAST(str2lang("glucose>120&potassium<5.0"))
#Q <- call("=", expression(X), getAST(str2lang("glucose>120&potassium<5.0")))
#once(Q, options=list(portray=TRUE))
