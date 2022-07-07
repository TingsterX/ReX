## ------------------------------
# functions used in rex_command_*
## ------------------------------

#' unpack the python-style arguments 
#' 
#' convert the python-style variable name(s) or indices read in from the command line using \code{optparse} package into R readable arguments 
#' @param var_input - a character input specify the variable names separate by ',' or index(indices). For example
#'                    5:10              will take the 5-th to the 10-th columns 
#'                    3,4,5:10          will take the 3rd, 4-th and 5-th to the 10-th columns 
#'                    var1,var2,var3    will take 3 variables named var1, var2, and var3 
#' @return numeric vector or character list
#' @export
unpack_var <- function(var_input){
  if (grepl('[0-9]', gsub(',|:', '', var_input))){
    # if the inputs only contains numbers and , :
    v <- eval(parse(text=sprintf("c(%s)",var_input)))
  }
  else if (grepl('[a-zA-Z]', var_input) && !grepl(':', var_input)){
    # if the inputs contains letters but no :
    var_list <- strsplit(var_input, ",")
    v <- unlist(var_list)
  }
  else{
    stop("Check the input argument(s). If more than one variable, names need to be separated using comma; indices follows ", call.=FALSE)
  }
  return(v)
}