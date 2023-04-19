# Utility functions used in this package.
# These functions will not be visible to the user.


#' @param variable Variable to be checked if numeric
#' @param min.value Minimum value for variable
#' @param max.value Maximum value for variable
#' @param req.length Required length for variable
#' @param check.whole Check if all values in variable are whole numbers
#' @param data A  data frame
#' @param dt_type Type of double tags. See valid_dt_type()
#' @param sep. Separator for capture histories

#'
#' @importFrom reshape2 melt
#' @noRd



#######################################################################3
# Check that argument is numeric, specified length, and all values are between min /max
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol}  # taken from examples of is.integer


#######################################################################3

check.numeric <- function(variable, min.value=0, max.value=Inf, req.length=1, check.whole=TRUE){
  var.name <- deparse(substitute(variable))
  if(!is.numeric(min.value) | !is.numeric(max.value) | !is.numeric(req.length))
    stop("Bad input values to check.numeric: ", paste(c(var.name,"; ", min.value,"; ", max.value,"; ", req.length), collapse=", "))
  if(length(req.length) !=1)stop("Bad required length for check.numeric. You had ", req.length)
  if(length(min.value) !=1 & length(min.value) != req.length)
    stop("Min values wrong length in check.numeric. You have: ",paste(min.value, collapse=", "))
  if(length(max.value) !=1 & length(max.value) != req.length)
    stop("Max values wrong length in check.numeric. You have: ",paste(max.value, collapse=", "))
  if(!is.numeric(variable))stop("'",var.name, "' must be numeric")
  if( any(is.na(variable)))stop("'",var.name, "' cannot be missing. You have ", paste(variable, collapse=", "))
  if( length(variable) != req.length)stop("'",var.name, "' must have length ", req.length)
  if( any(variable < min.value) | any(variable > max.value))
    stop("'",var.name, "' must between (",paste(min.value, collapse=", "),") and (",
         paste(max.value, collapse=", "),
         ') (inclusive). You have ',paste(variable, collapse=", "))
  # check if values are all whole numbers
  if(check.whole){
    if(any(!is.wholenumber(variable)))stop("'",var.name, "' must be integers. You had ", paste(variable, collapse=", "))
  }
  invisible()
}

