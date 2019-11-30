#' Helper functions for this package (logit, expit)
#' 
#' These functions are helper functions for the SPAS package and not normally accessed by the user
#' 
#' \code{logit} - compute the logit of a proportion
#' \code{expit} - compute a proportion from a logit
#'  
#' @param p A proportion.
#' @param theta A logit
#' 
#' @keywords internal

# Miscellaneous functions for use in the modelling

# LOGIT amd EXPIT FUNCTIONs
logit <- function(p){
  res <- log(p/(1-p))
  res <- pmax(-20, pmin(20, res)) # restrict the logit between -20 + 20
  return(res)
}

expit <- function(theta){ #antilogit,i.e. from logit to p scale
  res <- 1/(1+exp(-theta))
  return(res)
}






