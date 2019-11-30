#' Extract the estimates into a simpler form from a Stratified-Petersen fit
#' 
#' This function is normally used internally and not by users of the package.
#' 
#' @param est.red Vector of parameter estimates
#' @param rowDM Design matrix for covariates for row parameters (after pooling). Not currently implemented so
#'   this is usually an identity matrix.
#' @param colDM Design matrix for covariates for column parameters (after pooling). Not currently implemented so
#'   this is usually an identity matrix.
#' @param thetaDM Design matrix for covariates for movement parameters (after pooling). Not currently implemented so
#'   this is usually an identity matrix.
#' @param conditional Indicates if a conditional (poisson model) or full likelihood (multinomial) fit was done.
#'
#' @return 
#'   Returns a list with entries 
#' \itemize{
#'   \item theta The modelled number of animals released in row stratum i and recovered in column stratum j.
#'   \item cap. The estimated probability of tagging in row stratum i.
#'   \item psi. The estimated number of animals never seen after tagging for each row stratum.
#'   \item N.   The estimated population size.
#' }
#' @keywords internal




# SPAS.extract.par.est
# Extracts and creates the full estimates from the reduced parameter vector

# Input : 
#     Reduced parameter vector (in order)
#        theta.red.star  (on log scale)
#        cap.red.star    (on logit scale)
#        psi.red.star    (on log scale)
#        logN

SPAS.extract.par.est <- function(est.red,rowDM,colDM,thetaDM, conditional=TRUE){   
# extract the parameter estimates and create the parameter list
# if conditional=TRUE, then the N parameter does not exist
#
# first extact the reduced estimates, i.e. what is actually optimized

   n.theta.red.star <- ncol(thetaDM)
   theta.red.star <- est.red[1:n.theta.red.star]
   
   n.cap.red.star <- ncol(rowDM)
   cap.red.star   <- est.red[ (n.theta.red.star+1) : (n.theta.red.star+n.cap.red.star) ]
   
   n.psi.red.star <- nrow(rowDM) # not possible to put constraints on psi
   psi.red.star   <- est.red[(n.theta.red.star+n.cap.red.star+1)  : (n.theta.red.star+n.cap.red.star+n.psi.red.star)]
   if(!conditional) logN           <- est.red[(n.theta.red.star+n.cap.red.star+n.psi.red.star+1)]
   
#  expand the reduced estimates to the full  estimates using appropriate design matrices
   theta  <- thetaDM %*% theta.red.star
   cap    <- rowDM   %*% cap.red.star
   psi    <- psi.red.star  # no design matrices for psi
   
#  convert from logit to anti-logit, etc
   est <- NULL
   est$theta <- matrix(exp(theta), nrow=nrow(rowDM))
   est$psi   <- as.vector(exp(psi))
   est$cap   <- as.vector(expit(-cap)) # recall that estimates are log( (1-p)/p) rather than log( p/(1-p))
   if(!conditional)est$N     <- exp(logN)
   
   return(est)
} 