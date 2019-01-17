#' Score, likelihood, and related functions for fitting Stratified Petersen model
#' 
#' These functions are used internally to fit the Stratified Petersen models and are not normally used.
#' .
#' @keywords internal

# Likelihood (and related) functions for the Open SPAS models.

# SPAS.likelihood.star.DM.O(init.est, rowDM, colDM, thetaDM, rawdata, returnnegll=TRUE)
# SPAS.extract.par.est.O (res$par,rowDM,colDM,thetaDM, conditional=TRUE)
# SPAS.extract.par.est.O (init.est,rowDM,colDM,thetaDM, conditional=TRUE)

SPAS.likelihood.star.DM = function(Est.Star,rowDM,colDM,thetaDM,rawdata, returnnegll=FALSE, conditional=TRUE, fixed.N=0, mytrace=FALSE){
   #print(Est.Star)
   est <- SPAS.extract.par.est(Est.Star,rowDM,colDM,thetaDM, conditional=conditional)
   #browser()
   res <- SPAS.log.likelihood(est,rawdata, returnnegll=returnnegll, conditional=conditional, fixed.N=fixed.N, mytrace=mytrace)
   return(res)
} 


SPAS.score.DM = function(Est.Star,rowDM,colDM,thetaDM,rawdata, returnnegll=TRUE, conditional=TRUE){
   # compute the score (first partial deritivatives) based on the current estimates, design matrices, etc
   # This is computed only for the conditional likelihood at the moment.
  
   # convert the single vector into the actual estimates
   est <- SPAS.extract.par.est(Est.Star,rowDM,colDM,thetaDM, conditional=TRUE)
   #browser()
   
   s = nrow(rawdata)-1
   t = ncol(rawdata)-1

   # m matrix - Marked Fishes from strata s recaptured in strata t
   m = rawdata[1:s,1:t, drop=FALSE]

   # Fish Unrecovered from s-th stratum 
   n.10 = rawdata[1:s,t+1]

   # Unmarked Fish from the t-th stratum 
   u = rawdata[s+1,1:t]

   # Fish tagged for the s-th stratum
   n = rowSums(m) + n.10
   
   # score w.r.t. to parameter for number released and recaptured - theta
   # dont forget that Est.Star has log(theta.red) where theta = exp( thetaDM*theta.red)
   #
   score.theta.red <- m - est$theta  + 
                      diag((1-est$cap)/est$cap,nrow=length(est$cap),ncol=length(est$cap)) %*% 
                      est$theta %*% 
                      diag(u / as.vector(as.vector((1-est$cap)/est$cap) %*% est$theta)-1, ncol=length(u),nrow=length(u))
   score.theta  <- as.vector(score.theta.red) %*% thetaDM
   
   # score w.r.t. parameter for capture probabilities- caps
   # dont forget that Est.Star has MINUS logit(cap) as the parameter 
   score.cap <- as.vector((1-est$cap)/est$cap) * rowSums( est$theta %*% diag(u/ as.vector(as.vector((1-est$cap)/est$cap) %*% est$theta)-1, nrow=length(u), ncol=length(u))      )
   score.cap <- as.vector(score.cap) %*% rowDM
   
   # score w.r.t. to parameter for number released  but never seen - log(psi)
   # dont forget the Est.Star has log(psi) recorded
   score.psi <- n.10 - est$psi
   
   res <- c(score.theta, score.cap, score.psi)
   # but we are minimizing the negative of the loglikelihood
   if(returnnegll) {res <- -res}
#   print(res)
   return(res)
} 




# Log Likelihood Function for Open SPAS models
# Input : Full Estimate list on probability and anti-log scale for N, Raw Data.
# Output: Value. 
SPAS.log.likelihood <- function(est,rawdata, returnnegll=FALSE, conditional=FALSE, fixed.N=0, mytrace=FALSE){
# est - list of real parameter values with separate entry for each parameter type
# rawdata - the raw data in standard format
# returnnegll - should the negative of the log-likelihood be returned (for optimization purposes)
# condtional - should the conditional log likelihood poisson approximation be used (Plante and Rivest L_p)
  #browser()
  s = nrow(rawdata)-1
  t = ncol(rawdata)-1

  # m matrix - Marked Fishes from strata s recaptured in strata t
  m = rawdata[1:s,1:t, drop=FALSE]

  # Fish Unrecovered from s-th stratum 
  n.10 = rawdata[1:s,t+1]

  # Unmarked Fish from the t-th stratum 
  u = rawdata[s+1,1:t]

  # total fish seen
  n = sum(rawdata)

  # Movement Matrix
  theta = est$theta

  # Capture Probabilities
  cap = as.vector(est$cap)

  # Fish never seen again 
  psi <- as.vector(est$psi)
  
  # N hat only if the unconditional estimator is to be used.
  if(!conditional) N = est$N
  if(fixed.N>0) N=fixed.N

  if(!conditional){
     # The unconditional loglikelihood based on the the multionomial
     part1 = lgamma(N+1)  - lgamma(N-n+1) - lgamma(n+1)   
     part2 = sum(m    *log( (theta + as.numeric(theta==0))/N))
     part3 = sum(n.10 *log( (psi + as.numeric(psi==0))/N)) 
     part4 = sum(u    *log( (((1-cap)/cap) %*% theta+ as.numeric(u==0))/N))
     part5 = (N-n)    *log(1- (sum(theta)+sum(psi)+sum((((1-cap)/cap) %*% theta )))/N)
     l.l.h = part1 + part2 + part3 + part4 + part5 #- N*log(N)
     if( is.nan(l.l.h))browser()
     #browser()
  }
  if(mytrace)print(est)
   if(conditional){
     # the conditional log likelihood based on L_p of Plante and Rivest
     # Notice that conditional log-likelihood does not have N has a parameter
     part2 = sum(m * log(theta + as.numeric(theta==0))) - sum(theta)
     part3 = sum( n.10*log( psi + as.numeric(psi==0))) - sum(psi)
     part4 = sum(u*log(((1-cap)/cap) %*% theta+ as.numeric(u==0))) - sum( ((1-cap)/cap) %*% theta)
     l.l.h =  part2 + part3 + part4
   }
   # Negative Log Likelihood ? 
   if(returnnegll){ l.l.h <- -l.l.h}
   return(l.l.h)
}

# SPAS.i.fisher.DM
# This function creates the Variance Covariance Matrix for the Open SPAS models
#    Note: The inverse hessian is calculated using SVD.
#
# Input : 
#      Star Estimate Vector, row Design Matrix, Col Design Matrix, theta Design Matrix,  Raw Data
# Output: List of the following;
#        vcv = Variance Covariance Matrix. 
#        u = U orth matrix (SVD)
#        v = V orth matrix (SVD)
#        d.sig = Inverse Singular Values (SVD)

SPAS.i.fisher.DM <- function(est.star,rowDM,colDM,thetaDM,rawdata, conditional=FALSE, svd.cutoff=.0001){
   hes = -numDeriv::hessian(SPAS.likelihood.star.DM, x=est.star, method = ("Richardson"),
                 rowDM=rowDM,colDM=colDM,thetaDM=thetaDM,rawdata=rawdata, conditional=conditional)
   hess.svd = svd(hes)
   hess.svd$d = 1/(hess.svd$d+(hess.svd$d<svd.cutoff)) * (hess.svd$d > svd.cutoff)    
   vcv <- hess.svd$u %*% diag(hess.svd$d) %*% t(hess.svd$v )        
   return(list(vcv=vcv,u = hess.svd$u,v = hess.svd$v, d.sig = diag(hess.svd$d)) )
}




# Model AICc
#This function calculates the AICc for the Open SPAS models.
# Input : 
#      Star Estimate Vector, Row Design Matrix, Col Design Matrix, Design Matrix, Theta Design Matrix, 
#      Raw Data.
# Output: AICC Value. 

SPAS.AICc <- function(est.star, rowDM, colDM, thetaDM,rawdata){
   est <- SPAS.extract.par.est(est.star, rowDM, colDM, thetaDM)
   np = length(est.star)  # numer of parameters  
   AIC =  2*np + 2*SPAS.likelihood.star.DM(est.star,rowDM,colDM,thetaDM,rawdata, returnnegll=FALSE, conditional=TRUE) #before correction
   n = sum(rawdata) # total sample size  
   AICc = AIC + 2*np*(np+1)/(n-np-1)
   return(AICc)
} 

