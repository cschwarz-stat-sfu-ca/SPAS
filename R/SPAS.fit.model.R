#' Fit a Stratified-Petersen (SP) model using  TMB.
#' 
#' This function fits a Stratified-Petersen (Plante, 1996) to data and specify which rows/columns of the data
#' should be pooled. The number of rows after pooling should be <= number of columns after
#' pooling
#' .
#' @param model.id Character string identifying the name of the model including any pooling..
#' @template param.rawdata 
#' @template param.autopool
#' @template param.autopool.settings
#' @template param.row.pool.in
#' @template param.row.physical.pool
#' @param theta.pool,CJSpool NOT YET IMPLEMENTED. DO NOT CHANGE.
#' @param optMethod What optimization method is used. Defaults is the nlminb() function..
#' @param optMethod.control Control parameters for optimization method. See the documentation on the different optimization methods for details.
#' @param svd.cutoff When finding the variance-covariance matrix, a singular value decomposition is used. This identifies the smallest 
#'   singular value to retain.
#' @param chisq.cutoff When finding a goodness of fit statistic using (obs-exp)^2/exp, all cell whose Exp < gof.cutoff are ignored
#'        to try and remove structural zero cells.
#' @return A list with many entries. Refer to the vignettes for more details.
#' @export
#' @import msm MASS stats plyr TMB 
#' @importFrom Matrix bdiag 
#' @importFrom utils packageVersion
#' @examples 
#' conne.data.csv <- textConnection("
#' 9  ,    21  ,     0  ,    0  ,    0  ,    0  ,   171
#' 0  ,   101  ,    22  ,    1  ,    0  ,    0  ,   763
#' 0  ,     0  ,   128  ,   49  ,    0  ,    0  ,   934
#' 0  ,     0  ,     0  ,   48  ,   12  ,    0  ,   434
#' 0  ,     0  ,     0  ,    0  ,    7  ,    0  ,    49
#' 0  ,     0  ,     0  ,    0  ,    0  ,    0  ,     4
#' 351,  2736  ,  3847  , 1818  ,  543  ,   191 ,     0")
#' conne.data <- as.matrix(read.csv(conne.data.csv, header=FALSE))
#' close(conne.data.csv)
#'  
#' mod1 <- SPAS.fit.model(conne.data, model.id="Pooling rows 1/2, 5/6; pooling columns 5/6",
#'                       row.pool.in=c("12","12","3","4","56","56"),
#'                       col.pool.in=c(1,2,3,4,56,56))
#' mod2 <- SPAS.fit.model(conne.data, model.id="Auto pool",
#'                       autopool=TRUE)


# Fit the OPEN SPAS model to the data.
# This does the physical pooling of the data prior to the fit
# We have left in code to eventually allow you to specify covariates for capture-probabilties etc, but
# this has not yet been implemented.

# The final matrix, after pooling must have s <= t.

SPAS.fit.model<- function(model.id='Stratified Petersen Estimator', 
                          rawdata, autopool=FALSE,
                          row.pool.in=NULL, col.pool.in=NULL, 
                          row.physical.pool=TRUE, theta.pool=FALSE, CJSpool=FALSE, 
                          optMethod=c("nlminb"), 
                          optMethod.control=list(maxit = 50000), 
                          svd.cutoff=.0001, chisq.cutoff=.1,
                          min.released  =100, # autopooling settings
                          min.inspected = 50,
                          min.recaps    = 50,
                          min.rows=1, min.cols=1){
# Fit the Open SPAS model given the data and pooling information on the rows and columns
#
# The Open models have no constraints on the movement into the observed strata for each release group
# This is the model fit by the windoze SPAS program.
#
# rawdata - (s+1) x (t+1) raw data matrix
# row.pool.in - vector length s where duplicate entries indicate row to pool
# col.pool.in - vector length t where duplicate entries indicate columns to pool
# pooltheta   - logical indicator if want to pool for theta (movement) parameters - not yet implemented
# CJSpool     - logical indicator if want to have equal movement parameters along diagonal - not yet implemented
#
  
# Error checking of input values.
   # is Input data ok?
   if( !is.matrix(rawdata))stop("Input raw data must be a matrix")
   if( nrow(rawdata) < 2)  stop("Input raw data needs at least 2 rows")
   if( ncol(rawdata) < 2)  stop("Input raw data needs at least 2 columns")
   if( !is.numeric(rawdata))stop("Input raw data must be numeric")
  
   if(!is.logical(autopool) || length(autopool) !=1)stop("Autopool must be logical and length 1")
   if( autopool){
      if( !is.null(row.pool.in)) stop("Cannot specify row pooling if autopool=TRUE")
      if( !is.null(col.pool.in)) stop("Cannot specify columh pooling if autopool=TRUE")
   }
  
   if(!autopool){
      if(length(row.pool.in) != (nrow(rawdata)-1))stop("Row pooling vector not right length - number of rows(rawdata) -1")
      if(length(col.pool.in) != (ncol(rawdata)-1))stop("Column pooling vector not right length - number of columns(rawdata) -1")
   }
  
   # check that row.phyiscal.pool is logical.
   if(!is.logical(row.physical.pool) | length(row.physical.pool)!= 1)stop("Row.physical.pool must be logical of length 1")
   if(theta.pool)stop("Theta pooling not yet implemented")
   if(CJSpool   )stop("CJSpooling not yet implemented")

   # check input noise
   #if(!is.numeric(sd.noise.init.est))stop("sd.noise.input.est must be numeric")
  
   # check optimization method
   if(!optMethod[1] %in% c("nlminb"))stop("Invalid optimization method specified")
  
   if(!is.numeric(svd.cutoff))stop("Svd.cutoff must be numeric")
   if( svd.cutoff > .0001)stop("svd.cutoff is too large")   
  
   if(!is.numeric(chisq.cutoff) | length(chisq.cutoff) != 1)stop("chisquare cutoff must be numeric and length 1")
   
   if(autopool){
      res <- SPAS.autopool(rawdata, 
                           min.released  = min.released,
                           min.inspected = min.inspected,
                           min.recaps    = min.recaps,
                           min.rows      = min.rows, 
                           min.cols      = min.cols)
      rawdata <- res$reddata
      row.pool.in <- res$row.pool
      col.pool.in <- res$col.pool
   }

   # Does final pooled matrix have s <= t?
   if(length(unique(row.pool.in)) > length(unique(col.pool.in)))stop("S must be <= T after pooling")
  
   RESULT <- NULL
   RESULT$version <- paste0("SPAS-R ", utils::packageVersion("SPAS"))
   RESULT$date    <- Sys.time()   # date run and start date
   
   RESULT$input <- list(model.id    =model.id,
                        rawdata     = rawdata,
                        row.pool.in = row.pool.in,
                        col.pool.in = col.pool.in,
                        row.physical.pool=row.physical.pool,
                        theta.pool  = theta.pool,
                        CJSpool     = CJSpool,
                        chisq.cutoff= chisq.cutoff)
  
   s = nrow(rawdata)-1 # The number of release   strata prior to pooling
   t = ncol(rawdata)-1 # The number of recapture strata prior to pooling
   
   
#  row pooling first
   pool.frame <- data.frame(pool=as.factor(row.pool.in))
   if(length(unique(row.pool.in)) >1)  rowDM <- model.matrix( ~-1 + pool, data=pool.frame)
   if(length(unique(row.pool.in))==1)  {
      rowDM <- matrix(1, nrow=s, ncol=1)
      colnames(rowDM) <- as.character(row.pool.in[1])
   }
   if(row.physical.pool){  # physical pooling for the rows
      pool.row    <- t(rowDM) %*% rawdata[1:s,] 
      pool.row    <- rbind(pool.row, rawdata[s+1,])
      row.names(pool.row)[nrow(pool.row)] <- paste(row.names(rawdata)[s+1],"",sep="")
   }
   if(! row.physical.pool){  # logical pooling for the rows. nothing is done to the data
      pool.row <- rawdata
      row.names(pool.row) <- c(paste("pool", row.pool.in, sep="."), "")
      row.names(pool.row)[nrow(pool.row)] <- paste(row.names(rawdata)[s+1],"",sep="")
   }
   
#  column pooling next
   pool.frame <- data.frame(pool=as.factor(col.pool.in))
   if(length(unique(col.pool.in))> 1) colDM <- model.matrix( ~-1 + pool, data=pool.frame)
   if(length(unique(col.pool.in))==1)  {
      colDM <- matrix(1, nrow=t, ncol=1)
      colnames(colDM) <- as.character(col.pool.in[1])
   }
   # physical pooling of the rows
   pooldata    <- pool.row[, 1:t] %*% colDM 
   pooldata <- cbind(pooldata, pool.row[,t+1] )
   colnames(pooldata)[ncol(pooldata)] <-paste(colnames(pool.row)[t+1],"",sep="")
 
   s = nrow(pooldata)-1 # The number of release   strata AFTER physical pooling
   t = ncol(pooldata)-1 # The number of recapture strata AFTER physical pooling

# get the design matrices. If you do physical pooling, these are simple identity matrices
   if(row.physical.pool){
      rowDM  = diag(1, nrow(pooldata)-1)
      colDM  = diag(1, ncol(pooldata)-1)
      thetaDM= diag(1, (nrow(pooldata)-1)*(ncol(pooldata)-1))
   }
   #browser()
   if(!row.physical.pool){ #logical pooling so create the design matrices for rows and/columns
     # rowDM and colDM were computed earlier
     thetaDM = diag(1, (nrow(pooldata)-1)*(ncol(pooldata)-1)) # not yet implemented
   }

   # compute the condition number of XX' there X = physical matrix 
   # after physical pooling but prior to logical pooling
   # a large kappa value indicate the rows are colinear and so the estimates are unstable
   RESULT$kappa <- kappa( pooldata[1:s, 1:t,drop=FALSE] %*% t(pooldata[1:s, 1:t,drop=FALSE]))
   
   # compute the kappa after logical pooling of the data
   logical.pooldata <-   t(rowDM) %*% pooldata[1:s, 1:t,drop=FALSE]
   RESULT$kappa.after.lp <- kappa( logical.pooldata %*% t(logical.pooldata))
   
   # compute the pooled Petersen using the Chapman modification (add 1) and the se
   n1 <- sum(pooldata[1:s,    ])
   m2 <- sum(pooldata[1:s, 1:t])
   n2 <- sum(pooldata[   , 1:t])
   RESULT$est$N.Chapman = (n1+1)*(n2+1)/(m2+1) - 1
   RESULT$vcv$N.Chapman = ((n1+1)*(n2+1)*(n1-m2)*(n2-m2)) / ((m2+1)^2 * (m2+2))
   RESULT$se $N.Chapman = sqrt(RESULT$vcv$N.Chapman)
   
   
   # Initial Estimates obtained using least squares
   init.est = SPAS.init.est(rawdata=pooldata,rowDM=rowDM,colDM=colDM,thetaDM=thetaDM)

   # refer to https://stackoverflow.com/questions/48627069/guidelines-for-including-tmb-c-code-in-an-r-package
   # on how to include a TMB file with an R package
   
   # set up for the optimization 
   #dyn.load(dynlib("SPAS"))

   #browser() 
   # Create the model
   tmb.model <- MakeADFun(data= list(n_10 = pooldata[1:s  , t+1],
                                 u    = pooldata[s+1  , 1:t],
                                 m    = pooldata[1:s  , 1:t, drop=FALSE],
                                 rowDM=rowDM),
                        parameters=list(
                                 beta_cap   =as.vector(init.est$beta_cap),
                                 beta_theta =matrix(init.est$beta_theta, nrow=s, ncol=t),
                                 beta_psi   =as.vector(init.est$beta_psi)),
                        DLL="SPAS"
                        )

   #browser()
   
   
   # set the lower and upper bounds on the estimates 
   # theta are optimized on log scale; cap on logit scale; psi on log scale
   l.b = -12  # Lower Bound for optimization routine
   u.b =  20  # Upper Bound for optimization routine

   RESULT$fit.setup <- list(pooldata=pooldata,
                            rowDM = rowDM,
                            colDM = colDM,
                            thetaDM = thetaDM,
                            init.est= init.est,
                            l.b     = l.b,
                            u.b     = u.b,
                            tmb.model=tmb.model)
   # First optimization of the conditional likelihood.
   # This gives us the estimates of everything but N
    if(optMethod[1]=='nlminb'){
       cat("Using nlminb to find conditional MLE\n")
       fit <- nlminb(tmb.model$par, tmb.model$fn, tmb.model$gr, tmb.model$he, lower=l.b, upper=u.b)
       cat("Convergence codes from nlminb ", fit$convergence, fit$message, "\n")
   }

   RESULT$conditional$res$optim.info <- fit # from conditional maximization
   
   # Second estimate N using a conditional estimator .
   cat("Finding conditional estimate of N\n")
   
   # Extract the various estimates of the parameters from the optimization routine
   RESULT$est$beta$all <- fit$par
   
   # The psi terms. No design matrix. Beta parameters on log-scale
   beta_psi_index <- grepl("beta_psi", names(fit$par))
   RESULT$est$beta$psi <- fit$par[ beta_psi_index ]
   RESULT$est$real$psi <- exp(RESULT$est$beta$psi)
   
   # The theta terms. We have a design matrix and beta parameters on log scale, but nothing implemented yet
   beta_theta_index <- grepl("beta_theta", names(fit$par))
   RESULT$est$beta$theta <- fit$par[ beta_theta_index]
   RESULT$est$real$theta <- as.vector(exp(thetaDM %*% RESULT$est$beta$theta))
 
   # the capture probabilities. We have a design matrix and beta terms on logit scale
   beta_cap_index <- grepl("beta_cap", names(fit$par))
   RESULT$est$beta$cap <- fit$par[ beta_cap_index]
   RESULT$est$real$cap <- expit( as.vector(rowDM %*% RESULT$est$beta$cap)) 
   
   # extract the vcv for the various parameters and the se on the beta and regular scale
   RESULT$vcv$beta$allinfo <- SPAS.i.hess(tmb.model$he    ( fit$par))
   RESULT$vcv$beta$all     <- RESULT$vcv$beta$allinfo$vcv
   RESULT$vcv$beta$psi     <- RESULT$vcv$beta$all[ beta_psi_index,   beta_psi_index]
   RESULT$vcv$beta$cap     <- RESULT$vcv$beta$all[ beta_cap_index,   beta_cap_index]
   RESULT$vcv$beta$theta   <- RESULT$vcv$beta$all[ beta_theta_index, beta_theta_index]
   
   
   # Get the beta estimates after expanding by the design matrix
   # Note that the ordering of the parameters is determined by the .cpp file. So if that changes, you need to update this code.
   # (a) Go from the reduced estimates to the expanded estimates but still on the logit or log scale using the design matrices
   X <- bdiag(rowDM,                    # the logit(prob.capture) parameters
              thetaDM,                  # the log(theta) parameters
               diag(1,s)                # the log(psi) parameters
              ) 
   X <- as.matrix(X)   # sometime need to convert from sparse representation
   #browser()
   RESULT$est$beta$all.expanded <-  as.vector(X %*% RESULT$est$beta$all)
   RESULT$vcv$beta$all.expanded <-  X %*% RESULT$vcv$beta$all %*% t(X)

   # Now convert from the logit(), log() and log() scale to the real scale and find the vcv and se.
   # We use the deltamethod() function in the msm package
   # and so need to create extressions to do the delta method
   RESULT$est$real$all.expanded  <-c( RESULT$est$real$cap,   
                              RESULT$est$real$theta,
                              RESULT$est$real$psi)
   # set up the transformations 
   backtrans <- c(paste("~1/(1+exp(-x",    1:s,         "))"    , sep=""),
                  paste("~exp(x",      (s+1):(s+s*t),   ")"     , sep=""),
                  paste("~exp(x",  (s*t+s+1):(s*t+s+s),  ")"    , sep="")
                   )
   backtrans <- plyr::llply(backtrans, function(x){as.formula(x)})
   RESULT$vcv$real$all.expanded  <- msm::deltamethod(backtrans,  RESULT$est$beta$all.expanded,  RESULT$vcv$beta$all.expanded, ses=FALSE)
   RESULT$se $real$all.expanded  <- sqrt(diag( RESULT$vcv$real$all.expanded ))

   # extract the vcv of the real parameters and their se
   RESULT$vcv$real$cap    <- RESULT$vcv$real$all.expanded[        1:s  ,                 1:s      ,   drop=FALSE ]
   RESULT$vcv$real$theta  <- RESULT$vcv$real$all.expanded[    (s+1):(s+s*t)  ,       (s+1):(s+s*t),   drop=FALSE  ]
   RESULT$vcv$real$psi    <- RESULT$vcv$real$all.expanded[(s*t+s+1):(s*t+s+s)  , (s*t+s+1):(s*t+s+s), drop=FALSE ]
   
   RESULT$se$real$cap     <- sqrt(diag(RESULT$vcv$real$cap))
   RESULT$se$real$theta   <- sqrt(diag(RESULT$vcv$real$theta))
   RESULT$se$real$psi     <- sqrt(diag(RESULT$vcv$real$psi))
   
   # estimate N = total populaton size
   # Getting the variance of N (see equation 11 of Plante's paper)
   # Notice that we don't find the covariance of N with any of the other parameters

   RESULT$est$real$N  <- sum(pooldata, na.rm=TRUE) + tmb.model$report(fit$par)$neverseen
   part1 <- t(RESULT$est$real$psi) %*% diag(1/RESULT$est$real$cap^2, nrow=s) %*% 
            RESULT$vcv$real$cap %*%
            diag(1/RESULT$est$real$cap^2, nrow=s) %*% RESULT$est$real$psi
   part2 <- sum( RESULT$est$real$psi * ((1-RESULT$est$real$cap)/RESULT$est$real$cap + ((1-RESULT$est$real$cap)/RESULT$est$real$cap)^2 ))
   RESULT$vcv$real$N     <- part1 + part2
   RESULT$se$real$N      <- sqrt(RESULT$vcv$real$N)


   # Third. We now evaluate the conditional likelihood to get full covariance matrix of ALL parameters
   logL.cond   = -tmb.model$fn(fit$par)
   np          = length(fit$par)
   AIC =  2*np - 2*logL.cond #before correction
   n = sum(rawdata, na.rm=TRUE) # total sample size  
   AICc = AIC + 2*np*(np+1)/(n-np-1)

   RESULT$model.info <- list(model.id=model.id,
                             logL.cond   = logL.cond,
                             np          = np,       
                             AICc = AIC
                             ) 
   # Individual stratum estimates and their variances and set
   RESULT$est$real$N.stratum <- rowSums(pooldata)[1:s]/RESULT$est$real$cap 

   # Variances of individual stratum estimates (conditonal on number released)
   if( s >1){
     RESULT$vcv$real$N.stratum  <-   rowSums(pooldata)[1:s]^2 * (1/RESULT$est$real$cap^2)^2 * diag(RESULT$vcv$real$cap)
     RESULT$se $real$N.stratum  <-   sqrt(RESULT$vcv$real$N.stratum)
   }
   if( s==1){
     RESULT$vcv$real$N.stratum  <- RESULT$vcv$real$N
     RESULT$se $real$N.stratum  <- RESULT$se $real$N 
   }
   
   # Estimates of the expansion factors and their vcv
   RESULT$est$real$exp.factor     <- (1-RESULT$est$real$cap)/RESULT$est$real$cap
   RESULT$vcv$real$exp.factor     <- diag(1/RESULT$est$real$cap^2, nrow=s) %*% 
            RESULT$vcv$real$cap %*%
            diag(1/RESULT$est$real$cap^2, nrow=s)
   RESULT$se$real$exp.factor  <- sqrt(diag(RESULT$vcv$real$exp.factor))
 
   
   # Goodness-of-fit test. This is a simple chi-square test of observed vs. expected counts.
   # No pooling is done to deal with sparse counts, but a "cut off" of 0.1 is used to try
   # and eliminate cells with natural 0 counts (e.g. time stratification and t1 < t2)
   # The psi parameters are always a perfect fit and so no need to include them in the chi-square value
   # the cell counts, or the number of fitted parameters
   #browser()
   # find the component due to the movement matrix (m vs theta)
   gof.obs1 <- as.vector(RESULT$fit.setup$pooldata[1:(-1+nrow(RESULT$fit.setup$pooldata)), 1:(-1+ncol(RESULT$fit.setup$pooldata))])
   gof.exp1 <- as.vector(RESULT$est$real$theta)
   select   <- gof.exp1>chisq.cutoff
   chisq.gof <- sum((gof.obs1[select] - gof.exp1[select])^2/gof.exp1[select])
   
   # for the total number unobserved in recovery strata
   gof.obs2 <- RESULT$fit.setup$pooldata[nrow(RESULT$fit.setup$pooldata),1:(-1+ncol(RESULT$fit.setup$pooldata)) ]
   gof.exp2 <- as.vector((1-RESULT$est$real$cap)/RESULT$est$real$cap) %*% matrix(RESULT$est$real$theta,nrow=s, ncol=t) 
   select   <- gof.exp2>chisq.cutoff
   chisq.gof <- chisq.gof + sum((gof.obs2[select] - gof.exp2[select])^2/gof.exp2[select])
   chisq.gof.df <-  length(RESULT$est$real$theta) + s + t-RESULT$model.info$np
   RESULT$gof$chisq <- chisq.gof
   RESULT$gof$chisq.df <- chisq.gof.df
   RESULT$gof$chisq.p  <- ifelse(chisq.gof.df>0, pchisq(chisq.gof, df=chisq.gof.df, lower.tail=FALSE), NA)

   return(RESULT)            
}





#--------------------------------------------------------------------------------------------------
# SPAS.init.est;
# The following function creates initial estimates for the optimization routine for the OPEN SPAS models.
# using least squares estimators

# Input: raw data,
#        rowDM is a design matrix used to constrain the capture probabilities.
#        colDM is a design matrix used to constrain the column parameters   
#        thetaDM is a design matrix used to contrain the movement parameters   
#      
# Ouput: a list with entries
#       theta* = log( reduced theta parameters), i.e. as input to the design matrix
#       cap*   = log((1-p)/p) for each row
#       psi*   = log(reduced psi parameter - animals never recaptured)

#
SPAS.init.est <- function(rawdata,rowDM,colDM,thetaDM) { 

   s = nrow(rawdata)-1  # The number of release   stratas 
   t = ncol(rawdata)-1  # The number of recapture stratas
    
   # Number tagged in each stratum
   tagged = rowSums(rawdata)[1:s]
   # Unmarked captured
   unmarked = rawdata[s+1,1:t]
   # Marked fish recaptured
   m = rawdata[1:s,1:t, drop=FALSE]
   
   
   # The initial N hat estimate is found using the pooled petersen (a biased estimator). 
   N.hat = sum(tagged)*sum(c(m,unmarked))/sum(m)   
   # The N hat estimate is now transformed to the log scale. 
   logN.hat = log(N.hat)
   
   # number of fish tagged but never recovered (psi)
   psi.hat <- tagged - rowSums(m)
   psi.star.hat <- log(psi.hat) # no design matrix for psi, psihat is usually equal to actual psi
   
   # use a least squares estimator for the log (1-cap)/p
   logit.cap.hat <- log( pmax(.01, LS(t(m),unmarked) ))
   logit.cap.hat <- rep(mean(logit.cap.hat), length(logit.cap.hat))
   
   # initial theta values prior to using design matrix
   theta.hat <- as.vector(log(m+.1))
  
   # Now to find the estimates used with the design matrices. These are least-squares estimates
   logit.cap.star.hat <- LS(rowDM, logit.cap.hat)
   theta.star.hat     <- LS(thetaDM, theta.hat)
       
   res<- list(beta_theta=theta.star.hat, 
              beta_cap  = logit.cap.star.hat, 
              beta_psi  =psi.star.hat)
   return(res)
}


#Input : a Matrix A and Vector B (Design Matrix)
#Output: Least Squares Solution
LS <- function(A,b){
   x = MASS::ginv(t(A) %*% A) %*% t(A) %*% b
   return(x)
}
