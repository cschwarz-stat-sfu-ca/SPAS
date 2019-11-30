#' Fit a Stratified-Petersen (SP) model using the legacy method without TMB.
#' The preferred method is to use the SPAS.fit.model() function that used TMB.
#' 
#' This function fits a Stratified-Petersen (Plante, 1996) to data and specify which rows/columns of the data
#' should be pooled. The number of rows after pooling should be <= number of columns after
#' pooling
#' .
#' @param model.id Character string identifying the name of the model including any pooling..
#' @param rawdata An (s+1) x (t+1) of the raw data BEFORE pooling.
#'   The s x t upper left matrix is the number of animals released in row stratum i and recovered in
#'   column stratum j. Row s+1 contains the total number of UNMARKED animals recovered in column stratum j.
#'   Column t+1 contains the number of animals marked in each row stratum but not recovered in any column stratum.
#'   The rawdata[s+1, t+1] is not used and can be set to 0 or NA.
#'   The sum of the entries in each of the first s rows is then the number of animals marked in each row stratum.
#'   The sum of the entries in each of the first t columns is then the number of animals captured (marked and unmarked) in each column stratum.
#'   The row/column names of the matrix may be set to identify the entries in the output.
#' @param row.pool.in,col.pool.in Vectors (character/numeric) of length s and t respectively. These identify the rows/columns to be pooled before the analysis is done.
#'   The vectors consists of entries where pooling takes place if the entries are the same. For example, if s=4, then 
#'   row.pool.in = c(1,2,3,4) implies no pooling because all entries are distinct; row.pool.in=c("a","a","b","b") implies that the 
#'   first two rows will be pooled and the last two rows will be pooled. It is not necessary that row/columns be continuous to be pooled, but
#'   this is seldom sensible. A careful choice of pooling labels helps to remember what as done, e.g. row.pool.in=c("123","123","123","4") indicates
#'   that the first 3 rows are pooled and the 4th row is not pooled. Character entrie ensure that the resulting matrixi is sorted properly (e.g. if 
#'   row.pool.in=c(123,123,123,4), then the same pooling is done, but the matrix rows are sorted rather strangely.
#' @param row.physical.pool  Should physical pooling be done (default) or should logical pooling be done. For example, if there are 3 rows in 
#'   the data matrix and row.pool.in=c(1,1,3), then in physical pooling, the entries in rows 1 and 2 are physically added together to create
#'   2 rows in the data matrix before fitting. Because the data has changed, you cannot compare physical pooling using AIC. In logical pooling,
#'   the data matrix is unchanged, but now parameters p1=p2 but the movement parameters for the rest of the matrix are not forced equal.
#' @param theta.pool,CJSpool NOT YET IMPLEMENTED. DO NOT CHANGE.
#' @param sd.noise.init.est How much random noise should be added to the initial (least squares) estimates. Normally only used with severe convergence problems.
#' @param optMethod What optimization method is used. Defaults is the BBoptim function from the BB package.
#' @param optMethod.control Control parameters for optimization method. See spg() function in BB package or optim() function for details.
#'    For BBoptim, a suggest control parameter for debugging is optMethod.control=list(M=20, trace=TRUE , maxit = 50000, ftol=10^-5).
#' @param svd.cutoff When finding the variance-covariance matrix, a singular value decomposition is used. This identifies the smallest 
#'    singular value to retain.
#' @param chisq.cutoff When finding a goodness of fit statistic using (obs-exp)^2/exp, all cell whose Exp < gof.cutoff are ignored
#'        to try and remove structural zero cells.
#' @return A list with many entries. Refer to the vignettes for more details.
#' @export
#' @import BB numDeriv msm MASS stats plyr
#' @importFrom Matrix bdiag 
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
#' mod1 <- SPAS.fit.model.legacy(conne.data, model.id="Pooling rows 1/2, 5/6; pooling columns 5/6",
#'                       row.pool.in=c("12","12","3","4","56","56"),
#'                       col.pool.in=c(1,2,3,4,56,56),
#'                       optMethod.control=list(ftol=.0001))
#'

# Fit the OPEN SPAS model to the data.
# This does the physical pooling of the data prior to the fit
# We have left in code to eventually allow you to specify covariates for capture-probabilties etc, but
# this has not yet been implemented.

# The final matrix, after pooling must have s <= t.

SPAS.fit.model.legacy<- function(model.id='Stratified Petersen Estimator', 
                          rawdata,row.pool.in, col.pool.in, 
                          row.physical.pool=TRUE, theta.pool=FALSE, CJSpool=FALSE, 
                          sd.noise.init.est=0,
                          optMethod=c("BBoptim","optim"), 
                          optMethod.control=list(maxit = 50000, ftol=1e-9, gtol=1e-5), 
                          svd.cutoff=.0001, chisq.cutoff=.1){
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
   
   if(length(row.pool.in) != (nrow(rawdata)-1))stop("Row pooling vector not right length - number of rows(rawdata) -1")
   if(length(col.pool.in) != (ncol(rawdata)-1))stop("Column pooling vector not right length - number of columns(rawdata) -1")
  
   # Does final pooled matrix have s <= t?
   if(length(unique(row.pool.in)) > length(unique(col.pool.in)))stop("S must be <= T after pooling")
  
   # check that phyiscal pool is logica.
   if(!is.logical(row.physical.pool) | length(row.physical.pool)!= 1)stop("Row.physical.pool must be logical of length 1")
   if(theta.pool)stop("Theta pooling not yet implemented")
   if(CJSpool   )stop("CJSpooling not yet implemented")

   # check input noise
   if(!is.numeric(sd.noise.init.est))stop("sd.noise.input.est must be numeric")
  
   # check optimization method
   if(!optMethod[1] %in% c("BBoptim","optim"))stop("Invalid optimization method specified")
  
   if(!is.numeric(svd.cutoff))stop("Svd.cutoff must be numeric")
   if( svd.cutoff > .0001)stop("svd.cutoff is too large")   
  
   if(!is.numeric(chisq.cutoff) | length(chisq.cutoff) != 1)stop("chisquare cutoff must be numeric and length 1")
   
   RESULT <- NULL
   RESULT$version <- "SPAS-R 2019-02-06"
   RESULT$date    <- Sys.time()   # date run and start date
   
   RESULT$input <- list(rawdata     = rawdata,
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
 
   s = nrow(pooldata)-1 # The number of release   strata AFTER to pooling
   t = ncol(pooldata)-1 # The number of recapture strata AFTER to pooling

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

   # Initial Estimates obtained using least squares
   #browser(text='Call to init.est')
   init.est = SPAS.init.est.legacy(rawdata=pooldata,rowDM=rowDM,colDM=colDM,thetaDM=thetaDM, conditional=TRUE)
   #browser()
   # set the lower and upper bounds on the estimates 
   # theta are optimized on log scale; cap on logit scale; psi on log scale; and N on log scale (not needed for conditional fit)
   l.b = c(rep(-12,length(init.est))) # 0 ) # Lower Bound for optimization routine
   u.b = c(rep( 12,length(init.est)))# ,30) # Upper Bound for optimization routine

   # Add a little noise to the initial estimates ???
   if(sd.noise.init.est>0){init.est <- init.est + rnorm(length(init.est),sd=sd.noise.init.est)}
   RESULT$fit.setup <- list(pooldata=pooldata,
                            rowDM = rowDM,
                            colDM = colDM,
                            thetaDM = thetaDM,
                            init.est= init.est,
                            l.b     = l.b,
                            u.b     = u.b)
   # First optimization of the conditional likelihood.
   # This gives us the estimates of everything but N
   # browser()
   if((!(optMethod[1] %in% c("BBoptim","optim","SANN"))) | optMethod[1]=='BBoptim'){
       cat("Using BBoptim() to find conditional MLE\n")
       res =BB::BBoptim(par=init.est,
                fn=SPAS.likelihood.star.DM,  gr=SPAS.score.DM,
                method=1,lower=l.b,upper=u.b, 
                control=optMethod.control,
                #control=list(M=20, trace=TRUE , maxit = 50000), # ftol=10^-5),
                quiet=FALSE,
                rowDM=rowDM,colDM=colDM,thetaDM=thetaDM,rawdata=pooldata, returnnegll=TRUE, conditional=TRUE)
   }
   if(optMethod[1] == 'optim'){
        res =optim(par=init.est,
                fn=SPAS.likelihood.star.DM, gr=SPAS.score.DM,
                method="L-BFGS-B",lower=l.b,upper=u.b,
                control=optMethod.control,
                #control=list(trace=6, maxit = 5000),
                rowDM=rowDM,colDM=colDM,thetaDM=thetaDM,rawdata=pooldata, returnnegll=TRUE, conditional=TRUE)
   }
#   if(optMethod[1] == 'SANN'){
#      res = GenSA(par=init.est, fn=SPAS.likelihood.star.DM, lower=l.b, upper=u.b, 
#               control=list(verbose=TRUE, max.time=600),
#               rowDM=rowDM,colDM=colDM,thetaDM=thetaDM,rawdata=pooldata, returnnegll=TRUE, conditional=TRUE)
#   }
   
   RESULT$conditional$res$optim.info <- res # g from conditional maximization
   
   # Second estimate N using a conditional estimator .
   cat("Finding conditional estimate of N\n")
   est <- SPAS.extract.par.est(res$par, rowDM, colDM, thetaDM, conditional=TRUE)
   est$Nhat <- sum(pooldata) + sum(est$psi*(1-est$cap)/est$cap) 
   RESULT$conditional$res$Nhat <- est$Nhat
   RESULT$conditional$res$Nhat.stratum <- rowSums(pooldata)[1:s]/est$cap 
   
   # Third. We now evaluate the conditional likelihood to get full covariance matrix of ALL parameters
   #res$par <- as.vector(c(res$par, log(est$Nhat)))  # use the conditional values with log N as the next parameter
   RESULT$model.info <- list(model.id=model.id,
                             logL.cond   = SPAS.likelihood.star.DM(res$par,rowDM,colDM,thetaDM,pooldata, conditional=TRUE),
                             #logL.uncond = SPAS.likelihood.star.DM(res$par,rowDM,colDM,thetaDM,pooldata, conditional=FALSE),
                             np   = length(res$par),       
                             AICc = SPAS.AICc(res$par,rowDM,colDM,thetaDM,pooldata)
                             ) 
 
   # Extract the VCV from conditional model using numerical derivatives at the conditional estimates
   hess = SPAS.i.fisher.DM(res$par,rowDM,colDM,thetaDM,pooldata, svd.cutoff= svd.cutoff, conditional=TRUE)
 
   RESULT$est.red.star$est  <- res$par
   RESULT$est.red.star$vcv  <- hess$vcv  # hess$vcv[nrow(hess$vcv), ncol(hess$vcv)]
   RESULT$est.red.star$se   <- sqrt(diag(hess$vcv))

   # Get the estimates after expanding by the design matrix
   # (a) Go from the reduced estimates to the expanded estimates but still on the * scale using the design matrices
   X <- bdiag(thetaDM,                 # the theta* parameters
              rowDM,                   # the logit(prob.capture) parameters
              diag(1,s)                # the psi parameters
              #diag(1)                 # the log(N) parameter (not currently estimated directly)
              ) 
   X <- as.matrix(X)   # sometime need to convert from spare representation
   #browser()
   RESULT$est.star$est <-  as.vector(X %*% RESULT$est.red.star$est)
   RESULT$est.star$vcv  <- X %*% RESULT$est.red.star$vcv %*% t(X)
   RESULT$est.star$se   <- sqrt(diag(RESULT$est.star$vcv))
 
   # (b) Go from log(theta) -> theta, log((1-p)/p) -> p, log(psi) -> psi, log(N) - > N
   # We use the deltamethod() function in the msm package
   # and so need to create extressions to do the delta method
   RESULT$real$est.all <- c( exp  ( RESULT$est.star$est[1:(s*t)]),            #log(theta) -> theta
                             expit(-RESULT$est.star$est[(s*t+1):(s*t+s)]),    #log((1-p)/p)-> p
                             exp  ( RESULT$est.star$est[(s*t+s+1):(s*t+s+s)]) #log(psi) -> psi
                             #exp  ( RESULT$est.star$est[(s*t+s+s+1):(s*t+s+s+1)]) # log(N) -> N (not currently done)
                             ) 
   
   backtrans <- c( paste("~exp(x", 1:(s*t), ")"               , sep=""),
                  paste("~1/(1+exp(x", (s*t+1):(s*t+s),"))"  , sep=""),
                  paste("~exp(x", (s*t+s+1):(s*t+s+s),")"    , sep="")
                  #paste("~exp(x", (s*t+s+s+1):(s*t+s+s+1),")", sep="")
                  )
   backtrans <- llply(backtrans, function(x){as.formula(x)})
   RESULT$real$vcv.all  <- msm::deltamethod(backtrans, RESULT$est.star$est, RESULT$est.star$vcv, ses=FALSE)
   RESULT$real$se.all   <- sqrt(diag( RESULT$real$vcv ))
   
   # Extract the individual VCV's of the parameters now
   # Now extact the individual estimates 
   RESULT$real$est.indiv  <- SPAS.extract.par.est(RESULT$est.red.star$est, rowDM, colDM, thetaDM, conditional=TRUE)
   RESULT$real$est.indiv$N<- RESULT$conditional$res$Nhat
   RESULT$real$est.indiv$N.stratum <- RESULT$conditional$res$Nhat.stratum

   RESULT$real$est.indiv$theta.vcv <-  RESULT$real$vcv.all[1:(s*t), 1:(s*t)]
   RESULT$real$est.indiv$theta.se  <-  matrix(RESULT$real$se.all[1:(s*t)], nrow=s)

   RESULT$real$est.indiv$cap.vcv   <-  RESULT$real$vcv.all[(s*t+1):(s*t+s), (s*t+1):(s*t+s) ]
   RESULT$real$est.indiv$cap.se    <-  RESULT$real$se.all [(s*t+1):(s*t+s)]
   
   RESULT$real$est.indiv$psi.vcv  <- RESULT$real$vcv.all[(s*t+s+1):(s*t+s+s), (s*t+s+1):(s*t+s+s) ]
   RESULT$real$est.indiv$psi.se   <- RESULT$real$se.all [(s*t+s+1):(s*t+s+s)]
   
   # Getting the variance of N (see question 11 of Plante's paper)
   # Notice that we don't find the covariance of N with any of the other parameters
   #browser()
   part1 <- t(est$psi) %*% diag(1/est$cap^2, nrow=length(est$cap), ncol=length(est$cap)) %*% 
            RESULT$real$est.indiv$cap.vcv %*%
            diag(1/est$cap^2, nrow=length(est$cap), ncol=length(est$cap)) %*% est$psi
   part2 <- sum( est$psi * ((1-est$cap)/est$cap + ((1-est$cap)/est$cap)^2 ))
   RESULT$real$est.indiv$N.vcv     <- part1 + part2
   RESULT$real$est.indiv$N.se      <- sqrt(RESULT$real$est.indiv$N.vcv)

   # Variances of individual stratum estimates (conditonal on number released)
   if( s >1){
     RESULT$real$est.indiv$N.stratum.vcv <-   rowSums(pooldata)[1:s]^2 * (1/est$cap^2)^2 * diag(RESULT$real$est.indiv$cap.vcv)
     RESULT$real$est.indiv$N.stratum.se  <-   sqrt(RESULT$real$est.indiv$N.stratum.vcv)
   }
   if( s==1){
     RESULT$real$est.indiv$N.stratum.vcv <- RESULT$real$est.indiv$N.vcv
     RESULT$real$est.indiv$N.stratum.se  <- RESULT$real$est.indiv$N.se 
   }
   
   # Estimates of the expansion factors and their vcv
   RESULT$real$est.indiv$exp.factor     <- (1-RESULT$real$est.indiv$cap)/RESULT$real$est.indiv$cap
   RESULT$real$est.indiv$exp.factor.vcv <- diag(1/est$cap^2, nrow=length(est$cap), ncol=length(est$cap)) %*% 
            RESULT$real$est.indiv$cap.vcv %*%
            diag(1/est$cap^2, nrow=length(est$cap), ncol=length(est$cap))
   if(s >  1) RESULT$real$est.indiv$exp.factor.se  <- sqrt(diag(RESULT$real$est.indiv$exp.factor.vcv))
   if(s == 1) RESULT$real$est.indiv$exp.factor.se  <- sqrt(RESULT$real$est.indiv$exp.factor.vcv)
   
   
   # Goodness-of-fit test. This is a simple chi-square test of observed vs. expected counts.
   # No pooling is done to deal with sparse counts, but a "cut off" of 0.1 is used to try
   # and eliminate cells with natural 0 counts (e.g. time stratification and t1 < t2)
   # The psi parameters are always a perfect fit and so no need to include them in the chi-square value
   # the cell counts, or the number of fitted parameters
   #browser()
   # find the component due to the movement matrix (m vs theta)
   gof.obs1 <- as.vector(RESULT$fit.setup$pooldata[1:(-1+nrow(RESULT$fit.setup$pooldata)), 1:(-1+ncol(RESULT$fit.setup$pooldata))])
   gof.exp1 <- as.vector(RESULT$real$est.indiv$theta)
   select   <- gof.exp1>chisq.cutoff
   chisq.gof <- sum((gof.obs1[select] - gof.exp1[select])^2/gof.exp1[select])
   
   # for the total number unobserved in recovery strata
   gof.obs2 <- RESULT$fit.setup$pooldata[nrow(RESULT$fit.setup$pooldata),1:(-1+ncol(RESULT$fit.setup$pooldata)) ]
   gof.exp2 <- round(as.vector((1-RESULT$real$est.indiv$cap)/RESULT$real$est.indiv$cap) %*% RESULT$real$est.indiv$theta,1) 
   select   <- gof.exp2>chisq.cutoff
   chisq.gof <- chisq.gof + sum((gof.obs2[select] - gof.exp2[select])^2/gof.exp2[select])
   chisq.gof.df <-  length(RESULT$real$est.indiv$theta) + nrow(RESULT$real$est.indiv$theta)+ncol(RESULT$real$est.indiv$theta)-RESULT$model.info$np
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
# Ouput: 
# Order of parameters is 
#       theta* = log( reduced theta parameters), i.e. as input to the design matrix
#       cap*   = log((1-p)/p) for each row
#       psi*   = log(reduced psi parameter - animals never recaptured)
#       N*    = log(N)

#
SPAS.init.est.legacy <- function(rawdata,rowDM,colDM,thetaDM, conditional) { 
  # if conditional=TRUE, then don't estimate N

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
   
   # use a least squares estimator for the log (1-cao)/p
   logit.cap.hat <- log( pmax(.01, LS(t(m),unmarked) ))
   logit.cap.hat <- rep(mean(logit.cap.hat), length(logit.cap.hat))
   
   # initial theta values prior to using design matrix
   theta.hat <- as.vector(log(m+.1))
  
   # Now to find the estimates used with the design matrices. These are least-squares estimates
   logit.cap.star.hat <- LS(rowDM, logit.cap.hat)
   theta.star.hat     <- LS(thetaDM, theta.hat)
       
   res<- c(theta.star.hat, logit.cap.star.hat, psi.star.hat)
   if(!conditional) res <- c(res, logN.hat)
   names(res) <- NULL
   return(res)
}


