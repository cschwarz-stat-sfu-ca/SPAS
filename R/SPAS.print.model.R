#' Print the results from a fit of a Stratifed-Petersen (SP) model when using the TMB optimizer
#' 
#' This function makes a report of the results of the model fitting
#' .
#' @param x A result from the model fitting. See \code{SPAS.fit.model}.

#' @return A report to the console. Refer to the vignettes.
#'
#' @export
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
#'
#' SPAS.print.model(mod1)




SPAS.print.model = function(x){
#
#  Print out model results from the OPEN SPAS model fit using TMB
#
#  Check that object is of the new form
   if( x$version<"SPAS-R 2020-01-01"){
       stop("Object was created using previous version of SPAS. Use SPAS.print.model.legacy()")
   }
   cat("Model Name:",x$model.info$model.id,"\n")
   cat("   Date of Fit:",format(x$date,"%Y-%m-%d %H:%M"),"\n")
   cat("   Version of OPEN SPAS used :", x$version,"\n")
   cat(" \n")
   cat("Raw data \n")
   print(x$input$rawdata)
   cat("\n")
   cat("Row pooling setup :", x$input$row.pool.in, "\n")
   cat("Col pooling setup :", x$input$col.pool.in, "\n")
   cat("Physical pooling  :", x$input$row.physical.pool, "\n")
   cat("Theta pooling     :", x$input$theta.pool,  "\n")
   cat("CJS pooling       :", x$input$CJSpool,     "\n")
   cat("\n\n")
   
   cat("Chapman estimator of population size ",round(x$est$N.Chapman),
       ' (SE ', round(x$se$N.Chapman), " )\n")
   cat(" \n\n")
   cat("Raw data AFTER PHYSICAL (but not logical) POOLING \n")
   print(x$fit.setup$pooldata)
   cat("\n")
   cat("Condition number of XX' where X= (physically) pooled matrix is ", x$kappa, "\n")
   cat("Condition number of XX' after logical pooling                  ", x$kappa.after.lp, "\n")
   cat("\n")
   cat("Large value of kappa (>1000) indicate that rows are approximately proportional which is not good\n")
   cat("\n")
   
   cat("  Conditional   Log-Likelihood:",x$model.info$logL.cond,"   ;  np:",x$model.info$np,";  AICc:", x$model.info$AICc,"\n")
   cat("\n")

   cat("  Code/Message from optimization is: ",
       x$conditional$res$optim.info$convergence,x$conditional$res$optim.info$message,"\n" )
   
   # Get the estimates in a nice format
   temp <- x$fit.setup$pooldata   # get the basic matrix
   S <- nrow(temp)-1
   T <- ncol(temp)-1

   temp[1:S, 1:T]  <- round(x$est$real$theta,1)
   temp[1:S,   T+1]<- round(x$est$real$psi,1);                colnames(temp)[T+1] <- "psi"
   temp <- cbind(temp,round(c(x$est$real$cap,       NA),3));  colnames(temp)[T+2] <- "cap.prob"
   temp <- cbind(temp,round(c(x$est$real$exp.factor,NA),1));  colnames(temp)[T+3] <- "exp factor"
   temp[S+1,1:T] <-   round(as.vector((1-x$est$real$cap)/x$est$real$cap) %*% matrix(x$est$real$theta,nrow=S, ncol=T)) ;   rownames(temp)[S+1] <- "est unmarked" 
   temp<- cbind(temp, round(c(x$est$real$N.stratum, x$est$real$N)))     ; colnames(temp)[T+4] <- "Pop Est"
   cat("\nEstimates\n")
   print(temp)
   
   # get the se's of the estimates
   cat("\nSE of above estimates\n")
   temp <- x$fit.setup$pooldata
   temp[1:S, 1:T]  <- round(x$se$real$theta,1)
   temp[1:S,   T+1]<- round(x$se$real$psi,1);                 colnames(temp)[T+1] <- "psi"
   temp <- cbind(temp,round(c(x$se$real$cap,NA),3));          colnames(temp)[T+2] <- "cap.prob"
   temp <- cbind(temp,round(c(x$se$real$exp.factor,NA),1));   colnames(temp)[T+3] <- "exp factor"
   temp[S+1,1:T] <-   as.vector(rep(NA,T) )                    ;   rownames(temp)[S+1] <- "est unmarked" 
   temp<- cbind(temp, round(c(x$se$real$N.stratum, x$se$real$N))); colnames(temp)[T+4] <- "Pop Est"
   print(temp)
   
   # goodness of fit statistics
   cat("\n\n")
   cat("Chisquare gof cutoff  :", x$input$chisq.cutoff, "\n")
   cat("Chisquare gof value   :", x$gof$chisq,          "\n")
   cat("Chisquare gof df      :", x$gof$chisq.df,       "\n")
   cat("Chisquare gof p       :", x$gof$chisq.p,        "\n")
   
 }
