#' Print the results from a fit of a Stratifed-Petersen (SP) model
#' 
#' This function makes a report of the results of the model fitting
#' .
#' @param x A result from the model fitting. See \code{SPAS.fit.model}.

#' @return A report to the console. Refer to the vignettes.
#' 
#' @export





SPAS.print.model = function(x){
#
#  Print out model results from the OPEN SPAS model
#
   cat("Model Name:",x$model.info$model.id,"\n")
   cat("   Date of Fit:",format(x$date,"%Y-%m-%d %H:%M"),"\n")
   cat("   Version of OPEN SPAS used :", x$version,"\n")
   cat(" \n")
   cat("Raw data \n")
   print(x$input$rawdata)
   cat("\n")
   cat("Row pooling setup :", x$input$row.pool.in, "\n")
   cat("Col pooling setup :", x$input$col.pool.in, "\n")
   cat("Theta pooling     :", x$input$theta.pool,  "\n")
   cat("CJS pooling       :", x$input$CJSpool,     "\n")
   cat("\n")
   
   cat(" \n")
   cat("Raw data AFTER POOLING \n")
   print(x$fit.setup$pooldata)
   cat("\n")
   
   cat("  Conditional   Log-Likelihood:",x$model.info$logL.cond,"   ;  np:",x$model.info$np,";  AICc:", x$model.info$AICc,"\n")
   #cat("  Unconditional Log-Likelihood:",x$model.info$logL.uncond,";  np:",x$model.info$np,";  AICc:", x$model.info$AICc,"\n")
   cat("\n")

   cat("  Code/Message from optimization is: ",
       x$conditional$res$optim.info$convergence,x$conditional$res$optim.info$message,"\n" )
   
   # Get the estimates in a nice format
   temp <- x$fit.setup$pooldata   # get the basic matrix
   S <- nrow(temp)-1
   T <- ncol(temp)-1

   temp[1:S, 1:T]  <- round(x$real$est.indiv$theta,1)
   temp[1:S,   T+1]<- round(x$real$est.indiv$psi,1);           colnames(temp)[T+1] <- "psi"
   temp <- cbind(temp,round(c(x$real$est.indiv$cap,NA),3));    colnames(temp)[T+2] <- "cap.prob"
   temp <- cbind(temp,round(c( (1-x$real$est.indiv$cap)/x$real$est.indiv$cap,NA),1)); colnames(temp)[T+3] <- "exp factor"
   temp[S+1,1:T] <-   round(as.vector((1-x$real$est.indiv$cap)/x$real$est.indiv$cap) %*% x$real$est.indiv$theta,1)  ;   rownames(temp)[S+1] <- "est unmarked" 
   temp<- cbind(temp, round(c(x$real$est.indiv$N.stratum, x$real$est.indiv$N)))     ; colnames(temp)[T+4] <- "Pop Est"
   cat("\nEstimates\n")
   print(temp)
   
   # get the se's of the estimates
   cat("\nSE of above estimates\n")
   temp <- x$fit.setup$pooldata
   temp[1:S, 1:T]  <- round(x$real$est.indiv$theta.se,1)
   temp[1:S,   T+1]<- round(x$real$est.indiv$psi.se,1);           colnames(temp)[T+1] <- "psi"
   temp <- cbind(temp,round(c(x$real$est.indiv$cap.se,NA),3));    colnames(temp)[T+2] <- "cap.prob"
   temp <- cbind(temp,round(c(x$real$est.indiv$exp.factor.se,NA),1));   colnames(temp)[T+3] <- "exp factor"
   temp[S+1,1:T] <-   as.vector(rep(NA,T) )                    ;   rownames(temp)[S+1] <- "est unmarked" 
   temp<- cbind(temp, round(c(x$real$est.indiv$N.stratum.se, x$real$est.indiv$N.se))); colnames(temp)[T+4] <- "Pop Est"
   print(temp)
 }
