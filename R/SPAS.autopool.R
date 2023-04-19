#' Autopooling a Stratified-Petersen (SP) data set.
#' This function applies pooling rules to pool a SPAS dataset to meeting minimum sparsity requirements
#' .
#' @template param.rawdata 
#' @template param.autopool.settings
#' 
#' @details
#' In many cases, the stratified set of releases and recapture is too sparse (many zeroes) or count
#' are very small. Pooling rows and columns may be needed.
#' 
#' Data needs to be pooled both row wise and column wise if the data are sparse 
#' to avoid singularities in the fit.  
#' This function automates pooling rows or columns following Schwarz and Taylor (1998).
#' * All rows that have 0 releases are discarded
#' * All columns that have 0 recaptures of tagged fish and 0 fish inspected are discarded
#' * Starting at the first row and working forwards in time,
#'   and then working from the final row and working backwards in time, 
#'.  rows are pooled until a minimum of \code{min.released} are released.
#'   An alternating pooling (from the top, from the bottom, from the top, etc) is used
#' * Starting at the first column and working forwards in time,
#'.   and then working from the final column and working backwards in time, 
#'    columns are pooled until a minimum of \code{min.inspected} are inspected.
#'    An alternating pooling (from the left, from the right, from the left, etc) is used.
#' * If the sum of the total recaptures from released fish is <= \code{min.recaps}, then all rows are pooled 
#' (which reduces to a Chapman estimator)
#' 
#' 
#' @return A list with a suggest pooling.
#' 
#' @export SPAS.autopool
#' @import msm MASS stats plyr TMB 
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
#' SPAS.autopool(conne.data)
#' @md


SPAS.autopool<- function( rawdata, 
                          min.released  =100,
                          min.inspected = 50,
                          min.recaps    = 50,
                          min.rows=1, min.cols=1){

# Error checking of input values.
   # is Input data ok?
   if( !is.matrix(rawdata))stop("Input raw data must be a matrix")
   if( nrow(rawdata) < 2)  stop("Input raw data needs at least 2 rows")
   if( ncol(rawdata) < 2)  stop("Input raw data needs at least 2 columns")
   if( !is.numeric(rawdata))stop("Input raw data must be numeric")
  
   check.numeric(min.rows, min.value=1, max.value=nrow(rawdata)-1, req.length=1, check.whole=TRUE)
   check.numeric(min.cols, min.value=1, max.value=ncol(rawdata)-1, req.length=1, check.whole=TRUE)
   
   if(min.rows > min.cols)stop("Min rows must be <= min.cols")

   check.numeric(min.released,  min.value=1, req.length=1, check.whole=TRUE)
   check.numeric(min.inspected, min.value=1, req.length=1, check.whole=TRUE)
   check.numeric(min.recaps,    min.value=1, req.length=1, check.whole=TRUE)
  
   if(min.rows>1 | min.cols>1)warning("**** minimum rows or columns not yet implemented.****") 
   
   spas.mat <- rawdata
   
   s <- nrow(spas.mat) - 1
   t <- ncol(spas.mat) - 1
   
   #browser()
   # get rid of rows that have 0 releases
   select <- apply(spas.mat,1,sum, na.rm=TRUE)>0
   spas.red  <- spas.mat[select,]
   # get rid of columns that are all 0
   select <- apply(spas.red,2,sum, na.rm=TRUE)>0
   spas.red  <- spas.red[,select]
   
   # recompute s and t
   s.red <- nrow(spas.red)-1
   t.red <- ncol(spas.red)-1
   
   #browser()
   # pool columns until you get at least xxxx recoveries in a stratum and you hit the minimum number of rows etc
   # we will will work from the outside in
   col.pool <- 1:t.red
   col.sum  <- colSums(spas.red)
   left.last.col <- 1
   left.cum.sum  <- 0
   right.last.col <- t.red
   right.cum.sum  <- 0
   left <- 1
   right <- t.red
   min.count <- min.inspected
   while(left < right){
     col.pool[left] <- left.last.col
     left.cum.sum <- left.cum.sum + col.sum[left]
     if(left.cum.sum > min.count){
       left.last.col <- left+1
       left.cum.sum  <- 0
     }
     left <- left + 1
     
     col.pool[right] <- right.last.col
     right.cum.sum <- right.cum.sum + col.sum[right]
     if(right.cum.sum > min.count){
       right.last.col <- right-1
       right.cum.sum  <- 0
     }
     right <- right - 1
   }
   
   # pool rows until you get at least xxx releases  in a stratum or at least min.rows
   # we will will work from the outside in
   
   row.pool <- 1:s.red
   row.sum  <- rowSums(spas.red)
   top.last.row <- 1
   top.cum.sum  <- 0
   bot.last.row <- s.red
   bot.cum.sum  <- 0
   top <- 1
   bot <- s.red
   min.count <- min.released
   while(top < bot){
     row.pool[top] <- top.last.row
     top.cum.sum <- top.cum.sum + row.sum[top]
     if(top.cum.sum > min.count){
       top.last.row <- top+1
       top.cum.sum  <- 0
     }
     top <- top + 1
     
     row.pool[bot] <- bot.last.row
     bot.cum.sum <- bot.cum.sum + row.sum[bot]
     if(bot.cum.sum > min.count){
       bot.last.row <- bot-1
       bot.cum.sum  <- 0
     }
     bot <- bot - 1
   }
   
   # check the total recaptures
   if( sum(spas.mat[1:s, 1:t])<= 50){
     row.pool[] <- 1
   }  
   
   # return the original, reduced, and pooling vectors
   list(rawdata = rawdata,
        reddata = spas.red,
        row.pool= row.pool,
        col.pool= col.pool,
        summary = cbind(rbind(spas.red, c(col.pool, NA)), c(row.pool,NA,NA))
   )

}

