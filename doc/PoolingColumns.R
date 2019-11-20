## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
  
# define reporting and removal functions
get.models <- function(pattern){
    # get the list of models according to the pattern specified
    #browser()
    model.list <- mget( ls(envir=globalenv())[grepl(pattern,ls(envir=globalenv()))], envir=globalenv())
    model.list
}


make.report <- function(model.list){
   # make a little report
   report <- plyr::ldply(model.list, function(x){
      #browser()
      data.frame(#version=x$version,
         date   = as.Date(x$date),
         model.id         = x$model.info$model.id,
         s.a.pool         =-1+nrow(x$fit.setup$pooldata),
         t.p.pool         =-1+ncol(x$fit.setup$pooldata),
         logL.cond        = x$model.info$logL.cond,
         np               = x$model.info$np,
         AICc             = x$model.info$AICc,
         gof.chisq        = round(x$gof$chisq,1),
         gof.df           = x$gof$chisq.df,
         gof.p            = round(x$gof$chisq.p,3),
         Nhat             = round(x$est$real$N),
         Nhat.se          = round(x$se $real$N))
      
   })
   report
}

remove.models <- function(model.list){
    rm(list=names(model.list), envir=globalenv())
}


## -----------------------------------------------------------------------------

test.data.csv <- textConnection("
 160   ,   127   ,     72   ,     82   ,   3592
  24   ,    66   ,     13   ,     10   ,    532
7960   ,  9720   ,   6264   ,   7934   ,   0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data

## -----------------------------------------------------------------------------
library(SPAS)
mod..1 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:4)

SPAS.print.model(mod..1)

## -----------------------------------------------------------------------------
mod..2 <- SPAS.fit.model(test.data,
                           model.id="Pool last two columns",
                           row.pool.in=c(1,2), col.pool.in=c(1,2,34,34))

SPAS.print.model(mod..2)

## -----------------------------------------------------------------------------
mod..3 <- SPAS.fit.model(test.data,
                           model.id="Pool last two columns",
                           row.pool.in=c(1,2), col.pool.in=c(12,22,34,34))

SPAS.print.model(mod..3)

## ----echo=FALSE---------------------------------------------------------------
model.list <- get.models("^mod\\.\\.")
make.report(model.list)
remove.models(model.list)


## -----------------------------------------------------------------------------
mod..3 <- SPAS.fit.model(test.data,
                           model.id="Physical pooling to single row",
                           row.pool.in=c(1,1), col.pool.in=1:4)
SPAS.print.model(mod..3)

## -----------------------------------------------------------------------------
mod..3a <- SPAS.fit.model(test.data,
                           model.id="Logical pooling to single row",
                           row.pool.in=c(1,1), col.pool.in=1:4, row.physical.pool=FALSE)
SPAS.print.model(mod..3a)

## -----------------------------------------------------------------------------
# do physcial complete pooling 
mod..4 <- SPAS.fit.model(test.data,
                           model.id="Physical pooling all rows and last two columns",
                           row.pool.in=c(1,1), col.pool.in=c(12,12,34,34))
SPAS.print.model(mod..4)

## -----------------------------------------------------------------------------
# do physcial complete pooling 
mod..5 <- SPAS.fit.model(test.data,
                           model.id="Physical complete pooling",
                           row.pool.in=c(1,1), col.pool.in=c(1,1,1,1))
SPAS.print.model(mod..5)

## ----echo=FALSE---------------------------------------------------------------
model.list <- get.models("^mod\\.\\.")
make.report(model.list)
remove.models(model.list)


