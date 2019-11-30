## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

test.data.csv <- textConnection("
  86   ,     54   ,     39   ,   219
  76   ,     35   ,     45   ,   168
  24   ,     53   ,     73   ,   190
1039   ,   1148   ,   2009   ,   0")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data

## -----------------------------------------------------------------------------
library(SPAS)
mod1 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:3, col.pool.in=1:3)

SPAS.print.model(mod1)

## -----------------------------------------------------------------------------
mod2 <- SPAS.fit.model(test.data,
                           model.id="Logical pooling to single row",
                           row.pool.in=c(1,1,1), col.pool.in=1:3, row.physical.pool=FALSE)

SPAS.print.model(mod2)

## -----------------------------------------------------------------------------
mod3 <- SPAS.fit.model(test.data,
                           model.id="Physical pooling to single row",
                           row.pool.in=c(1,1,1), col.pool.in=1:3)
SPAS.print.model(mod3)

## -----------------------------------------------------------------------------
# do physical complete pooling 
mod4 <- SPAS.fit.model(test.data,
                           model.id="Physical pooling all rows and last two colum ns",
                           row.pool.in=c(1,1,1), col.pool.in=c(1,1,3))
SPAS.print.model(mod4)

## -----------------------------------------------------------------------------
# do physical complete pooling 
mod5 <- SPAS.fit.model(test.data,
                           model.id="Physical complete pooling",
                           row.pool.in=c(1,1,1), col.pool.in=c(1,1,1))
SPAS.print.model(mod5)

## -----------------------------------------------------------------------------
model.list <- mget( ls()[grepl("^mod.$",ls())])
names(model.list)

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

