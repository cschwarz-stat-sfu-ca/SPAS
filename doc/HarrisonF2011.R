## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries----------------------------------------------------------------
library(SPAS)

## ----loaddata-----------------------------------------------------------------
harrison.2011.chinook.F.csv <- textConnection("
  4   ,      2   ,      1   ,     1   ,     0   ,     0   ,   130
 12   ,      7   ,     14   ,     1   ,     3   ,     0   ,   330
  7   ,     11   ,     41   ,     9   ,     1   ,     1   ,   790
  1   ,     13   ,     40   ,    12   ,     9   ,     1   ,   667
  0   ,      1   ,      8   ,     8   ,     3   ,     0   ,   309
  0   ,      0   ,      0   ,     0   ,     0   ,     1   ,    65
744   ,   1187   ,   2136   ,   951   ,   608   ,   127   ,     0")

har.data <- as.matrix(read.csv(harrison.2011.chinook.F.csv, header=FALSE))
har.data

## ----fit1,results="hide"------------------------------------------------------
mod..1 <- SPAS.fit.model(har.data,
                       model.id="No restrictions",
                       row.pool.in=1:6, col.pool.in=1:6)

## ----mod1p--------------------------------------------------------------------
SPAS.print.model(mod..1)

## ----str1---------------------------------------------------------------------
cat("Names of objects at highest level\n")
names(mod..1)
cat("\n\nNames of estimates (both beta and real)\n")
names(mod..1$est)
cat("\n\nNames of real estimates\n")
names(mod..1$est$real)

## ----fit2,results="hide"------------------------------------------------------
mod..2 <- SPAS.fit.model(har.data, model.id="Pooling some rows",
                       row.pool.in=c("12","12","3","4","56","56"),
                       col.pool.in=c(1,2,3,4,56,56))

## ----mod2p--------------------------------------------------------------------
SPAS.print.model(mod..2)

## ----mod3, echo=TRUE----------------------------------------------------------
mod..3 <- SPAS.fit.model(har.data, model.id="A single row",
                       row.pool.in=rep(1, nrow(har.data)-1),
                       col.pool.in=c(1,2,3,4,56,56))
SPAS.print.model(mod..3)

## -----------------------------------------------------------------------------
mod..4 <- SPAS.fit.model(har.data, model.id="Pooled Peteren",
                       row.pool.in=rep(1, nrow(har.data)-1),
                       col.pool.in=rep(1, ncol(har.data)-1))
SPAS.print.model(mod..4)

## -----------------------------------------------------------------------------
mod..5 <- SPAS.fit.model(har.data, model.id="Logical Pooling some rows",
                       row.pool.in=c("12","12","3","4","56","56"),
                       row.physical.pool=FALSE,
                       col.pool.in=c(1,2,3,4,56,56))
SPAS.print.model(mod..5)

## -----------------------------------------------------------------------------
mod..6 <- SPAS.fit.model(har.data, model.id="A single row - Logical Pool",
                       row.pool.in=rep(1, nrow(har.data)-1), row.physical.pool=FALSE,
                       col.pool.in=c(1,2,3,4,56,56))

SPAS.print.model(mod..6)

mod..7 <- SPAS.fit.model(har.data, model.id="Pooled Peteren - Logical Pool",
                       row.pool.in=rep(1, nrow(har.data)-1), row.physical.pool=FALSE,
                       col.pool.in=rep(1, ncol(har.data)-1))

SPAS.print.model(mod..7)

## -----------------------------------------------------------------------------
mod..8 <- SPAS.fit.model(har.data, model.id="Logical Pooling pairs rows",
                       row.pool.in=c("12","12","34","34","56","56"),
                       row.physical.pool=FALSE,
                       col.pool.in=c(1,2,3,4,56,56))
SPAS.print.model(mod..8)

## ----echo=TRUE----------------------------------------------------------------
model.list <- mget( ls()[grepl("^mod\\.\\.",ls())])
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
              kappa.after.lp   = round(x$kappa.after.lp),
              Nhat             = round(x$est$real$N),
              Nhat.se          = round(x$se $real$N))
  
})
report


