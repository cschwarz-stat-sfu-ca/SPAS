## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(SPAS)  


## -----------------------------------------------------------------------------

test.data.csv <- textConnection("
 160   ,   120   ,     72   ,     82   ,   3592
  80   ,    60   ,     36   ,     41   ,    532
7960   ,  9720   ,   6264   ,   7934   ,   0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data

mod..1 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:4)

SPAS.print.model(mod..1)

## -----------------------------------------------------------------------------
# Compute the condition number of XX'

XX <- test.data[1:2, 1:4] %*% t(test.data[1:2, 1:4])
XX
cat("\n\nCondition number is\n")
kappa(XX)

## ----echo=TRUE----------------------------------------------------------------
mod..2 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=c(1,1), col.pool.in=1:4, 
                       row.physical.pool=FALSE)

SPAS.print.model(mod..2)

## -----------------------------------------------------------------------------

test.data.csv <- textConnection("
 160   ,   120   ,     72   ,     82   ,   3592
  75   ,    62   ,     38   ,     35   ,    532
7960   ,  9720   ,   6264   ,   7934   ,   0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data

mod..2 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:4)

SPAS.print.model(mod..2)

# Compute the condition number of XX'

XX <- test.data[1:2, 1:4] %*% t(test.data[1:2, 1:4])
XX
cat("\n\nCondition number is\n")
kappa(XX)

## -----------------------------------------------------------------------------

test.data.csv <- textConnection("
 160   ,   120   ,     72   ,     82   ,   0, 3592
 100   ,    45   ,     39   ,     90   ,   0,  532
7960   ,  9720   ,   6264   ,   7934   ,   0,    0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data

mod..3 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:5)

SPAS.print.model(mod..3)

XX <- test.data[1:2, 1:5] %*% t(test.data[1:2, 1:5])
XX
cat("\n\nCondition number is\n")
kappa(XX)

