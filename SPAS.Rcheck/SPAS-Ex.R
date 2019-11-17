pkgname <- "SPAS"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "SPAS-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('SPAS')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("SPAS.fit.model")
### * SPAS.fit.model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SPAS.fit.model
### Title: Fit a Stratifed-Petersen (SP) model
### Aliases: SPAS.fit.model

### ** Examples

conne.data.csv <- textConnection("
9  ,    21  ,     0  ,    0  ,    0  ,    0  ,   171
0  ,   101  ,    22  ,    1  ,    0  ,    0  ,   763
0  ,     0  ,   128  ,   49  ,    0  ,    0  ,   934
0  ,     0  ,     0  ,   48  ,   12  ,    0  ,   434
0  ,     0  ,     0  ,    0  ,    7  ,    0  ,    49
0  ,     0  ,     0  ,    0  ,    0  ,    0  ,     4
351,  2736  ,  3847  , 1818  ,  543  ,   191 ,     0")
conne.data <- as.matrix(read.csv(conne.data.csv, header=FALSE))
close(conne.data.csv)
 
mod1 <- SPAS.fit.model(conne.data, model.id="Pooling rows 1/2, 5/6; pooling columns 5/6",
                      row.pool.in=c("12","12","3","4","56","56"),
                      col.pool.in=c(1,2,3,4,56,56),
                      optMethod.control=list(ftol=.0001))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SPAS.fit.model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("SPAS.print.model")
### * SPAS.print.model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: SPAS.print.model
### Title: Print the results from a fit of a Stratifed-Petersen (SP) model
### Aliases: SPAS.print.model

### ** Examples

conne.data.csv <- textConnection("
9  ,    21  ,     0  ,    0  ,    0  ,    0  ,   171
0  ,   101  ,    22  ,    1  ,    0  ,    0  ,   763
0  ,     0  ,   128  ,   49  ,    0  ,    0  ,   934
0  ,     0  ,     0  ,   48  ,   12  ,    0  ,   434
0  ,     0  ,     0  ,    0  ,    7  ,    0  ,    49
0  ,     0  ,     0  ,    0  ,    0  ,    0  ,     4
351,  2736  ,  3847  , 1818  ,  543  ,   191 ,     0")
conne.data <- as.matrix(read.csv(conne.data.csv, header=FALSE))
close(conne.data.csv)
 
mod1 <- SPAS.fit.model(conne.data, model.id="Pooling rows 1/2, 5/6; pooling columns 5/6",
                      row.pool.in=c("12","12","3","4","56","56"),
                      col.pool.in=c(1,2,3,4,56,56),
                      optMethod.control=list(ftol=.0001))

SPAS.print.model(mod1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("SPAS.print.model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
