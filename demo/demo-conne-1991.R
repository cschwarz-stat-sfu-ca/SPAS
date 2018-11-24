# Sample Analysis of the Conne 1991 Smolt migration 

sink("demo-conne-1991-results.txt", split=TRUE)   # Save the output to a text file
cat("\n\n\n ***** Sample  SPAS Analysis using the Conne 1991 data run on ", date(), " ***** \n\n\n")

# The experiment consisted of daily sampling, but condensed to weeks.
# Smolt were tagged and released at the upstream trap. They were recaptured
# along with untagged smolt, at the downstream trap.

# Data were stratified into weekly interval.
# The top-left 6 x6 array is the number of fish released in row statum i and 
# recovered in column stratum j. The last column is the number of fish who
# were tagged in row stratum i but never recovered. The last row is the
# number of untagged fish recovered in each stratum 

# Notice that very small number of releases and recoveries in week 6.

conne.1991.csv <- textConnection("
9  ,    21  ,     0  ,    0  ,    0  ,    0  ,   171
0  ,   101  ,    22  ,    1  ,    0  ,    0  ,   763
0  ,     0  ,   128  ,   49  ,    0  ,    0  ,   934
0  ,     0  ,     0  ,   48  ,   12  ,    0  ,   434
0  ,     0  ,     0  ,    0  ,    7  ,    0  ,    49
0  ,     0  ,     0  ,    0  ,    0  ,    0  ,     4
351,  2736  ,  3847  , 1818  ,  543  ,   191 ,     0")
 

library(SPAS)

conne.data <- as.matrix(read.csv(conne.1991.csv, header=FALSE))
conne.data

mod1 <- SPAS.fit.model(conne.data, model.id="No restrictions",
                       row.pool.in=1:6, col.pool.in=1:6,
                       theta.pool=FALSE, CJSpool=FALSE)
SPAS.print.model(mod1)   

mod2 <- SPAS.fit.model(conne.data, model.id="Early vs Late",
                       row.pool.in=c(1,1,1,2,2,2),
                       col.pool.in=c(1,1,1,2,2,2),
                       theta.pool=FALSE, CJSpool=FALSE)
SPAS.print.model(mod2)   

mod3 <- SPAS.fit.model(conne.data, model.id="Poole-Petersen",
                       row.pool.in=c(1,1,1,1,1,1),
                       col.pool.in=c(1,1,1,1,1,1),
                       theta.pool=FALSE, CJSpool=FALSE)
SPAS.print.model(mod3)


sink()


