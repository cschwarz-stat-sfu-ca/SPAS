---
title: "Things that can go wrong"
author: "Carl James Schwarz"
date: "2019-11-18"
output:
  rmarkdown::html_vignette:
    number_sections: yes
    toc: yes
vignette: >
  %\VignetteIndexEntry{Things that can go wrong} 
  %\VignetteEngine{knitr::knitr} 
  %\VignetteEncoding{UTF-8}
---



# Things that can go wrong.
There are several types of problem that can occur that make the use of 
SPAS problematic.

# Rows are proportional
If the row in the movement matrix are proportional, then there an infinite
number of solutions. Note it is the singularity of the movement matrix 
that is problematic; the number of tags releases and not recovered (last column of
the input data) can be proportion without affecting the fit.

The model may converge to different solutions depending on your set up.
There is no automatic "flagging" of this situation and you need
to be vigilant. A diagnostic tool is the condition factor of
$XX'$ where $X$ is the movement matrix. If the rows are exactly 
proportional (or more generally if the rows are colinear) the
condition factor will be $+\infty$. 

The solution is to pool the row(s) that are proportional.

Here are some examples:


```r

test.data.csv <- textConnection("
 160   ,   120   ,     72   ,     82   ,   3592
  80   ,    60   ,     36   ,     41   ,    532
7960   ,  9720   ,   6264   ,   7934   ,   0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data
#>        V1   V2   V3   V4   V5
#> [1,]  160  120   72   82 3592
#> [2,]   80   60   36   41  532
#> [3,] 7960 9720 6264 7934    0

mod..1 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:4)
#> Using nlminb to find conditional MLE
#> outer mgc:  21233.77 
#> outer mgc:  31337.35 
#> outer mgc:  29620.78 
#> outer mgc:  7736.026 
#> outer mgc:  1637.496 
#> outer mgc:  77.12751 
#> outer mgc:  4.863 
#> outer mgc:  0.1203054 
#> outer mgc:  0.04505754 
#> outer mgc:  0.01671065 
#> Convergence codes from nlminb  0 relative convergence (4) 
#> Finding conditional estimate of N

SPAS.print.model(mod..1)
#> Model Name: No restrictions 
#>    Date of Fit: 2019-11-18 22:22 
#>    Version of OPEN SPAS used : SPAS-R 2020-01-01 
#>  
#> Raw data 
#>        V1   V2   V3   V4   V5
#> [1,]  160  120   72   82 3592
#> [2,]   80   60   36   41  532
#> [3,] 7960 9720 6264 7934    0
#> 
#> Row pooling setup : 1 2 
#> Col pooling setup : 1 2 3 4 
#> Physical pooling  : TRUE 
#> Theta pooling     : FALSE 
#> CJS pooling       : FALSE 
#> 
#>  
#> Raw data AFTER POOLING 
#>       pool1 pool2 pool3 pool4   V5
#> pool1   160   120    72    82 3592
#> pool2    80    60    36    41  532
#>        7960  9720  6264  7934    0
#> 
#>   Conditional   Log-Likelihood: 285758.5    ;  np: 12 ;  AICc: -571493 
#> 
#>   Code/Message from optimization is:  0 relative convergence (4) 
#> 
#> Estimates
#>               pool1  pool2  pool3  pool4  psi cap.prob exp factor Pop Est
#> pool1         109.1  132.2   85.1  107.7 3592    0.013       73.5  299742
#> pool2          80.0   60.0   36.0   41.0  532    1.000        0.0     749
#> est unmarked 8011.0 9708.0 6251.0 7908.0    0       NA         NA  300491
#> 
#> SE of above estimates
#>              pool1 pool2 pool3 pool4  psi cap.prob exp factor Pop Est
#> pool1          5.3   6.4   4.2   5.3 59.9    0.001        3.5   14291
#> pool2          8.9   7.7   6.0   6.4 23.1    0.000        0.0       0
#> est unmarked    NA    NA    NA    NA  0.0       NA         NA   13499
#> 
#> 
#> Chisquare gof cutoff  : 0.1 
#> Chisquare gof value   : 33.49443 
#> Chisquare gof df      : 2 
#> Chisquare gof p       : 5.330621e-08
```

In this case, it appears that the model has converged, but this is ONE of many possible 
solutions and cannot be relied upon.

The condition factor is very large (!) as expected.

```r
# Compute the condition factor of XX'

XX <- test.data[1:2, 1:4] %*% t(test.data[1:2, 1:4])
XX
#>       [,1]  [,2]
#> [1,] 51908 25954
#> [2,] 25954 12977
cat("\n\nCondition factor is\n")
#> 
#> 
#> Condition factor is
kappa(XX)
#> [1] 4.785754e+16
```


# Rows are approximately proportional
Of course with real data, it is highly unlikely that the rows in the
recovery matrix will be exactly proportional. 

The model may converge to different solutions depending on your set up.
There is no automatic "flagging" of this situation and you need
to be vigilant. The condition factor of $XX'$ may be useful.

The solution is to pool the row(s) that are proportional.

Here are some examples where the entries in row 2 are modified
slightly to make the rows only approximately proportional


```r

test.data.csv <- textConnection("
 160   ,   120   ,     72   ,     82   ,   3592
  75   ,    62   ,     38   ,     35   ,    532
7960   ,  9720   ,   6264   ,   7934   ,   0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data
#>        V1   V2   V3   V4   V5
#> [1,]  160  120   72   82 3592
#> [2,]   75   62   38   35  532
#> [3,] 7960 9720 6264 7934    0

mod..2 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:4)
#> Using nlminb to find conditional MLE
#> outer mgc:  21468.58 
#> outer mgc:  31261.98 
#> outer mgc:  29306.81 
#> outer mgc:  8081.517 
#> outer mgc:  1632.478 
#> outer mgc:  75.28523 
#> outer mgc:  4.412961 
#> outer mgc:  0.1427219 
#> outer mgc:  0.05345935 
#> outer mgc:  0.01986673 
#> Convergence codes from nlminb  0 relative convergence (4) 
#> Finding conditional estimate of N

SPAS.print.model(mod..2)
#> Model Name: No restrictions 
#>    Date of Fit: 2019-11-18 22:22 
#>    Version of OPEN SPAS used : SPAS-R 2020-01-01 
#>  
#> Raw data 
#>        V1   V2   V3   V4   V5
#> [1,]  160  120   72   82 3592
#> [2,]   75   62   38   35  532
#> [3,] 7960 9720 6264 7934    0
#> 
#> Row pooling setup : 1 2 
#> Col pooling setup : 1 2 3 4 
#> Physical pooling  : TRUE 
#> Theta pooling     : FALSE 
#> CJS pooling       : FALSE 
#> 
#>  
#> Raw data AFTER POOLING 
#>       pool1 pool2 pool3 pool4   V5
#> pool1   160   120    72    82 3592
#> pool2    75    62    38    35  532
#>        7960  9720  6264  7934    0
#> 
#>   Conditional   Log-Likelihood: 285730.4    ;  np: 12 ;  AICc: -571436.7 
#> 
#>   Code/Message from optimization is:  0 relative convergence (4) 
#> 
#> Estimates
#>               pool1  pool2  pool3  pool4  psi cap.prob exp factor Pop Est
#> pool1         109.1  132.2   85.1  107.7 3592    0.013       73.5  299742
#> pool2          75.0   62.0   38.0   35.0  532    1.000        0.0     742
#> est unmarked 8011.0 9708.0 6251.0 7908.0    0       NA         NA  300484
#> 
#> SE of above estimates
#>              pool1 pool2 pool3 pool4  psi cap.prob exp factor Pop Est
#> pool1          5.3   6.4   4.2   5.3 59.9    0.001        3.5   14291
#> pool2          8.7   7.9   6.2   5.9 23.1    0.000        0.0       0
#> est unmarked    NA    NA    NA    NA  0.0       NA         NA   13499
#> 
#> 
#> Chisquare gof cutoff  : 0.1 
#> Chisquare gof value   : 33.49443 
#> Chisquare gof df      : 2 
#> Chisquare gof p       : 5.33061e-08

# Compute the condition factor of XX'

XX <- test.data[1:2, 1:4] %*% t(test.data[1:2, 1:4])
XX
#>       [,1]  [,2]
#> [1,] 51908 25046
#> [2,] 25046 12138
cat("\n\nCondition factor is\n")
#> 
#> 
#> Condition factor is
kappa(XX)
#> [1] 1785.545
```

Now there is only one solution, but the estimate is very sensitive
to small changes in the data. Notice that the condition factor
is still very large.


# Columns that are all zero
In theory this should have no influence on the fit (see the 
section on pooling columns). 


```r

test.data.csv <- textConnection("
 160   ,   120   ,     72   ,     82   ,   0, 3592
 100   ,    45   ,     39   ,     90   ,   0,  532
7960   ,  9720   ,   6264   ,   7934   ,   0,    0  ")

test.data <- as.matrix(read.csv(test.data.csv, header=FALSE, strip.white=TRUE))
test.data
#>        V1   V2   V3   V4 V5   V6
#> [1,]  160  120   72   82  0 3592
#> [2,]  100   45   39   90  0  532
#> [3,] 7960 9720 6264 7934  0    0

mod..3 <- SPAS.fit.model(test.data,
                       model.id="No restrictions",
                       row.pool.in=1:2, col.pool.in=1:5)
#> Using nlminb to find conditional MLE
#> outer mgc:  19793.78 
#> outer mgc:  31172.76 
#> outer mgc:  29040.42 
#> outer mgc:  25712.46 
#> outer mgc:  5362.637 
#> outer mgc:  496.3737 
#> outer mgc:  8.392982 
#> outer mgc:  0.2236606 
#> outer mgc:  0.1492354 
#> outer mgc:  0.05639175 
#> outer mgc:  0.02111307 
#> outer mgc:  0.007818906 
#> outer mgc:  0.002723444 
#> outer mgc:  0.0009406725 
#> Convergence codes from nlminb  0 relative convergence (4) 
#> Finding conditional estimate of N

SPAS.print.model(mod..3)
#> Model Name: No restrictions 
#>    Date of Fit: 2019-11-18 22:22 
#>    Version of OPEN SPAS used : SPAS-R 2020-01-01 
#>  
#> Raw data 
#>        V1   V2   V3   V4 V5   V6
#> [1,]  160  120   72   82  0 3592
#> [2,]  100   45   39   90  0  532
#> [3,] 7960 9720 6264 7934  0    0
#> 
#> Row pooling setup : 1 2 
#> Col pooling setup : 1 2 3 4 5 
#> Physical pooling  : TRUE 
#> Theta pooling     : FALSE 
#> CJS pooling       : FALSE 
#> 
#>  
#> Raw data AFTER POOLING 
#>       pool1 pool2 pool3 pool4 pool5   V6
#> pool1   160   120    72    82     0 3592
#> pool2   100    45    39    90     0  532
#>        7960  9720  6264  7934     0    0
#> 
#>   Conditional   Log-Likelihood: 286003.7    ;  np: 14 ;  AICc: -571979.3 
#> 
#>   Code/Message from optimization is:  0 relative convergence (4) 
#> 
#> Estimates
#>               pool1  pool2  pool3  pool4 pool5  psi cap.prob exp factor Pop Est
#> pool1         109.1  132.2   85.1  107.7     0 3592    0.013       73.5  299742
#> pool2         100.0   45.0   39.0   90.0     0  532    1.000        0.0     806
#> est unmarked 8011.0 9708.0 6251.0 7908.0     0    0       NA         NA  300548
#> 
#> SE of above estimates
#>              pool1 pool2 pool3 pool4 pool5  psi cap.prob exp factor Pop Est
#> pool1          5.3   6.4   4.2   5.3     0 59.9    0.001        3.5   14291
#> pool2         10.0   6.7   6.2   9.5     0 23.1    0.000        0.0       0
#> est unmarked    NA    NA    NA    NA    NA  0.0       NA         NA   13499
#> 
#> 
#> Chisquare gof cutoff  : 0.1 
#> Chisquare gof value   : 33.49436 
#> Chisquare gof df      : 3 
#> Chisquare gof p       : 2.533076e-07

XX <- test.data[1:2, 1:5] %*% t(test.data[1:2, 1:5])
XX
#>       [,1]  [,2]
#> [1,] 51908 31588
#> [2,] 31588 21646
cat("\n\nCondition factor is\n")
#> 
#> 
#> Condition factor is
kappa(XX)
#> [1] 46.8607
```

Notice that the column of 0's does not affect the fit
and has no impact on the condition factor of $XX'$.


# References
Darroch, J. N. (1961). The two-sample capture-recapture census when tagging and sampling are stratified. Biometrika, 48, 241–260.
https://www.jstor.org/stable/2332748

Plante, N., L.-P Rivest, and G. Tremblay. (1988). Stratified Capture-Recapture Estimation of the Size of a Closed Population. Biometrics 54, 47-60.
https://www.jstor.org/stable/2533994

Schwarz, C. J., & Taylor, C. G. (1998). The use of the stratified-Petersen estimator in fisheries management with an illustration of estimating the number of pink salmon (Oncorhynchus gorbuscha) that return to spawn in the Fraser River. Canadian Journal of Fisheries and Aquatic Sciences, 55, 281–296.
https://doi.org/10.1139/f97-238

