# SPAS
Stratified Petersen Analysis System in R

## Versions and installation

  * **CRAN**  Download the **SPAS** package

  * **Github** To install the latest development version from Github, 
    install the newest version of the **devtools** package; then run
```
devtools::install_github("cschwarz-stat-sfu-ca/SPAS", dependencies = TRUE,
                        build_vignettes = TRUE)
```

## Features
This is an R version of the Windoze program SPAS to estimate population abundance
using a Stratified Petersen Estimator (Darroch 1961; Plante et al 1998; Schwarz and Taylor, 1998)

The user is allows to pool rows and/or columns prior to analysis but the number of rows must be
less than or equal to the number of columns (s <= t). The conditional likelihood formulation of
Plante et al (1998) is used to estimate the parameters. 

A good discussion 
of how to decide on pooling rows/columns is found in Schwarz and Taylor (1998). The *row.physical.pool*
parameter allows you to choose between physical pooling of rows, or logical pooling of rows (the underlying
data table is unchanged, but capture probabilities for the pooled rows are forced equal). It is not possible
to do logical pooling of columns and only physical pooling is possible. See the help() function 
for details.

If the data are physically pooled prior to analysis, it is not possible to compare different
poolings to see which is most appropriate using AIC or likelihood ratio tests. If you do logical 
pooling of rows, you can compare poolings using AIC or likelihood ratio methods.

Optimization is now done using the TMB package (a relative of ADMB) which seems to have
fixed the convergence issues that plagued earlier versions of SPAS-R.

## References
Darroch, J. N. (1961). The two-sample capture-recapture census when tagging and sampling are stratified. Biometrika, 48, 241-260.
https://www.jstor.org/stable/2332748

Plante, N., L.-P Rivest, and G. Tremblay. (1988). Stratified Capture-Recapture Estimation of the Size of a Closed Population. Biometrics 54, 47-60.
https://www.jstor.org/stable/2533994

Schwarz, C. J., & Taylor, C. G. (1998). The use of the stratified-Petersen estimator in fisheries management with an illustration of estimating the number of pink salmon (Oncorhynchus gorbuscha) that return to spawn in the Fraser River. Canadian Journal of Fisheries and Aquatic Sciences, 55, 281-296.
https://doi.org/10.1139/f97-238

