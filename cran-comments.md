## 2025.1.2 Update
* Added argument to SPAS.print.model to extract model information to a list object rather than to the console.
This will be useful when fitting many SPAS models and you want to process outputs programatically.


## Test environments
* local OS X install, R 4.4.2
* local R CMD build SPAS
* local R CMD check --as-cran SPAS*tar.gz
* devtools::check_win_release()
* devtools::check_win_devel()


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## R CMD check results
There were no ERRORs or WARNINGs. 

There were up to 1 Notes depending on R version/OS used: 

* checking installed package size ... NOTE
  installed size is 12.8Mb
  sub-directories of 1Mb or more:
    libs  12.2Mb

  Compiled Template Model Builder (TMB) code that is automatically generated
  by the TMB R package.
  

## Reverse dependencies

None.