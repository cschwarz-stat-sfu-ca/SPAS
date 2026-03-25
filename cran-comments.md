
## 2026.4.1 Update
* Fixed CRAN issue of non-API calls back to R in TMB code

## 2025.1.2 Update
* Added argument to SPAS.print.model to extract model information to a list object rather than to the console.
This will be useful when fitting many SPAS models and you want to process outputs programatically.


## Test environments
* local OS X install, R 4.5.2
* local R CMD build SPAS
* local R CMD check --as-cran SPAS*tar.gz
* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_mac_release()
* devtools::check_mac_devel()


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## R CMD check results
There were no ERRORs 
<<<<<<< HEAD
=======

There was one warning:

* Found the following significant warnings:
     /Library/Frameworks/R.framework/Resources/include/R_ext/Boolean.h:62:36: warning: unknown warning group '-Wfixed-enum-extension', ignored [-Wunknown-warning-option]
WARNINGs. 

This appears to be an issue with the C compiler options and not fixable by myself.
>>>>>>> b071291cdd819c60ed6f942d0e246d1335fda735

There was one warning:

<<<<<<< HEAD
* Found the following significant warnings:
     /Library/Frameworks/R.framework/Resources/include/R_ext/Boolean.h:62:36: warning: unknown warning group '-Wfixed-enum-extension', ignored [-Wunknown-warning-option]
WARNINGs. 

This appears to be an issue with the C compiler options and not fixable by myself.

There were up to 1 Notes depending on R version/OS used: 
=======
* checking installed package size ... INFO
     installed size is 13.1Mb
     sub-directories of 1Mb or more:
       libs  12.3Mb
>>>>>>> b071291cdd819c60ed6f942d0e246d1335fda735

* checking for future file timestamps ... NOTE unable to verify current time
  
  Appears to be an issue with the package builder and nothing that I can fix.

## Reverse dependencies

THe Petersen package loads the SPAS package.
