## 2023 Update
* Additional functionality added.

## Test environments
* local OS X install, R 4.3.2
* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_rhub()

## R CMD check results
There were no ERRORs or WARNINGs. 

There were up to 2 Notes depending on R version/OS used: 

* checking installed package size ... NOTE
  installed size is 12.8Mb
  sub-directories of 1Mb or more:
    libs  12.2Mb

  Compiled Template Model Builder (TMB) code that is automatically generated
  by the TMB R package.
  
* Found the following (possibly) invalid URLs:
  URL: https://www.jstor.org/stable/2332748
    From: DESCRIPTION
    Status: 403
    Message: Forbidden
  URL: https://www.jstor.org/stable/2533994
    From: DESCRIPTION
    Status: 403
    Message: Forbidden
    
  These are valid URLs
  
## Reverse dependencies

None.