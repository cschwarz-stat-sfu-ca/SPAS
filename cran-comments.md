
## 2026.12.2 Update
* Update to unarchive this package and resolved remaining check notes.
* Fixed example that was added to a function

## Test environments
* local OS X install, R 4.5.2
* local R CMD build SPAS
* local R CMD check --as-cran SPAS*tar.gz
* devtools::check_win_release()
* devtools::check_win_devel()

* usethis::use_github_action() which also checks ubuntu and latest Mac OS releases

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages


## R CMD check results
0 errors | 0 warnings | 1 note

*  checking installed package size ... NOTE
     installed size is 13.1Mb
     sub-directories of 1Mb or more:
       libs  12.3Mb
   This is expected due to the compiled TMB C++ shared library.



## Reverse dependencies

The Petersen package loads the SPAS package.
