# 2020-01-01 CJS Now using TMB for optimization
#
#' Message to display when package is loaded
#' 
#' @keywords internal
#'

.onAttach <- function(libname,pkgname){

  packageStartupMessage("***** SPAS: Stratified Petersen Analysis System - Version 2023.3.31 2023-03-31) ***** \n\n",
        "      Help available with  help(package='SPAS') \n",
        '      Several vignettes are available. See browseVignettes(package="SPAS") \n\n')
}


# SEE http://r-pkgs.had.co.nz/src.html on why you need the following
.onUnload <- function (libpath) {
  library.dynam.unload("SPAS", libpath)
}







