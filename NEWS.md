SPAS 2020.1.1
-------------

  * Optimization using TMB package. This speeds up optimization considerably and
    reduces the number of non-convergence issues.
  * Implement logical pooling of rows. Because data are not physically modified
    you can look at different row pooling using AIC. You cannot compare column
    pooling using any method. See help files for details.
  * Chi-square goodness of fit test implemented.
  * The structure of the fitted model object has changed. This may break some 
    of your code.


SPAS 2018.11.22
-------------

  * First release of conversion of Windoze SPAS to R SPAS

