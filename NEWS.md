SPAS 2025.1.2
-------------

* Added argument to SPAS.print.model to extract model information to a list object rather than to the console.
This will be useful when fitting many SPAS models and you want to process outputs programatically.


SPAS 2024.01.31
----------------

* Minor changes to fix CRAN check messages. No functional/user changes.


SPAS 2023.03.31
---------------

* Implement an autopool argument on the model fitting
* Add a function to compute autopooling for a data matrix.


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

