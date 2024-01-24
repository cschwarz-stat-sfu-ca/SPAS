pkgname <- "Petersen"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('Petersen')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("LP_AICc")
### * LP_AICc

flush(stderr()); flush(stdout())

### Name: LP_AICc
### Title: Create an AIC table comparing multiple LP fits
### Aliases: LP_AICc

### ** Examples


data(data_rodli)
mt <- Petersen::LP_fit(data=data_rodli, p_model=~..time)
m0 <- Petersen::LP_fit(data=data_rodli, p_model=~1)
Petersen::LP_AICc(m0,mt)




cleanEx()
nameEx("LP_BTSPAS_est")
### * LP_BTSPAS_est

flush(stderr()); flush(stdout())

### Name: LP_BTSPAS_est
### Title: Extract estimates of abundance after BTSPAS fit
### Aliases: LP_BTSPAS_est

### ** Examples






cleanEx()
nameEx("LP_BTSPAS_fit_Diag")
### * LP_BTSPAS_fit_Diag

flush(stderr()); flush(stdout())

### Name: LP_BTSPAS_fit_Diag
### Title: Wrapper (*_fit) to call the Time Stratified Petersen Estimator
###   with Diagonal Entries function in BTSPAS.
### Aliases: LP_BTSPAS_fit_Diag

### ** Examples





cleanEx()
nameEx("LP_BTSPAS_fit_NonDiag")
### * LP_BTSPAS_fit_NonDiag

flush(stderr()); flush(stdout())

### Name: LP_BTSPAS_fit_NonDiag
### Title: Wrapper (*_fit) to call the Time Stratified Petersen Estimator
###   with NON-Diagonal Entries function in BTSPAS.
### Aliases: LP_BTSPAS_fit_NonDiag

### ** Examples




cleanEx()
nameEx("LP_CL_fit")
### * LP_CL_fit

flush(stderr()); flush(stdout())

### Name: LP_CL_fit
### Title: Fit the Chen-Lloyd model to estimate abundance using a
###   non-parametric smoother for a covariates
### Aliases: LP_CL_fit

### ** Examples


library(Petersen)
data(data_NorthernPike)
res <- LP_CL_fit(data_NorthernPike, "length")
res$summary



cleanEx()
nameEx("LP_IS_est")
### * LP_IS_est

flush(stderr()); flush(stdout())

### Name: LP_IS_est
### Title: Estimate abundance after the LP_IS conditional likelihood fit.
### Aliases: LP_IS_est

### ** Examples


data(data_wae_is_short)
fit <- Petersen::LP_IS_fit(data=data_wae_is_short, p_model=~..time)
fit$summary
est <- LP_IS_est(fit, N_hat=~1)
est$summary



cleanEx()
nameEx("LP_IS_fit")
### * LP_IS_fit

flush(stderr()); flush(stdout())

### Name: LP_IS_fit
### Title: Fit a Lincoln-Petersen Model with incomplete stratification
### Aliases: LP_IS_fit

### ** Examples


data(data_wae_is_short)
fit <- Petersen::LP_IS_fit(data=data_wae_is_short, p_model=~..time)
fit$summary
est <- LP_IS_est(fit, N_hat=~1)
est$summary




cleanEx()
nameEx("LP_IS_print")
### * LP_IS_print

flush(stderr()); flush(stdout())

### Name: LP_IS_print
### Title: Print the results from a fit a Lincoln-Petersen Model with
###   incomplete stratification
### Aliases: LP_IS_print

### ** Examples


data(data_wae_is_short)
res <- Petersen::LP_IS_fit(data=data_wae_is_short, p_model=~-1 + ..cat:..time)
LP_IS_print(res)




cleanEx()
nameEx("LP_SPAS_est")
### * LP_SPAS_est

flush(stderr()); flush(stdout())

### Name: LP_SPAS_est
### Title: Extract estimates of abundance after SPAS fit
### Aliases: LP_SPAS_est

### ** Examples


data(data_spas_harrison)
fit <- Petersen::LP_SPAS_fit(data=data_spas_harrison,
                              model.id="Pooling rows 5/6",
                              row.pool.in=c(1,2,3,4,56,56),
                              col.pool.in=c(1,2,3,4,5,6))
fit$summary
est <- Petersen::LP_SPAS_est(fit)
est$summary



cleanEx()
nameEx("LP_SPAS_fit")
### * LP_SPAS_fit

flush(stderr()); flush(stdout())

### Name: LP_SPAS_fit
### Title: Fit a Stratified-Petersen SPAS model.
### Aliases: LP_SPAS_fit

### ** Examples

data(data_spas_harrison)

fit <- Petersen::LP_SPAS_fit(data=data_spas_harrison,
                              model.id="Pooling rows 5/6",
                              row.pool.in=c(1,2,3,4,56,56),
                              col.pool.in=c(1,2,3,4,5,6),quietly=TRUE)
fit$summary
est <- Petersen::LP_SPAS_est(fit)
est$summary

# make a nice report using the SPAS package functions
SPAS::SPAS.print.model(fit$fit)




cleanEx()
nameEx("LP_TL_est")
### * LP_TL_est

flush(stderr()); flush(stdout())

### Name: LP_TL_est
### Title: Estimate abundance after the LP_TL (tag loss) conditional
###   likelihood fit.
### Aliases: LP_TL_est

### ** Examples


data(data_kokanee_tagloss)
fit <- Petersen::LP_TL_fit(data=data_kokanee_tagloss, p_model=~1, rho_model=~1, dt_type="notD")
fit$summary
est <- Petersen::LP_TL_est(fit, N_hat=~1)
est$summary



cleanEx()
nameEx("LP_TL_fit")
### * LP_TL_fit

flush(stderr()); flush(stdout())

### Name: LP_TL_fit
### Title: Fit a Lincoln-Petersen Model with Tag Loss using conditional
###   likelihood
### Aliases: LP_TL_fit

### ** Examples


data(data_kokanee_tagloss)
fit <- Petersen::LP_TL_fit(data=data_kokanee_tagloss, p_model=~1, rho_model=~1, dt_type="notD")
fit$summary
est <- Petersen::LP_TL_est(fit, N_hat=~1)
est$summary




cleanEx()
nameEx("LP_TL_simulate")
### * LP_TL_simulate

flush(stderr()); flush(stdout())

### Name: LP_TL_simulate
### Title: Simulate data from a Lincoln-Petersen Model with Tag Loss
### Aliases: LP_TL_simulate

### ** Examples


sim_data <-LP_TL_simulate(
      dt_type="t2perm",  # permanent tag
      N=1000,
      cov1=function(N)         {rep(1,N)},
      cov2=function(cov1)      {rep(1,  length(cov1))},
      p1  =function(cov1, cov2){rep(.1, length(cov1))},
      pST =function(cov1, cov2){rep(.25,length(cov1))},
      rho1=function(cov1, cov2){rep(.70,length(cov1))},
      rho2=function(cov1, cov2){rep(1,  length(cov1))},  # permanent second tag
      p2  =function(cov1, cov2){rep(.1, length(cov1))},
      seed=round(1000000*runif(1)))
sim_data




cleanEx()
nameEx("LP_est")
### * LP_est

flush(stderr()); flush(stdout())

### Name: LP_est
### Title: Estimate abundance after the LP conditional likelihood fit.
### Aliases: LP_est

### ** Examples


data(data_rodli)
fit <- Petersen::LP_fit(data=data_rodli, p_model=~..time)
fit$summary
est <- Petersen::LP_est(fit, N_hat=~1)
est$summary



cleanEx()
nameEx("LP_est_adjust")
### * LP_est_adjust

flush(stderr()); flush(stdout())

### Name: LP_est_adjust
### Title: Estimate abundance after empirical adjustments for various
###   factors.
### Aliases: LP_est_adjust

### ** Examples


data(data_rodli)
rodli.fit <- Petersen::LP_fit(data=data_rodli, p_model=~..time)
rodli.est <- Petersen::LP_est(rodli.fit)
res <- Petersen::LP_est_adjust(rodli.est$summary$N_hat, rodli.est$summary$N_hat_SE,
          tag.retention.est=.90, tag.retention.se=.05)
res$summary



cleanEx()
nameEx("LP_fit")
### * LP_fit

flush(stderr()); flush(stdout())

### Name: LP_fit
### Title: Fit a Lincoln-Petersen Model using conditional likelihood
### Aliases: LP_fit

### ** Examples


data(data_rodli)
fit <- Petersen::LP_fit(data=data_rodli, p_model=~..time)
fit$summary
res <- Petersen::LP_est(fit, N_hat=~1)
res$summary




cleanEx()
nameEx("LP_modavg")
### * LP_modavg

flush(stderr()); flush(stdout())

### Name: LP_modavg
### Title: Create an table of individual estimates and the model averaged
###   values
### Aliases: LP_modavg

### ** Examples


data(data_rodli)
mt <- Petersen::LP_fit(data=data_rodli, p_model=~..time)
m0 <- Petersen::LP_fit(data=data_rodli, p_model=~1)
Petersen::LP_modavg(m0,mt)




cleanEx()
nameEx("LP_summary_stats")
### * LP_summary_stats

flush(stderr()); flush(stdout())

### Name: LP_summary_stats
### Title: Compute summary statistics from the capture histories
### Aliases: LP_summary_stats

### ** Examples


data(data_rodli)
LP_summary_stats(data_rodli)




cleanEx()
nameEx("LP_test_equal_mf")
### * LP_test_equal_mf

flush(stderr()); flush(stdout())

### Name: LP_test_equal_mf
### Title: Test for equal marked fractions in LP experiment
### Aliases: LP_test_equal_mf

### ** Examples


data(data_NorthernPike)
LP_test_equal_mf(data_NorthernPike, "Sex")




cleanEx()
nameEx("LP_test_equal_recap")
### * LP_test_equal_recap

flush(stderr()); flush(stdout())

### Name: LP_test_equal_recap
### Title: Test for equal recapture probability in LP experiment
### Aliases: LP_test_equal_recap

### ** Examples


data(data_NorthernPike)
LP_test_equal_recap(data_NorthernPike, "Sex")




cleanEx()
nameEx("cap_hist_to_n_m_u")
### * cap_hist_to_n_m_u

flush(stderr()); flush(stdout())

### Name: cap_hist_to_n_m_u
### Title: Convert capture history data to n, m and u for use in BTSPAS
### Aliases: cap_hist_to_n_m_u

### ** Examples


data(data_btspas_diag1)
cap_hist_to_n_m_u(data_btspas_diag1)

data(data_btspas_nondiag1)
cap_hist_to_n_m_u(data_btspas_nondiag1)





cleanEx()
nameEx("logit")
### * logit

flush(stderr()); flush(stdout())

### Name: logit
### Title: Logit and anti-logit function.
### Aliases: logit expit
### Keywords: ~misc

### ** Examples


##---- compute the logit and its inverse
logitp <- logit(.3)
p <- expit(-.84)




cleanEx()
nameEx("split_cap_hist")
### * split_cap_hist

flush(stderr()); flush(stdout())

### Name: split_cap_hist
### Title: Split a vector of capture histories into a matrix with one
###   column for each occasion
### Aliases: split_cap_hist

### ** Examples


# standard 2 character capture histor
data(data_rodli)
Petersen::split_cap_hist(data_rodli$cap_hist)

# history vector with ".." separating the fields
test <- c("1..1","1..0")
split_cap_hist(test, sep=stringr::fixed(".."))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
