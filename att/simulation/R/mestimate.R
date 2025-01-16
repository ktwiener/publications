
setup_ef <- function(cht){

  a <- cht$a
  w <- cht$w
  y <- cht$y

  function(theta){

    ## Propensity score estimates
    alpha <- theta[1:2]

    ## Weighted mean estimates
    mu <- theta[3:8]

    ## Weighted effect measure estimates
    delta <- theta[9:14]

    ## Empirically estimate propensity scores
    ef_ps_w1 <- w*(a - alpha[1])
    ef_ps_w0 <- (1-w)*(a - alpha[2])
    pscore <- w*alpha[1] + (1-w)*alpha[2]

    ## Estimate weights -- using ifelse for IPW to avoid dividing by 0 with a/ps
    ipw <- ifelse(a == 1, 1/pscore, 1/(1-pscore))
    smr <- a*1 + (1-a)*pscore/(1-pscore)

    ## Horwitz-Thompson estimator
    ef_ht_r1 <- a*ipw*y - mu[1]
    ef_ht_r0 <- (1 - a)*ipw*y - mu[2]

    ## Hajek estimator
    ef_hajek_r1 <- a*ipw*(y - mu[3])
    ef_hajek_r0 <- (1 - a)*ipw*(y - mu[4])

    ## SMR estimator
    ef_smr_r1 <- a*smr*(y - mu[5])
    ef_smr_r0 <- (1 - a)*smr*(y - mu[6])

    ## Effect estimates (risk ratio)
    ef_ht_lnrr <- log(mu[1]/mu[2]) - delta[1]
    ef_hajek_lnrr <- log(mu[3]/mu[4]) - delta[2]
    ef_smr_lnrr <- log(mu[5]/mu[6]) - delta[3]

    ## Effect estimates (odds ratio)
    ef_ht_lnor <-    log(mu[1]/(1-mu[1])) -  log(mu[2]/(1-mu[2])) - delta[4]
    ef_hajek_lnor <- log(mu[3]/(1-mu[3])) -  log(mu[4]/(1-mu[4])) - delta[5]
    ef_smr_lnor <-   log(mu[5]/(1-mu[5])) -  log(mu[6]/(1-mu[6])) - delta[6]


    cbind(ef_ps_w1, ef_ps_w0, ef_ht_r1, ef_ht_r0, ef_hajek_r1, ef_hajek_r0,
          ef_smr_r1, ef_smr_r0, ef_ht_lnrr, ef_hajek_lnrr, ef_smr_lnrr,
          ef_ht_lnor, ef_hajek_lnor, ef_smr_lnor)
  }
}

############################################
# Defining estimating equation

# estimating_function <- function(beta){
#   ef_mean <- Y1 - beta[1]
#   ef_var <- (Y1-beta[1])^2 - beta[2]
#   return(cbind(ef_mean, ef_var))
# }

setup_ee <- function(estimating_function){

  function(theta){
    estf = estimating_function(theta)  # Return estimating function
    colSums(estf)                      # Estimating equations are sum
  }

}

############################################
# Root-finding
sandwich_artist <- function(estimating_equation, estimating_function, beta_root, n){
  deriv <- numDeriv::jacobian(func = estimating_equation,
                              x = beta_root)

  bread <- -1*deriv / n

  outerprod <- t(estimating_function(beta_root)) %*% estimating_function(beta_root)

  filling <- outerprod/n

  solve(bread) %*% filling %*% t(solve(bread))
}

mestimate <- function(cht){

  n <- nrow(cht)
  estimating_function <- setup_ef(cht)
  estimating_equation <- setup_ee(estimating_function)
  proc <- rootSolve::multiroot(f = estimating_equation, # Function to find root(s) of
                               # Starting values for root-finding procedure
                               start = c(0.5, 0.5, # PS
                                         0.5, 0.5, 0.1, 0.1, 0.5, 0.5, # Weighted means
                                         0, 0, 0, 0, 0, 0)) # Effect estimates
  rootm <- proc$root

  sandwich <- sandwich_artist(estimating_equation, estimating_function, rootm, n)

  se <- sqrt(diag(sandwich / n))
  res <- bind_cols(
    "params" = c("ps_w1", "ps_w0", "ef_ht_r1", "ef_ht_r0",
                 "ef_hajek_r1", "ef_hajek_r0", "ef_smr_r1", "ef_smr_r0",
                 "ef_ht_lnrr", "ef_hajek_lnrr", "ef_smr_lnrr",
                 "ef_ht_lnor", "ef_hajek_lnor", "ef_smr_lnor"),
    "roots"=rootm,
    "se"=se)
  res
}
