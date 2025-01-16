
#' Create simulated population
#'
#' @description This function creates the full simulated population under a
#' simulation scenario.
#'
#' @param sims number of simulations
#' @param n    sample size for each simulation
#' @param pa   prevalence of treatment in population
#' @param alpha stabilizing intercept
#' @param delta log-odds treatment effect
#' @param pw_a1 prevalence of confounder in the treated
#' @param pw_a0 prevalence of confounder in the untreated
#' @return A simulated population
#' @export
#'

## Using balancing intercepts

popgen2 <- function(label, effect, pw, sims, n, pa, awcoef, delta, wycoef, yint, eta, writeout = T, measure = "rr"){

  meas_fxn <- `if`(
    measure == "rr",
    exp,
    plogis
  )

  ns <- sims * n # Full population size

  ## Generate W
  w <- rbern(ns, pw)


  if (awcoef != Inf){
    ## For full exchangeability
    ## Find root for exposure model
    gen_pr_a <- gen_pr_a_wrapper(w, awcoef)
    roota <- multiroot(f=tosolve, start=c(-1), fxname=gen_pr_a, goalmarg=pa, atol=1e-12)$root

    ## Generate A
    probs <- gen_pr_a(roota)

    pa_w1 = probs[w==1][1]
    pa_w0 = probs[w==0][1]

    a <- rbinom(ns, 1, probs)
  } else{
    ## For partial exchangeability.
    pa_w1 = pa/pw
    pa_w0 = 0
    roota <- NA
    a <- rbern(ns, w*pa_w1 + (1-w)*pa_w0)
  }

  pop <- dplyr::tibble(
    scenario = label,
    effect = effect,
    pw = pw,
    delta = delta,
    eta = eta,
    ## Simulation number to group by
    sim   = rep(1:sims, each = n),
    a = a,
    w = w,
    y0 = rbern(ns, meas_fxn(yint + wycoef*w)),
    y1 = rbern(ns, meas_fxn(yint + wycoef*w + delta + eta*w)),
    y = a*y1 + (1-a)*y0,
    roota = roota,
    pa_w0 = pa_w0,
    pa_w1 = pa_w1
  )

  if (writeout){
    dir.create(sprintf("data/population/%s/%s", Sys.Date(), measure), showWarnings = F, recursive = T)

    write.csv(pop, sprintf("data/population/%s/%s/%s-%s-%s.csv", Sys.Date(), measure,
                         tolower(stringr::word(label, 1)), tolower(effect),
                         100*as.numeric(pw)),
              row.names = FALSE)
    return(list(pa_w0 = pa_w0, pa_w1 = pa_w1, roota = roota))
  }

  else return(pop)

}

tosolve <- function(ints, fxname, goalmarg){
  values <- fxname(ints)                          # simulated individual expected values at given intercept value

  if(is.null(dim(values))){                        # obtain mean of simulated individual expected values
    empmeans <-  mean(values)
  }else{
    empmeans <-  colMeans(values)
  }

  diff = empmeans - goalmarg                      # difference from empirical mean to goal marginal distribution
  return(diff)
}

