
## Evaluate performance of estimator in each study type
sim_performance <- function(d, truth){

  truth <- truth |>
    tidyr::pivot_longer( # Transpose long
    cols = c(risk1, risk0, lnrr, rd),
    names_to = "param",
    values_to = "truth"
  )

  d |>
    tidyr::pivot_longer( # Transpose long
      cols = c(risk1, risk0, lnrr, rd),
      names_to = "param",
      values_to = "est"
    ) |>
    dplyr::left_join(truth, by = "param") |>
    dplyr::group_by(param, truth) |>
    dplyr::summarize(
      sims = max(sim), # No. of sims
      expval = mean(est), # Expected value of estimate
      bias_mcse = sqrt((1/(max(sim)-1))*mean((est - expval)^2)), # Monte-carlo SE for bias
      bias   = mean(est - truth), # Bias
      ese = sqrt((1/(max(sim)-1))*sum((est-expval)^2)), # Empirical standard error
      rmse = sqrt(mean((est - truth)^2)) # Root mean square error
    )
}


performance <- function(scen){
  truth <- readRDS(paste0(dataloc, "samples/scenario_truth_", scen, ".rds"))

  effs <- readRDS(paste0(dataloc, "effects/scenario", scen, ".rds"))
  dists <- readRDS(paste0(dataloc, "effects/distrib", scen, ".rds"))

  h <- list(
    spt_res  = effs$spt_effect |>
      standardize_emulation(dists$spt_dist, truth),

    snt_snt_res = effs$snt_effect |>
      standardize_emulation(dists$snt_dist, truth),

    snt_spt_res = effs$snt_effect |>
      standardize_emulation(dists$spt_dist, truth),

    td_td_res = effs$td_effect |>
      standardize_emulation(dists$td_dist, truth),

    td_spt_res = effs$td_effect |>
      standardize_emulation(dists$spt_dist, truth)
  )

  h |>
    saveRDS(paste0(dataloc, "performance/scenario", scen, ".rds"))

  h

}

standardize_emulation <- function(eff, dist, truth){

  crude <- eff[eff$sev=="Overall", ] |>
    sim_performance(truth)

  low <- eff[eff$sev=="Low", c("sim", "risk1", "risk0")]
  high <-eff[eff$sev=="High", c("sim", "risk1", "risk0")]

  ate <- dist |>
    dplyr::transmute(
      sim,
      risk0 = ate_low*low$risk0 + ate_high*high$risk0,
      risk1 = ate_low*low$risk1 + ate_high*high$risk1,
      rd = risk1 - risk0,
      lnrr = log(risk1/risk0)
    ) |>
    sim_performance(truth)

  att <- dist |>
    dplyr::transmute(
      sim,
      risk0 = att_low*low$risk0 + att_high*high$risk0,
      risk1 = att_low*low$risk1 + att_high*high$risk1,
      rd = risk1 - risk0,
      lnrr = log(risk1/risk0)
    ) |>
    sim_performance(truth)

  list(crude = crude, ate = ate, att = att)
}

