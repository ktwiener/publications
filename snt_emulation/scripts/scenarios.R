## Generate all simulation data and estimate effects

library(dplyr)
library(survival)

rfiles <- "snt_emulation/R/"
dataloc <- "snt_emulation/data/"

purrr::walk(
  list.files(path = rfiles, recursive = F, full.names = T, pattern = "*.R"),
  source
  )

## Same for all scenarios
weeks <- 5
popsize <- 5000
sims <- 2000

## Changes by scenario
enc_by_sev_nomod <- c(0.4, 0.4)
enc_by_sev_mod <- c(0.2, 0.6)
homg_effect <- 0.5
hetg_effect <- c(0.3, 0.75)

## IF YOU WOULD lke to test settings, go to scratch/test_settings.R.

set.seed(24601) # RNG

## Scenario 0: null treatment effect, no confounding,
create_sim_samples(scen = 0,
                   effect = 1,
                   enc_by_sev = enc_by_sev_nomod,
                   sims = sims)

## Scenario 1: no modification, severity does not influence encounters
create_sim_samples(scen = 1,
                   effect = homg_effect,
                   enc_by_sev = enc_by_sev_nomod,
                   sims = sims)

## Scenario 2: modification, severity does not influence encounters
create_sim_samples(scen = 2,
                   effect = hetg_effect,
                   enc_by_sev = enc_by_sev_nomod,
                   sims = sims)

## Scenario 3: no modification, severity does influence encounters
create_sim_samples(scen = 3,
                   effect = homg_effect,
                   enc_by_sev = enc_by_sev_mod,
                   sims = sims)

## Scenario 4: modification, severity does influence encounters
create_sim_samples(scen = 4,
                   effect = hetg_effect,
                   enc_by_sev = enc_by_sev_mod,
                   sims = sims)


## Effect estimation
purrr::walk(
  1:4,
  estimate_effects,
  sims = sims
)
