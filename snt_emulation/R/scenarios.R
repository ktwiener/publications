library(dplyr)
library(survival)
rfiles <- "plans/Simulation/R/new/"

## needed files.
source(paste0(rfiles, "utils.R"))
source(paste0(rfiles, "derive_treatment_prob.R"))

## Same for all scenarios
weeks <- 3
popsize <- 5000
sims <- 500
out_by_sev_trt0 <- c(0.09, 0.16)
trt_by_sev <- c(0.2, 0.6)

## create blank canvas of possible pathways.
source(paste0(rfiles, "trajectories.R"))

## create probabilistic population.
source(paste0(rfiles, "truepop.R"))

## function to generate samples for each scenario
source(paste0(rfiles, "create_sim_samples.R"))

## estimating effects functions
source(paste0(rfiles, "effects.R"))
source(paste0(rfiles, "performance.R"))
# Settings

## Changes by scenario
enc_by_sev_nomod <- c(0.4, 0.4)
enc_by_sev_mod <- c(0.4, 0.2)
homg_effect <- 0.5
hetg_effect <- c(0.7, 0.3)

set.seed(24601) # RNG

## Scenario 1: no modification, severity does not influence encounters
create_sim_samples(out_by_sev_trt1 = out_by_sev_trt0*homg_effect,
                       enc_by_sev = enc_by_sev_nomod, 1)

## Scenario 2: modification, severity does not influence encounters
create_sim_samples(out_by_sev_trt1 = out_by_sev_trt0*hetg_effect,
                     enc_by_sev = enc_by_sev_nomod, 2)

## Scenario 3: no modification, severity does influence encounters
create_sim_samples(out_by_sev_trt1 = out_by_sev_trt0*homg_effect,
                           enc_by_sev = enc_by_sev_mod, 3)

## Scenario 4: modification, severity does influence encounters
create_sim_samples(out_by_sev_trt1 = out_by_sev_trt0*hetg_effect,
                           enc_by_sev = enc_by_sev_mod, 4)

## Estimate effects
purrr::walk(
  1:4,
  estimate_effects
)

## Performance measures
test <- purrr::map(
  1:4,
  scenario_performance
)


