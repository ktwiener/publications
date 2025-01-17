


## needed files.
source(paste0(rfiles, "utils.R"))
source(paste0(rfiles, "derive_treatment_prob.R"))

## create probabilistic population.
source(paste0(rfiles, "truepop.R"))

## function to generate samples for each scenario
source(paste0(rfiles, "create_sim_samples.R"))

## estimating effects functions
source(paste0(rfiles, "estimate_effects.R"))
source(paste0(rfiles, "performance.R"))
