# Simulation settings
#
# This file contains the settings that will be applied to the simulation.

set_settings2 <- function(w, effects = c("None", "Homogeneous", "Heterogeneous")){
  tibble::tribble(
    ~label,                    ~effect,         ~delta    ,  ~awcoef,
    "Full exchangeability"   ,  "None"         ,  log(1)  ,       log(3),
    "Partial exchangeability",  "None"         ,  log(1)  ,       Inf,
    "Full exchangeability"   ,  "Homogeneous"  ,  deff,           log(3),
    "Partial exchangeability",  "Homogeneous"  ,  deff,           Inf,
     "Full exchangeability"   ,  "Heterogeneous",  log(1),      log(3),
     "Partial exchangeability",  "Heterogeneous",  log(1),      Inf
  ) %>%
    dplyr::mutate(
      sims = sims,
      n = n,
      pa = pa,
      wycoef = log(8),
      yint = logit(0.06),
      eta = if_else(effect == "Heterogeneous", log(0.6) - delta, 0),
      pw = w
    ) %>%
    dplyr::filter(effect %in% effects)
}


