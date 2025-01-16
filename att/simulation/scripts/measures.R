## Getting measures from delicatessen

library(dplyr)
library(readr)
source("R/performance.R")
source("R/utils.R")
source("R/settings.R")

date <- "2024-10-11"
positivity <- c("Full", "Partial")
delt <- c("None","Homogeneous", "Heterogeneous")
probw <- c(25, 50, 75)

measures <-
  purrr::map_dfr(
    c("or", "rr"),
    function(meas){
        purrr::pmap_dfr(
        .l =  expand.grid(positivity = positivity, delt = delt, probw = probw,
                          date = date, stringsAsFactors = FALSE),
        .f =  function(positivity, delt, probw, date) {
          setting_file <- dplyr::last(list.files(path = "data/settings/",
                                                 pattern = date, full.names = T))
          patt <- paste0(tolower(c(positivity, delt, probw)), collapse = "-")
          results_files <- list.files(sprintf("data/simulations/%s/%s", date, meas), pattern = patt, full.names = T)
          results <- purrr::map_dfr(results_files, readRDS)
           # readRDS(results_files[!grepl("crude", results_files)])
          settings <- readRDS(setting_file)
          effects <- settings %>%
            dplyr::filter(label == paste0(positivity, " exchangeability") & effect == delt & 100*pw==probw) %>%
            effect_measures(measure = meas)

          hold <- cbind(results[grepl(paste0("ln", meas), results$params), ], effects)
          hold |>
            dplyr::mutate(
              ## Confirm delta
              delta = log(if_else(grepl("smr", params), att, ate)),
              ## Measures
              Param = params,
              Coef = roots,
              LCL = roots - 1.96*se,
              UCL = roots + 1.96*se,
              cov = LCL <= delta & delta <= UCL,
              mse = (roots - delta)^2,
              bias = roots - delta,
              positivity = positivity,
              efftype = delt,
              probw = probw)
          }
    )
    }
)

saveRDS(measures, sprintf("data/results/raw/%s-measures.rds", date))

