## Script to make cohorts

library(dplyr)

setwd(here::here("positivity", "motivating-example"))
data.loc <- here::here("data")

source("R/analytic_setup.R")
source("R/make_incl_pos.R")
source("R/ps_vars.R")
source("R/mestimate.R")

stroke <- readRDS(paste0(data.loc,"/stroke_clean.rds"))
codebook <- readRDS(paste0(data.loc,"/codebook.rds"))

ad <- make_ad(stroke)

## Full cohort
adstroke <- ad$data %>%
  mutate(tpa_aje= tpa==1) %>%
  filter(if_all(all_of(ps_vars_all), ~!is.na(.x)))

adstroke |>
  convert_variables(ps_vars = ps_vars) |>
  dplyr::select(tpa, death, starts_with("ps_vars")) |>
  write.csv(paste0(data.loc, "/motivating-example/adstroke_dummy.csv"))

saveRDS(adstroke, paste0(data.loc, "/motivating-example/adstroke.rds"))

## 6000 sample.
set.seed(19910812)

adstroke6000 <- adstroke[sample(1:nrow(adstroke), 6000), ]
adstroke6000 |>
  convert_variables(ps_vars = ps_vars) |>
  dplyr::select(tpa, death, starts_with("ps_vars")) |>
  write.csv(paste0(data.loc, "/motivating-example/adstroke_6000_dummy.csv"))

saveRDS(adstroke6000, paste0(data.loc, "/motivating-example/adstroke_6000.rds"))
write.csv(adstroke6000, paste0(data.loc, "/motivating-example/adstroke_6000.csv"))
