## Script to run analyses

library(dplyr)
library(ggplot2)
library(Hmisc)

setwd(here::here("positivity", "motivating-example"))
data.loc <- here::here("data")

source("R/ps_vars.R")
source("R/mestimate.R")
source("R/predict_ps.R")
source("R/ps_table.R")
source("R/ps_density.R")
source("R/table1.R")
source("R/table2.R")

### Read in data ----

## Full cohort
adstroke <- readRDS(paste0(data.loc,"/motivating-example/adstroke.rds"))
full_files <- list.files(paste0(data.loc, "/motivating-example/results/"), pattern = "full.csv", full.names = T)

mestsfull <- read.csv(dplyr::last(full_files))

adstrokeps <- predict_ps(adstroke, mestsfull, ps_vars) # Add predicted propensity scores.

## 6000 sample
adstroke6000 <- readRDS(paste0(data.loc,"/motivating-example/adstroke_6000.rds"))
six_files <- list.files(paste0(data.loc, "/motivating-example/results/"), pattern = "6000.csv", full.names = T)

mests6000 <- read.csv(dplyr::last(six_files))

adstroke6000ps <- predict_ps(adstroke6000, mests6000, ps_vars) # Add predicted propensity scores

### Table 1: Unweighted and weighted ----

## Full table
crude <- make_table1(adstrokeps, wght = "dum")

crude$tbl %>%
  gt::gt() %>%
  gt::gtsave(sprintf("%s/motivating-example/results/output/table1_unwght_full.rtf", data.loc))

ipt <- make_table1(adstrokeps, "ipt")
smr <- make_table1(adstrokeps, "smr")

wght <- full_join(ipt$tbl %>%
                    rename(
                      ipttpa = tpap,
                      iptnotpa = notpap
                    ) %>%
                    select(-totalp),
                  smr$tbl %>%
                    rename(
                      smrtpa = tpap,
                      smrnotpa = notpap
                    ) %>%
                    select(-totalp),
                  by = c("cat", "lvl"))

wght %>%
  gt::gt() %>%
  gt::gtsave(sprintf("%s/motivating-example/results/output/table1_wght_full.rtf", data.loc))


## 6000 person table
crude <- make_table1(adstroke6000ps, wght = "dum")

crude$tbl %>%
  gt::gt() %>%
  gt::gtsave(sprintf("%s/motivating-example/results/output/table1_unwght_6000.rtf", data.loc))

ipt <- make_table1(adstroke6000ps, "ipt")
smr <- make_table1(adstroke6000ps, "smr")

wght <- full_join(ipt$tbl %>%
                    rename(
                      ipttpa = tpap,
                      iptnotpa = notpap
                    ) %>%
                    select(-totalp),
                  smr$tbl %>%
                    rename(
                      smrtpa = tpap,
                      smrnotpa = notpap
                    ) %>%
                    select(-totalp),
                  by = c("cat", "lvl"))

wght %>%
  gt::gt() %>%
  gt::gtsave(sprintf("%s/motivating-example/results/output/table1_wght_6000.rtf", data.loc))

### Table 2: Propensity score table ### ----
## Full Results
pstable <- ps_table_by_pct(adstrokeps, "full", data.loc)

## 6000 sample results
pstable6000 <- ps_table_by_pct(adstroke6000ps, "6000", data.loc)

### Propensity score distribution ----
ps_density(adstrokeps, data.loc, "full")
ps_density(adstroke6000ps, data.loc, "6000")

### Final results ----
full <- make_table2(adstroke, mestsfull)

full |>
  gt::gt() %>%
  gt::gtsave(sprintf("%s/motivating-example/results/output/table2_full.rtf", data.loc))

six <- make_table2(adstroke6000, mests6000)

six |>
  gt::gt() %>%
  gt::gtsave(sprintf("%s/motivating-example/results/output/table2_6000.rtf", data.loc))

