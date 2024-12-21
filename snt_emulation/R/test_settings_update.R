
rfiles <- "snt_emulation/R/"
source(paste0(rfiles, "update_pop.R"))
## Same for all scenarios
weeks <- 5 # for follow-up, three weeks for contributing indices
index_weeks <- 3
popsize <- 5000
sims <- 100
out_by_sev_trt0 <- c(0.1, 0.1)
trt_by_sev <- c(0.2, 0.2) # If these are the same, no confounding

## Changes by scenario
enc_by_sev_nomod <- c(0.4, 0.4)
enc_by_sev_mod <- c(0.4, 0.2)
#enc_by_sev_mod <- c(1, 1)
homg_effect <- 1
hetg_effect <- c(0.7, 0.3)

fup <- make_trajectories(trt_by_sev,
                         enc_by_sev = c(1, 1),
                         out_by_sev_trt0,
                         out_by_sev_trt0, weeks = weeks, index_weeks = index_weeks)

long_pop1 <- convert_to_long(fup, weeks = weeks)
long_pop <- long_pop1 |> convert_long_to_longer(fup, trt_enc_sev = trt_by_sev, weeks = weeks)

## Test risks and stuff
long_pop |>
  dplyr::left_join(
    fup |> dplyr::select(trajid, prob), # Get probability of each trajectory (prob=probability of trajectory in true population)
    by = "trajid"
  ) -> hold

# Checking risk under no treatment with "baseline" low severity because that is the one with time-varying confounding
s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = hold[hold$sev0==0,], weights = prob*ipcw) |> # Weight by prob means "true" weighted population
  summary()

# Notes:
# Everything looks ok with no treatment confounding (prob of treatment is 0.2 indep of severity) -- Good
# Risks start to diverge a little bit (3rd decimal) when probability of treatment is too high -- numerical problem?

sev0 <- dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)
sev0
s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = hold[hold$sev0==1,], weights = prob*ipcw) |>
  summary()

sev1 <- dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)
 sev1

s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = hold, weights = prob*ipcw*iptw) |>
  summary()

all <- dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)
all


