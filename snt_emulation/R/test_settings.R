
rfiles <- "snt_emulation/R/"
source(paste0(rfiles, "update_pop.R"))
## Same for all scenarios
weeks <- 5 # for follow-up, three weeks for contributing indices
index_weeks <- 3
popsize <- 5000
sims <- 100
out_by_sev_trt0 <- c(0.05, 0.1)
trt_by_sev <- c(0.2, 0.6) # If these are the same, no confounding
enc_by_sev = c(0.4, 0.4)
## Changes by scenario
enc_by_sev_nomod <- c(0.4, 0.4)
enc_by_sev_mod <- c(0.4, 0.2)
#enc_by_sev_mod <-1, 1)
homg_effect <- 1
hetg_effect <- c(0.7, 0.3)

rctfup <- make_trajectories(trt_by_sev = c(0.5, 0.5),
                         enc_by_sev = c(0.5, 0.5),
                         out_by_sev_trt0,
                         out_by_sev_trt0*homg_effect,
                         sev_prob = 0.25,
                         weeks = 3,
                         index_weeks = 1)

long_rct <- convert_to_long(rctfup, weeks = 3, index_weeks = 1)
longer_rct <- long_rct |>
  convert_long_to_longer(rctfup, trt_enc_sev = c(0.5, 0.5)*c(0.5,0.5),
                         weeks = 3, index_weeks = 1)

rct <- survfit(Surv(t, delta)~trt, data = long_rct |> dplyr::filter(sev == 0), weights = prob) |> summary()
rct <- survfit(Surv(t_in, t_out, delta) ~ trt, data = longer_rct[longer_rct$sev0==0,], weights = prob) |> # Weight by prob means "true" weighted population
  summary()
dplyr::tibble(
  rctrisk = 1-rct$surv[rct$time==3]
)
enc_by_sev <- c(0.5, 0.5)
trt_by_sev = c(0.2, 0.5)
sntfup <- make_truepop(trt_by_sev = trt_by_sev,
                         enc_by_sev = enc_by_sev,
                         out_by_sev_trt0,
                         out_by_sev_trt0*0.5,
                         sev_prob = 0.25,
                         weeks = 5,
                         index_weeks = 3)

long_snt <- convert_to_long(sntfup, weeks = 5)
longer_snt <- long_snt |>
  convert_long_to_longer(sntfup, trt_enc_sev = trt_by_sev*enc_by_sev, weeks = 5)

long_pop <- longer_snt

snt <- survfit(Surv(t_in, t_out, delta) ~ trt, data = long_pop[long_pop$sev0==0 & long_pop$enc==1,], weights = prob*ipcw) |> # Weight by prob means "true" weighted population
  summary()
dplyr::tibble(
  risk = 1-snt$surv[snt$time==3]
)

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

dplyr::tibble(
  risk = 1-s$surv[s$time==3]
)


s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = hold[hold$sev0==1,], weights = prob*ipcw) |>
  summary()

sev1 <- dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)
 sev1

s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = longer_rct, weights = prob*ipcw*iptw) |>
  summary()

all <- dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)
all


