
rfiles <- "snt_emulation/R/"
source(paste0(rfiles, "truepop.R"))
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

rctpop <- make_truepop(trt_by_sev = c(0.5, 0.5), # Treatment probability by severity
                       enc_by_sev = enc_by_sev, # Encounter probability by severity
                       out_by_sev_trt0 = out_by_sev_trt0, # Outcome probability by severity for under no treatment
                       out_by_sev_trt1 = out_by_sev_trt0*1, # Outcome probability by severity under treatment
                       sev_prob = 0.25, # Probability of severity progression at each visit
                       index_weeks = 1, # Number of visits that contribute indices
                       weeks = 3) |>
  convert_to_long(weeks = 3, index_weeks = 1) |>
  dplyr::filter(visit == 1)

rct <- survfit(Surv(t, delta)~trt, data = rctpop |> dplyr::filter(sev == 0), weights = prob) |> summary()

dplyr::tibble(
  rctrisk = 1-rct$surv[rct$time==3]
)


enc_by_sev <- c(0.25, 0.5)
trt_by_sev = c(0.2, 0.5)
sntpop <- make_truepop(trt_by_sev = trt_by_sev, # Treatment probability by severity
                       enc_by_sev = enc_by_sev, # Encounter probability by severity
                       out_by_sev_trt0 = out_by_sev_trt0, # Outcome probability by severity for under no treatment
                       out_by_sev_trt1 = out_by_sev_trt0*1, # Outcome probability by severity under treatment
                       sev_prob = 0.25, # Probability of severity progression at each visit
                       index_weeks = 3, # Number of visits that contribute indices
                       weeks = 6) # Full number of visits


## convert the possible true pops to long format
long_snt <- convert_to_long(fup = sntpop, weeks = 6, index_weeks = 3) |>
  convert_long_to_longer(sntpop,
                         trt_enc_sev = enc_by_sev*trt_by_sev,
                         weeks = 6)

snt <- survfit(Surv(t_in, t_out, delta) ~ trt, data = long_snt[long_snt$sev0==0,], weights = prob*ipcw*iptw) |> # Weight by prob means "true" weighted population
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


