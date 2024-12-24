library(dplyr)
library(survival)

rfiles <- "snt_emulation/R/"
dataloc <- "snt_emulation/data/"

## Same for all scenarios
weeks <- 6 # for follow-up, three weeks for contributing indices
popsize <- 5000
sims <- 100
out_by_sev_trt0 <- c(0.05, 0.7)
trt_by_sev <- c(0.2, 0.2) # If these are the same, no confounding

## Changes by scenario
enc_by_sev_nomod <- c(0.4, 0.4)
enc_by_sev_mod <- c(0.4, 0.2)
#enc_by_sev_mod <- c(1, 1)
homg_effect <- 1
hetg_effect <- c(0.7, 0.3)

out_by_sev_trt1 = out_by_sev_trt0*homg_effect # Testing with no treatment effect

source(paste0(rfiles, "setup.R"))

## Rerun to get true population with settings
pop <- truepop(enc_by_sev = enc_by_sev_nomod, out_by_sev_trt0, out_by_sev_trt1, trajectories, weeks) |>
  truepop_treat(treatments, severity, trt_by_sev)

## Dataset with one record per contributed index
long_pop1 <- convert_to_long(pop, trt_enc_sev = trt_by_sev*enc_by_sev)

long_pop1 |>
  dplyr::left_join(
    pop |> select(trajid, prob), # Get probability of each trajectory (prob=probability of trajectory in true population)
    by = "trajid"
  ) -> hold1

hold1 |>
  tidyr::pivot_wider(id_cols = c(trajid, prob), values_from = trt, names_from = visit, names_prefix = "v") |>
  dplyr::filter(v1 == 0, !is.na(v2)) |>
  dplyr::summarize(
    sum(prob*v2)/sum(prob)
  )
## Dataset with one record per time interval per contributed index
long_pop <- long_pop1 |>
  convert_long_to_longer(severity, trt_enc_sev = trt_by_sev*enc_by_sev)

## Test risks and stuff
long_pop |>
  dplyr::left_join(
    pop |> select(trajid, prob), # Get probability of each trajectory (prob=probability of trajectory in true population)
    by = "trajid"
  ) |>
  dplyr::group_by(index) |>
  dplyr::mutate(sev0 = sev[1]) |>
  dplyr::ungroup() -> hold


## Check IPCW

hold |>
  dplyr::filter(trt == 0) |>
  #dplyr::filter(trttime>0|is.na(trttime)) |>
  dplyr::mutate(
    cens = (trttime == (visit + t_in)),
    cens = if_else(is.na(cens), FALSE, cens)
  ) -> c

c$pp <- predict(glm(cens ~ I(as.factor(t_out))*sev, data = c , family = "binomial", weights = prob), type = "response")
c$uncens <- 1-c$pp
c |>
  dplyr::select(index, trt, sev0, sev, t_out, uncens) |>
  dplyr::rename(t_in = t_out) |>
  dplyr::distinct(trt, sev, t_in, uncens) -> ipcw


hold |>
  dplyr::left_join(ipcw, by = c("t_in", "sev", "trt")) |>
  dplyr::group_by(index) |>
  dplyr::mutate(
    uncens = replace(uncens, is.na(uncens), 1),
    cuncens = cumprod(uncens),
    new_ipcw = 1/cuncens
  ) |>
  dplyr::ungroup() -> hold

hold |>
  dplyr::group_by(trt, sev0) |>
  summarize(
    high_sev_prop = sum(prob*sev)/sum(prob),
    high_sev_ipcw = sum(prob*sev*ipcw)/sum(prob*ipcw),
    high_sev_ipcw_new = sum(prob*sev*new_ipcw)/sum(prob*new_ipcw)
  )
# Checking risk under no treatment with "baseline" low severity because that is the one with time-varying confounding
s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = hold[hold$sev0==0,], weights = prob*ipcw) |> # Weight by prob means "true" weighted population
  summary()

# Notes:
# Everything looks ok with no treatment confounding (prob of treatment is 0.2 indep of severity) -- Good
# Risks start to diverge a little bit (3rd decimal) when probability of treatment is too high -- numerical problem?

dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)

s <- survfit(Surv(t, delta) ~ trt, data = hold, weights = prob) |>
  summary()

dplyr::tibble(
  risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
  risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
  lnrr = log(risk1/risk0),
  rd = risk1-risk0
)

hold |>
  select(index, trt, t_out, sev, sev0, prob) |>
  tidyr::pivot_wider(id_cols = c(index, trt, prob), names_from = t_out, values_from = sev, names_prefix  = "visit") |>
  dplyr::group_by(
    trt, visit1, visit2, visit3
  ) |>
  dplyr::summarize(
    prob = sum(prob)
  ) |>
  dplyr::group_by(trt) |>
  dplyr::mutate(prob = prob/sum(prob)) |>
  dplyr::filter(visit1 == 0)


hold |>
  dplyr::filter(t_in == 0) |>
  dplyr::mutate(treated = (trttime + 1) == visit,
                treated = replace(treated, is.na(treated), FALSE),
                treated = replace(treated, (trttime + 1)< visit, NA)) |>
  dplyr::group_by(visit) |>
  dplyr::summarize(sum(treated*prob, na.rm = T)/sum(prob, na.rm = T))

m <- glm(delta ~ trt*I(as.factor(t_out))*sev, data = hold, family = "binomial")




