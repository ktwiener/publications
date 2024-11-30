

estimate_effects <- function(scen){
  ## Load the samples
  samples <- list.files(paste0("data/simulation/samples/scenario", scen), full.names = T)[1:(sims/100)]

  effs <- purrr::map(
    samples,
    function(filenm){
      h <- readRDS(filenm)

      ## Estimate the effects
      list(
        spt_effect = sim_spt_effect(h$spt),
        snt_effect = snt_effect_overall_and_sev(h$snt),
        td_effect  = snt_effect_overall_and_sev(h$snt |> dplyr::filter(enc==1))
      )

    }
  )

  purrr::transpose(effs) |>
    purrr::map(
      ~purrr::map_dfr(.x, function(x) x)
    ) |>
    saveRDS(paste0("data/simulation/effects/scenario", scen, ".rds"))

  dists <- purrr::map(
    samples,
    function(filenm){
      h <- readRDS(filenm)

      list(
        spt_dist = sev_distribution(h$spt),
        snt_dist = sev_distribution(h$snt),
        td_dist = sev_distribution(h$snt |> dplyr::filter(enc==1))
      )
    }
  )

  purrr::transpose(dists) |>
    purrr::map(
      ~purrr::map_dfr(.x, function(x) x)
    ) |>
    saveRDS(paste0("data/simulation/effects/distrib", scen, ".rds"))

}

true_spt_effect <- function(pop){
  pop |>
    dplyr::transmute(
      y0 = py0,
      y1 = py1,
      prob
    ) |>
    dplyr::summarize(
      risk1 = sum(prob*y1),
      risk0 = sum(prob*y0),
      lnrr = log(risk1/risk0),
      rd = risk1 - risk0
    ) |>
    tidyr::pivot_longer(
      cols = c(risk1, risk0, lnrr, rd),
      names_to = "param",
      values_to = "truth"
    )
}

## Estimate effects in each simulation of single point trial
sim_spt_effect <- function(spt){
  spt |>
    dplyr::group_by(sim) |>
    dplyr::summarize(
      risk1 = mean(y[trt==1]),
      risk0 = mean(y[trt==0]),
      lnrr = log(risk1/risk0),
      rd = risk1-risk0
    )
}

snt_effect <- function(d){
  # Survival by treatment, weighted by IPCW
  s <- survfit(Surv(t_in, t_out, delta) ~ trt, data = d, weights = ipcw) |>
    summary()

  dplyr::tibble(
    risk1 = 1-s$surv[s$time==3 & s$strata=="trt=1"],
    risk0 = 1-s$surv[s$time==3 & s$strata=="trt=0"],
    lnrr = log(risk1/risk0),
    rd = risk1-risk0
  )
}

snt_effect_overall_and_sev <- function(snt){
  snt |>
    dplyr::group_by(sim) |>
    tidyr::nest() |>
    dplyr::mutate(
      res = purrr::map(data,
                       function(d){
                         overall <- snt_effect(d) # Effect overall
                         low <- snt_effect(d[d$sev==0, ]) ## Effect low severity indices
                         high <- snt_effect(d[d$sev==1, ]) ## Effect high severity indices

                         bind_rows(
                           overall |> dplyr::mutate(sev = "Overall"),
                           low |> dplyr::mutate(sev = "Low"),
                           high |> dplyr::mutate(sev = "High")
                         )
                       })
    ) |>
    dplyr::select(-data) |>
    tidyr::unnest(res)

}

## Distribution of high and low severity in the first index.
sev_distribution <- function(d){
  d |>
    dplyr::group_by(sim) |>
    dplyr::summarize(
      ate_low = mean(1-sev),
      ate_high = mean(sev),
      att_low = mean(1-sev[trt==1]),
      att_high = mean(sev[trt==1])
    )
}

