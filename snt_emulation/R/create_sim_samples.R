

create_sim_samples <- function(scen,
                               effect,
                               enc_by_sev,
                               out_by_sev_trt0 = c(0.09, 0.16),
                               sev_prob = 0.25,
                               sims = 2000,
                               popsize = 5000,
                               index_weeks = 3,
                               followup = 3){

  weeks <- index_weeks + followup - 1

  # Treatment effect
  out_by_sev_trt1 <- out_by_sev_trt0*effect

  ## Treatment probability by severity
  trt_low <- uniroot(scen_trt_prob,
          interval = c(0,1),
          enc_by_sev = enc_by_sev,
          out_by_sev_trt0 = out_by_sev_trt0,
          out_by_sev_trt1 = out_by_sev_trt1,
          sev_prob = sev_prob,
          index_weeks = index_weeks,
          weeks = weeks)$root

  trt_by_sev <- c(trt_low, 2*trt_low)

  ## True effect by setting treatment
  true_spt <- purrr::map_dfc(
    0:1,
    function(lvl){
      make_truepop(trt_by_sev = c(lvl, lvl), # Setting to 0 for no treatment, 1 for treatment.
                   enc_by_sev = enc_by_sev, # Encounter probability by severity
                   out_by_sev_trt0 = out_by_sev_trt0, # Outcome probability by severity for under no treatment
                   out_by_sev_trt1 = out_by_sev_trt1, # Outcome probability by severity under treatment
                   sev_prob = sev_prob, # Probability of severity progression at each visit
                   index_weeks = 1, # Number of visits that contribute indices
                   weeks = followup) |>
        dplyr::summarize(!!paste0("risk", lvl) := sum(as.numeric(!is.na(ytime))*prob)/sum(prob))
    }
  ) |>
    dplyr::mutate(lnrr = log(risk1/risk0),
                  rd = risk1-risk0)


  saveRDS(true_spt, paste0(dataloc, "/samples/scenario_truth_", scen, ".rds"))

  ## Setting up RCT -- treatment is 50/50, one week contributes indices
  rctpop <- make_truepop(trt_by_sev = c(0.5, 0.5), # Treatment probability by severity
                         enc_by_sev = enc_by_sev, # Encounter probability by severity
                         out_by_sev_trt0 = out_by_sev_trt0, # Outcome probability by severity for under no treatment
                         out_by_sev_trt1 = out_by_sev_trt1, # Outcome probability by severity under treatment
                         sev_prob = sev_prob, # Probability of severity progression at each visit
                         index_weeks = 1, # Number of visits that contribute indices
                         weeks = 3) |>
    convert_to_long(weeks = weeks, index_weeks = 1) |>
    dplyr::filter(visit == 1)

  ## Setting up SNT.
  ## all the trajectories with probabilities
  sntpop <- make_truepop(trt_by_sev = trt_by_sev, # Treatment probability by severity
                      enc_by_sev = enc_by_sev, # Encounter probability by severity
                      out_by_sev_trt0 = out_by_sev_trt0, # Outcome probability by severity for under no treatment
                      out_by_sev_trt1 = out_by_sev_trt1, # Outcome probability by severity under treatment
                      sev_prob = sev_prob, # Probability of severity progression at each visit
                      index_weeks = index_weeks, # Number of visits that contribute indices
                      weeks = weeks) # Full number of visits



  ## convert the possible true pops to long format
  long_snt <- convert_to_long(fup = sntpop, weeks = weeks, index_weeks = index_weeks) |>
    convert_long_to_longer(sntpop,
                           trt_enc_sev = enc_by_sev*trt_by_sev,
                           weeks = weeks)

  ## sample rct IDs
  rctids <- sample(rctpop$trajid, popsize*sims, replace = T, prob = rctpop$prob)
  ## get first visit severity distribution
  sevs <- rctpop$sev[rctids]
  ## number of high severity indices per simulation
  distsev <- purrr::map(split(sevs, gl(sims, popsize)),sum)
  ## get snt ids
  sntids <- purrr::map(
    unname(distsev),
    ~{
      sev1s <- with(sntpop[sntpop$sev1==1, ], sample(trajid, .x, replace = T, prob = prob))
      sev0s <- with(sntpop[sntpop$sev1==0, ], sample(trajid, popsize - .x, replace = T, prob = prob))
      c(sev1s, sev0s)
    }) |> unlist()

  ## Number of simulations per partition
  simsize <- min(100,sims)

  rctsplit <- split(rctids, gl(sims/simsize, simsize)) # RCT ids
  sntsplit <- split(sntids, gl(sims/simsize, simsize)) # SNT ids

  purrr::pwalk(
    list(rctsplit, sntsplit, 1:length(rctsplit)),
    function(rct, snt, i){
      simn <- 1:simsize %% popsize
      sample_rct <- rctpop[rct, ] |>
        dplyr::mutate(
          sim = rep((1:simsize)+((i-1)*simsize), each = popsize),
          id = rep(1:popsize, simsize)
        )

      sample_snt <- sntpop[snt, ] |>
        dplyr::mutate(
          sim = rep((1:simsize)+((i-1)*simsize), each = popsize),
          id = rep(1:popsize, simsize)
        )

      ## same samples, but in long format
      sample_snt_long <- sample_snt |>
        dplyr::select(trajid, sim, id) |>
        dplyr::left_join(long_snt, by = "trajid", relationship = "many-to-many")

      list(
        spt = sample_rct,
        snt = sample_snt_long
      ) |>
        saveRDS(paste0(dataloc, "samples/scenario", scen, "/sim_samples_", sprintf("%02d", i), ".rds"))
    }
  )



}


scen_trt_prob <- function(plow,
                          enc_by_sev,
                          out_by_sev_trt0,
                          out_by_sev_trt1,
                          sev_prob,
                          index_weeks,
                          weeks){
  make_truepop(trt_by_sev = c(plow, 2*plow), # Treatment probability by severity
               enc_by_sev = enc_by_sev_mod, # Encounter probability by severity
               out_by_sev_trt0 = out_by_sev_trt0, # Outcome probability by severity for under no treatment
               out_by_sev_trt1 = out_by_sev_trt1, # Outcome probability by severity under treatment
               sev_prob = sev_prob, # Probability of severity progression at each visit
               index_weeks = index_weeks, # Number of visits that contribute indices
               weeks = weeks) |>
    dplyr::mutate(trt = pmax(trt1, trt2, trt3, na.rm = T)) |>
    dplyr::summarize(
      p = sum(trt*prob)/sum(prob)
    ) |> pull(p) -> p

  p - 0.5

}
