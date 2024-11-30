

create_sim_samples <- function(out_by_sev_trt1, enc_by_sev, scen){

  ## all the trajectories with probabilities
  pop <- truepop(enc_by_sev, out_by_sev_trt0, out_by_sev_trt1, trajectories, weeks) |>
    truepop_treat(treatments, severity, trt_by_sev)

  saveRDS(true_spt_effect(pop), paste0("data/simulation/samples/scenario_truth_", scen, ".rds"))
  ## convert the possible true pops to long format
  long_pop <- convert_to_long(pop) |>
    convert_long_to_longer(severity, trt_enc_sev = trt_by_sev*enc_by_sev)

  ## sample ids for both trials (so same underlying population for each sim)
  ids <- sample(pop$trajid, popsize*sims, replace = T, prob = pop$prob)
  simsize <- 100
  simsplit <- split(ids, gl(sims/simsize, simsize))

  ## create samples -- draw from same population
  purrr::imap(
    simsplit,
    function(ids, lbl){
      lbl <- as.numeric(lbl)
      sample_pop <- pop[ids, ] |>
        dplyr::mutate(
          sim = rep((simsize*(lbl-1)+1):(lbl*simsize), each = popsize),
          id = rep(1:popsize, simsize)
        )

      ## same samples, but in long format
      sample_long_pop <- sample_pop |>
        dplyr::select(trajid, sim, id) |>
        dplyr::left_join(long_pop, by = "trajid", relationship = "many-to-many")

      list(
        spt = sample_pop,
        snt = sample_long_pop
      ) |>
        saveRDS(paste0("data/simulation/samples/scenario", scen, "/sim_samples_", sprintf("%02d", lbl), ".rds"))
    }
  )



}
