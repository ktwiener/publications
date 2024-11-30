
## Create the population with realized potential outcomes.
## Will assign treatment after this step.
## This population should apply to all trials within a scenario.

truepop <- function(enc_by_sev, out_by_sev_trt0, out_by_sev_trt1, trajectories, weeks){
  trajectories |>
    dplyr::mutate(
      ## Map over all time varying things and create probabilities of the combinations
      purrr::pmap_dfc(
        list(
          1:weeks,
          trajectories[paste0("sev", 1:weeks)],
          trajectories[paste0("enc", 1:weeks)],
          trajectories[paste0("y0",  1:weeks)],
          trajectories[paste0("y1",  1:weeks)]
        ),
        function(i, sev, enc, py0, py1){
          ## If encounter/potential outcome occurs, probability is corresponding entry in  the
          ## probability by severity vector
          ## Else the probability is 1 - prob of encounter/potential outcome occurring by severity.
          dplyr::tibble(
            enc = enc*enc_by_sev[sev+1] +  (1-enc)*(1-enc_by_sev[sev+1]),
            py0 = py0*out_by_sev_trt0[sev+1] +  (1-py0)*(1-out_by_sev_trt0[sev+1]),
            py1 = py1*out_by_sev_trt1[sev+1] +  (1-py1)*(1-out_by_sev_trt1[sev+1])
          ) %>%
            purrr::set_names(paste0("prob_", c("enc", "y0", "y1"),i))
        }
      ),
      prob_enc1 = 1
    ) -> h

  h$prob <-  h$prob_traj *
             Reduce(`*`, dplyr::select(h, paste0("prob_enc", 1:weeks))) *
             Reduce(`*`, dplyr::select(h, paste0("prob_y0", 1:weeks))) *
             Reduce(`*`, dplyr::select(h, paste0("prob_y1", 1:weeks)))

  h
}

truepop_treat <- function(pop, treatments, severity, trt_by_sev){

  snt_trt <- treatments |>
    cross_join(severity) |> # All possible treatment/severity combos.
    dplyr::mutate(
      # Probability of each treatment at each time point
      ptrt1 = trt_by_sev[sev1 + 1]*trt1 + (1-trt_by_sev[sev1 + 1])*(1-trt1),
      ptrt2 = trt_by_sev[sev2 + 1]*trt2 + (1-trt_by_sev[sev2 + 1])*(1-trt2),
      ptrt3 = trt_by_sev[sev3 + 1]*trt3 + (1-trt_by_sev[sev3 + 1])*(1-trt3),
      # Overall probability of that treatment pattern
      ptrt = ptrt1*ptrt2*ptrt3)

  snt_trt |>
    # Each trajectory already defined can exist within each treatment pattern
    dplyr::full_join(pop, by = c("sevid", "sev1", "sev2", "sev3"), relationship = "many-to-many") |>
    dplyr::mutate(
      prob = prob*ptrt, # Probability of trajectory with treatment pattern
      trt1 = trt1*enc1, # But actually treatment can only happen if encounter happens
      trt2 = trt2*enc2, # Ditto
      trt3 = trt3*enc3, # Ditto

      # Once treated, stay treated
      trt2 = pmax(trt1, trt2),
      trt3 = pmax(trt1, trt2, trt3),
      # Time of first treatment (will be censor time for untreated indices)
      trttime = dplyr::case_when(trt1 == 1 ~ 1, trt2 == 1 ~ 2, trt3 == 1 ~ 3, TRUE ~ NA_integer_),
    ) |>
    dplyr::mutate(
      # Assign observed outcome at each time point based on treated (SNT)
      y1 = trt1*y11 + (1-trt1)*y01,
      y2 = trt2*y12 + (1-trt2)*y02,
      y3 = trt3*y13 + (1-trt3)*y03,
      y2 = pmax(y1, y2),
      y3 = pmax(y2, y3),
      # Time to outcome (first time observed outcome)
      ytime  = dplyr::case_when(y1 == 1 ~ 1, y2 == 1 ~ 2, y3 == 1 ~ 3, TRUE ~ NA_integer_),
      delta = as.numeric(!is.na(ytime)),

      # SPT potential outcomes
      py0 = pmax(y01, y02, y03),
      py1 = pmax(y11, y12, y13)
    ) |>
    # Sum together probabilities that have the same trajectory after assigning treatment and observed outcome
    dplyr::group_by(sevid, enc1, enc2, enc3, sev1, sev2, sev3, trt1, trt2, trt3, trttime, y1, y2, y3, py0, py1, ytime, delta) |>
    dplyr::summarize(prob = sum(prob)) |>
    dplyr::ungroup()  -> hold

     bind_rows(
      hold |> dplyr::mutate(trt = 0, y = py0, sev = sev1),
      hold |> dplyr::mutate(trt = 1, y = py1, sev = sev1)
    ) |>
      dplyr::mutate(prob = 0.5*prob) |>
      dplyr::mutate(trajid = row_number())
}

convert_snt_indices <- function(sample_pop){
  sample_pop %>%
    dplyr::select( c(sim, id, sevid, enc1:enc3, sev1:sev3, trt1:trt3, trttime, y1:y3, anyy = delta, ytime)) |>
    pivot_longer(
      cols =  c(enc1:enc3, sev1:sev3, trt1:trt3, y1:y3),  # Include columns from enc1 to y3
      names_to = c(".value", "visit"),  # Split names to create 'visit' column
      names_pattern = "([a-z]+)([0-9]+)"  # Match variable type (enc, sev, trt, y) and visit number
    ) %>%
    select(sim, id, sevid, visit, enc, sev, trt, y, ytime, trttime, anyy) %>%
    mutate(
      visit = as.integer(visit)  # Ensure 'visit' is numeric
    ) |>
    dplyr::filter(visit <= trttime | is.na(trttime), # Filter to visits before or up to first treatment
                  ytime >= visit  | is.na(ytime)) |>  # Filter to visits before or up to outcome
    dplyr::mutate(
      # Event indicator
      delta = dplyr::case_when(
        trt == 1 ~ anyy, # If treated, event if any
        trt == 0 & is.na(trttime) ~ anyy, # If not treated, all indices have event (or not)
        TRUE ~ y # Otherwise, censored at treatment
      ),
      censtime = (1-trt)*pmin(trttime, 3, na.rm = T) + trt*3,
      eventtime = pmin(censtime, ytime, na.rm = T),
      t = eventtime - visit + 1
    ) |>
    dplyr::select(sim, id, sevid, visit, enc, sev, trt, t, delta)
}

convert_to_long <- function(pop){
 pop %>%
    dplyr::select( c(trajid, sevid, enc1:enc3, sev1:sev3, trt1:trt3, trttime, y1:y3, anyy = delta, ytime)) |>
    pivot_longer(
      cols =  c(enc1:enc3, sev1:sev3, trt1:trt3, y1:y3),  # Include columns from enc1 to y3
      names_to = c(".value", "visit"),  # Split names to create 'visit' column
      names_pattern = "([a-z]+)([0-9]+)"  # Match variable type (enc, sev, trt, y) and visit number
    ) %>%
    select(trajid, sevid, visit, enc, sev, trt, y, ytime, trttime, anyy) %>%
    mutate(
      visit = as.integer(visit)  # Ensure 'visit' is numeric
    ) |>
    dplyr::filter(visit <= trttime | is.na(trttime), # Filter to visits before or up to first treatment
                  ytime >= visit  | is.na(ytime)) |>  # Filter to visits before or up to outcome
    dplyr::mutate(
      # Event indicator
      delta = dplyr::case_when(
        trt == 1 ~ anyy, # If treated, event if any
        trt == 0 & is.na(trttime) ~ anyy, # If not treated, all indices have event (or not)
        TRUE ~ y # Otherwise, censored at treatment
      ),
      censtime = (1-trt)*pmin(trttime, 3, na.rm = T) + trt*3,
      eventtime = pmin(censtime, ytime, na.rm = T),
      t = eventtime - visit + 1
    ) |>
    dplyr::select(trajid, sevid, visit, enc, sev0=sev, trt, t, delta) -> h

  h
}

convert_long_to_longer <- function(h, severity, trt_enc_sev){

  survSplit(Surv(t, delta) ~ .,
            data = h,
            cut = 1:3,
            start = "t_in",
            end = "t_out",
            id = "index",
            event = "delta") |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      sev = purrr::map2_dbl(
        sevid,
        visit+t_in,
        ~unlist(severity[.x, 1:3])[.y]
      )
    ) |>
    dplyr::mutate(
      puncens = 1-trt_enc_sev[sev+1]
    ) |>
    dplyr::group_by(index) |>
    dplyr::mutate(
      cum_puncens = lag(cumprod(puncens), default = 1),
      ipcw = 1/cum_puncens
    )
}

## Possible SPT trajectories
#
# spt_treat <- function(pop){
#   bind_rows(
#     pop |> dplyr::mutate(a = 1, prob = 0.5*prob), # Each trajectory has equal prob of treatment
#     pop |> dplyr::mutate(a = 0, prob = 0.5*prob)
#   ) |>
#     dplyr::mutate(
#       y0 = pmax(y01, y02, y03), # Potential outcome under no treatment is max
#       y1 = pmax(y11, y12, y13), # Potential outcome under treatment
#       y  = a*y1 + (1-a)*y0    # Observed outcome
#     ) |>
#     dplyr::mutate(id = row_number()) |>
#     dplyr::as_tibble()
# }

