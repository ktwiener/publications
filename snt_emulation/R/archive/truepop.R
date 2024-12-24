
## Create the population with realized potential outcomes.
## Will assign treatment after this step.
## This population should apply to all trials within a scenario.

truepop <- function(enc_by_sev, out_by_sev_trt0, out_by_sev_trt1, trajectories, weeks){
  trajectories |>
    dplyr::mutate(
      purrr::pmap_dfc(
        list(
          1:trt_weeks,
          trajectories[paste0("sev", 1:trt_weeks)],
          trajectories[paste0("enc", 1:trt_weeks)]
        ),
        function(i, sev, enc){
          ## If encounter/potential outcome occurs, probability is corresponding entry in  the
          ## probability by severity vector
          ## Else the probability is 1 - prob of encounter/potential outcome occurring by severity.
          dplyr::tibble(
            enc = enc*enc_by_sev[sev+1] +  (1-enc)*(1-enc_by_sev[sev+1])
          ) %>%
            purrr::set_names(paste0("prob_", c("enc"),i))
        }
      ),
      ## Map over all time varying things and create probabilities of the combinations
      purrr::pmap_dfc(
        list(
          1:weeks,
          trajectories[paste0("sev", 1:weeks)],
          trajectories[paste0("y0",  1:weeks)],
          trajectories[paste0("y1",  1:weeks)]
        ),
        function(i, sev, py0, py1){
          ## If encounter/potential outcome occurs, probability is corresponding entry in  the
          ## probability by severity vector
          ## Else the probability is 1 - prob of encounter/potential outcome occurring by severity.
          dplyr::tibble(
            py0 = py0*out_by_sev_trt0[sev+1] +  (1-py0)*(1-out_by_sev_trt0[sev+1]),
            py1 = py1*out_by_sev_trt1[sev+1] +  (1-py1)*(1-out_by_sev_trt1[sev+1])
          ) %>%
            purrr::set_names(paste0("prob_", c("y0", "y1"),i))
        }
      ),
      prob_enc1 = 1
    ) -> h

  h$prob <-  h$prob_traj *
             Reduce(`*`, dplyr::select(h, paste0("prob_enc", 1:trt_weeks))) *
             Reduce(`*`, dplyr::select(h, paste0("prob_y0", 1:weeks))) *
             Reduce(`*`, dplyr::select(h, paste0("prob_y1", 1:weeks)))

  h
}

truepop_treat <- function(pop, treatments, severity, trt_by_sev){

  snt_trt <- treatments |>
    cross_join(severity |>
                 dplyr::select(sev1, sev2, sev3)) |> # All possible treatment/severity combos.
    cross_join(pop |>
                 dplyr::distinct(y01, y02, y03)) |>
    dplyr::mutate(
      # Once treated, stay treated
      trt2 = pmax(trt1, trt2),
      trt3 = pmax(trt1, trt2, trt3)) |>
    dplyr::distinct() |>
    dplyr::mutate(
      # Probability of each treatment at each time point
      ptrt1 = trt_by_sev[sev1 + 1]*trt1 + (1-trt_by_sev[sev1 + 1])*(1-trt1),
      ptrt2 = pmax(trt_by_sev[sev2 + 1]*trt2 + (1-trt_by_sev[sev2 + 1])*(1-trt2), trt1),
      ptrt3 = pmax(trt_by_sev[sev3 + 1]*trt3 + (1-trt_by_sev[sev3 + 1])*(1-trt3), trt2),
      # Overall probability of that treatment pattern
      ptrt = ptrt1*ptrt2*ptrt3)

  snt_trt |>
    # Each trajectory already defined can exist within each treatment pattern
    dplyr::full_join(pop, by = c("sev1", "sev2", "sev3", "y01", "y02", "y03"), relationship = "many-to-many") |>
    dplyr::mutate(
      prob = prob*ptrt, # Probability of trajectory with treatment pattern
      trt1 = trt1*enc1, # But actually treatment can only happen if encounter happens
      trt2 = trt2*enc2, # Ditto
      trt3 = trt3*enc3, # Ditto

      # Once treated, stay treated
      trt4 = trt3,
      trt5 = trt4,
      trt6 = trt5,
      # Time of first treatment (will be censor time for untreated indices)
      # Minus 1 to maintain ordering with outcome
      trttime = dplyr::case_when(trt1 == 1 ~ 1, trt2 == 1 ~ 2, trt3 == 1 ~ 3, TRUE ~ NA_integer_)-1
    ) |>
    dplyr::mutate(
      # Assign observed outcome at each time point based on treated (SNT)
      y1 = trt1*y11 + (1-trt1)*y01,
      y2 = trt2*y12 + (1-trt2)*y02,
      y3 = trt3*y13 + (1-trt3)*y03,
      y4 = trt4*y14 + (1-trt4)*y04,
      y5 = trt5*y15 + (1-trt5)*y05,
      y6 = trt5*y16 + (1-trt6)*y06,
      y2 = pmax(y1, y2),
      y3 = pmax(y2, y3),
      y4 = pmax(y3, y4),
      y5 = pmax(y4, y5),
      y6 = pmax(y5, y6),
      # Time to outcome (first time observed outcome)
      ytime  = dplyr::case_when(y1 == 1 ~ 1,
                                y2 == 1 ~ 2,
                                y3 == 1 ~ 3,
                                y4 == 1 ~ 4,
                                y5 == 1 ~ 5,
                                y6 == 1 ~ 6,
                                TRUE ~ NA_integer_),
      delta = as.numeric(!is.na(ytime)),

      # SPT potential outcomes
      py0 = pmax(y01, y02, y03),
      py1 = pmax(y11, y12, y13),

      enc4 = 1,
      enc5 = 1,
      enc6 = 1
    ) |>
    # Sum together probabilities that have the same trajectory after assigning treatment and observed outcome
    dplyr::group_by(sevid, enc1, enc2, enc3, enc4, enc5, enc6,
                    sev1, sev2, sev3, sev4, sev5, sev6,
                    trt1, trt2, trt3, trt4, trt5, trt6, trttime,
                    y1, y2, y3, y4, y5, y6,
                    py0, py1, ytime, delta) |>
    dplyr::summarize(prob = sum(prob)) |>
    dplyr::ungroup()  -> hold

     bind_rows(
      hold |> dplyr::mutate(trt = 0, y = py0, sev = sev1),
      hold |> dplyr::mutate(trt = 1, y = py1, sev = sev1)
    ) |>
      dplyr::mutate(prob = 0.5*prob) |>
      dplyr::mutate(trajid = row_number())
}

convert_to_long <- function(pop, trt_enc_sev){
 pop %>%
    dplyr::select( c(trajid, sevid, enc1:enc6, sev1:sev6, trt1:trt6, trttime, y1:y6, anyy = delta, ytime)) |>
    pivot_longer(
      cols =  c(enc1:enc6, sev1:sev6, trt1:trt6, y1:y6),  # Include columns from enc1 to y3
      names_to = c(".value", "visit"),  # Split names to create 'visit' column
      names_pattern = "([a-z]+)([0-9]+)"  # Match variable type (enc, sev, trt, y) and visit number
    ) %>%
    select(trajid, sevid, visit, enc, sev, trt, y, ytime, trttime, anyy) %>%
    mutate(
      visit = as.integer(visit)  # Ensure 'visit' is numeric
    ) |>
    dplyr::filter(visit <= trttime+1 | is.na(trttime), # Filter to visits before or up to first treatment
                  ytime >= visit  | is.na(ytime)) |>  # Filter to visits before or up to outcome
    dplyr::group_by(trajid) |>
    dplyr::mutate(trttime = ifelse(all(trt==0), NA_real_, trttime)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Event indicator
      delta = dplyr::case_when(
        trt == 1 ~ anyy, # If treated, event if any
        trt == 0 & is.na(trttime) ~ anyy, # If not treated, all indices have event (or not)
        TRUE ~ y # Otherwise, censored at treatment
      ),
      censtime = (1-trt)*pmin(trttime, visit + 3 - 1, na.rm = T) + trt*(visit + 3 - 1),
      eventtime = pmin(censtime, ytime, na.rm = T),
      t = eventtime - visit + 1,
      iptw = trt/trt_enc_sev[sev+1] + (1-trt)/(1+trt_enc_sev[sev+1]),
      delta = if_else(censtime < ytime | is.na(ytime), 0, delta),
      admin = as.numeric(t<3 & delta == 0)
    ) |>
    dplyr::filter(visit <= 3) -> h
    #dplyr::select(trajid, sevid, visit, enc, sev0=sev, trt, t, delta, iptw, admin) -> h

  h
}

convert_long_to_longer <- function(h, severity, trt_enc_sev){
  h$final_t <- h$t

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
        ~unlist(severity[.x, 1:weeks])[.y]
      )
    ) |>
    dplyr::mutate(
      puncens = 1-trt_enc_sev[sev+1]
    ) |>
    dplyr::group_by(index) |>
    dplyr::mutate(
      cum_puncens = cumprod(puncens),
      ipcw = trt + (1-trt)/cum_puncens,
      ipcw = lag(ipcw, default = 1)
    )
}


admin_censor <- function(long_pop, pop){
  long_pop |>
    dplyr::left_join(
      pop |> select(trajid, prob),
      by = "trajid"
    ) |>
    dplyr::mutate(admin = if_else(admin == 1 & final_t == t_out, 1, 0)) |>
    dplyr::filter(delta == 0) |>
    dplyr::group_by(trt, sev, t_out) |>
    dplyr::summarize(
      pcens = sum(prob*admin)/sum(prob)
    ) |>
    dplyr::mutate(
      puncens = 1-pcens,
      cum_puncens = cumprod(puncens),
      ipaw = 1/cum_puncens
    ) |>
    dplyr::select(trt, sev, t_in = t_out, ipaw)
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

