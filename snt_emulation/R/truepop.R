
## Make generic frame of information needed at each visit.
make_truepop <- function(trt_by_sev, # Treatment probability by severity
                              enc_by_sev, # Encounter probability by severity
                              out_by_sev_trt0, # Outcome probability by severity for under no treatment
                              out_by_sev_trt1, # Outcome probability by severity under treatment
                              sev_prob = 0.25, # Probability of severity progression at each visit
                              index_weeks = 3, # Number of visits that contribute indices
                              weeks = 5){ # Full number of visits

  fup_weeks <- weeks - index_weeks # Number of visits after indices are contributed

  ## All possible combinations of severity, treatment, and outcome
  dplyr::tibble(sev = 0:1) |>
    dplyr::cross_join(
      dplyr::tibble(enc = 0:1)
    ) |>
    dplyr::cross_join(
      dplyr::tibble(trt = 0:1)
    ) |>
    dplyr::cross_join(
      dplyr::tibble(y = 0:1)
    ) |>
    dplyr::mutate(
      # Prob of observed severity
      psev = sev*sev_prob + (1-sev)*(1-sev_prob),
      ## Probability of observed treatment
      ptrt = trt*trt_by_sev[sev + 1] + (1-trt)*(1-trt_by_sev[sev + 1]),
      ## Probability of observed encounter
      penc = enc*enc_by_sev[sev + 1] + (1-enc)*(1-enc_by_sev[sev+1]),
      trt = trt*enc,
      ## Probability of observed outcome
      py = y*(enc*trt*out_by_sev_trt1[sev + 1] + (1-trt)*out_by_sev_trt0[sev + 1]) + # Prob of outcome based on trt
        (1-y)*(enc*trt*(1-out_by_sev_trt1[sev + 1]) + (1-trt)*(1-out_by_sev_trt0[sev + 1])) # Prob of no outcome based on trt & sev
    ) -> frame

  ## Make initial week of indices
  week1 <- frame |>
    dplyr::filter(enc == 1) |>
    set_names(paste0(names(frame), 1)) |>
    dplyr::mutate(penc1 = 1) |>
    dplyr::distinct() |>
    dplyr::mutate(prob = ptrt1*py1*psev1) # Prob of pattern is P(S)xP(T|S)xP(Y|S,T)


  if (index_weeks > 1){
  trt_indices <- purrr::accumulate(
    2:index_weeks,
    .init = week1,
    .f = function(prior, week){ # Takes in prior week and adds new week
      frame |>
        dplyr::cross_join(prior |> dplyr::filter(!!sym(paste0("y", week-1)) == 0)) |> # Limit prior week to where outcomes didn't happen
        dplyr::filter(!!sym(paste0("trt", week-1)) <= trt, # Filter to make sure treatment severity monotically increasing
                      !!sym(paste0("sev", week-1)) <= sev) |>
        dplyr::mutate(
          # Prior prob x prob of treatment = max of prior treatment or being treated x prob of outcome x max of being severe or already severe
          prob = prob*pmax(ptrt*penc, !!sym(paste0("trt", week-1)))*py*pmax(psev, !!sym(paste0("sev", week-1)))
        ) %>%
        dplyr::rename_with(
          .fn = ~ sub("([a-z]+)$", paste0("\\1", week), .x), # Add '2' to the end of matching columns
          .cols = sev:py                     # Specify the range of columns to rename
        )
    }
  )
  } else {trt_indices <- list(week1)}

  ## Frame of probabilities for visits contributed after final index
  post_trt_frame <- frame |>
    dplyr::distinct(sev, psev, trt, y, py) |>
    dplyr::mutate(penc = 1, enc = 1, ptrt = 1) # Treatment doesn't change

  if (fup_weeks > 0) {
    post_trt_indices <- purrr::accumulate(
      (index_weeks + 1):weeks,
      .init = trt_indices[[index_weeks]],
      function(prior, week){
        post_trt_frame |>
          dplyr::cross_join(prior |> dplyr::filter(!!sym(paste0("y", week-1)) == 0)) |>
          dplyr::filter(!!sym(paste0("trt", week-1)) == trt, ## treatment doesn't change
                        !!sym(paste0("sev", week-1)) <= sev) |>
          dplyr::mutate(prob = prob*py*pmax(psev, !!sym(paste0("sev", week-1)))) |>
          dplyr::rename_with(
            .fn = ~ sub("([a-z]+)$", paste0("\\1", week), .x), # Add '2' to the end of matching columns
            .cols = sev:ptrt                     # Specify the range of columns to rename
          )
      }
    )

    all_indices <- c(trt_indices, post_trt_indices[-1])
  } else {all_indices <- trt_indices}



  ## Final accumulation
  fup <- purrr::imap_dfr(
    all_indices, ## Don't need to repeat week 3.
    function(x, week){
      x |>
        dplyr::filter(!!sym(paste0("y", week))==1) |>
        dplyr::mutate(ytime = week)
    }
  ) |>
    dplyr::bind_rows(dplyr::last(all_indices) |>
                       dplyr::filter(!!sym(paste0("y", weeks)) == 0)) |> ## Remainder who didn't have outcome
    dplyr::mutate(trajid = row_number())

  fup

}


convert_to_long <- function(fup, weeks = 5, index_weeks = 3){

  tau <- weeks - index_weeks

  fup$trttime <- purrr::pmap_dbl(
    dplyr::select(fup, paste0("trt", 1:index_weeks)),
    .f = function(...){
      which(c(...)==1)[1]
    }
  )

  fup %>%
    dplyr::mutate(
      anyy = !is.na(ytime)) |>
    dplyr::select(c("trajid", paste0("sev",1:weeks), paste0("enc",1:weeks),
                    paste0("trt", 1:weeks), trttime, paste0("y", 1:weeks), anyy, ytime), prob) |>
    pivot_longer(
      cols =  c(sev1:!!sym(paste0("sev", weeks)),
                enc1:!!sym(paste0("enc", weeks)),
                trt1:!!sym(paste0("trt", weeks)),
                y1:!!sym(paste0("y", weeks))),
      names_to = c(".value", "visit"),  # Split names to create 'visit' column
      names_pattern = "([a-z]+)([0-9]+)"  # Match variable type (enc, sev, trt, y) and visit number
    ) %>%
    dplyr::select(trajid, visit, enc, sev, trt, y, ytime, trttime, anyy, prob) |>
    dplyr::mutate(
      visit = as.integer(visit)  # Ensure 'visit' is numeric
    ) |>
    dplyr::filter(!is.na(y),
                  is.na(trttime)|visit <= trttime) |>
    dplyr::mutate(
      trtcens = if_else(trt == 0, trttime, NA_real_) - 1,
      admincens = visit + tau,
      eventtime = pmin(trtcens, admincens, ytime, na.rm = T),
      delta = dplyr::case_when(
        is.na(ytime) ~ 0,
        eventtime == trtcens ~ 0,
        eventtime == ytime ~ 1,
        TRUE ~ 0
      ),
      t = eventtime - visit + 1
    ) |>
    filter(visit <= index_weeks) -> h
  #dplyr::select(trajid, sevid, visit, enc, sev0=sev, trt, t, delta, iptw, admin) -> h

  # Estimating IPTW bc gets complicated with combination of treatment probs and encounter probs
  # (because equal to treatment prob at time 1, then P(Trt)xP(enc) at later times)
  h |>
    dplyr::group_by(sev) |>
    dplyr::mutate(
      ps = sum(prob*trt)/sum(prob),
      iptw = trt/ps + (1-trt)/(1-ps)
    ) |> dplyr::ungroup()

}

## Convert the dataset that is one row per ID per index to a dataset that is
## one row per ID per index per time intereval
convert_long_to_longer <- function(h, fup, trt_enc_sev, weeks = 5, index_weeks = 3){
  h$final_t <- h$t
  survSplit(Surv(t, delta) ~ ., # splits each time to event into one row per interval of length 1
            data = h,
            cut = 1:(weeks - index_weeks), # each index contributes fup time points
            start = "t_in", # time "in" (open on left)
            end = "t_out", # time out (closed on right)
            id = "index",
            event = "delta") |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      sev = purrr::map2_dbl( # Time varying severity based on weeks in FUP dataset
        trajid,
        visit+t_in,
        ~unlist(fup[.x, paste0("sev", 1:weeks)])[.y]
      )
    ) |>
    dplyr::mutate(
      puncens = dplyr::if_else(
        trt==1|t_in==0|visit+t_in>index_weeks, ## Assign weight of 1 if treatment is 1, or
        1,                           ##  if first index (no chance of censoring), or
        1-trt_enc_sev[sev+1])        ## if time is after first 3 visits (no chance of censoring)
    ) |>
    dplyr::group_by(index) |>
    dplyr::mutate(
      cum_puncens = cumprod(puncens),
      ipcw = trt + (1-trt)/cum_puncens,
      sev0 = sev[1], # Baseline severity
      #ps = trt_by_sev[sev0+1], # Probability of treatment cond. on baseline severity
      #iptw = trt/ps + (1-trt)/(1-ps) # Inverse probabilty of treatment weights
    ) |>
    dplyr::ungroup()
}

