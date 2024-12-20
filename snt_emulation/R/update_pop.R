
## Make generic frame of information needed at each visit.
make_trajectories <- function(trt_by_sev,
                              out_by_sev_trt0,
                              out_by_sev_trt1,
                              sev_prob = 0.25,
                              weeks = 5){

  dplyr::tibble(sev = 0:1) |>
    cross_join(
      dplyr::tibble(trt = 0:1)
    ) |>
    cross_join(
      dplyr::tibble(y = 0:1)
    ) |>
    dplyr::mutate(
      ptrt = trt*trt_by_sev[sev + 1] + (1-trt)*(1-trt_by_sev[sev + 1]),
      py = y*(trt*out_by_sev_trt1[sev + 1] + (1-trt)*out_by_sev_trt0[sev + 1]) +
        (1-y)*(trt*(1-out_by_sev_trt1[sev + 1]) + (1-trt)*(1-out_by_sev_trt0[sev + 1])),
      psev = sev*sev_prob + (1-sev)*(1-sev_prob)
    ) -> frame

  ## Make initial week of indices
  week1 <- frame |>
    set_names(paste0(names(frame), 1)) |>
    dplyr::mutate(prob = ptrt1*py1*psev1)


  trt_indices <- purrr::accumulate(
    2:3,
    .init = week1,
    .f = function(prior, week){ # Takes in prior week and adds new week

      frame |>
        dplyr::cross_join(prior |> dplyr::filter(!!sym(paste0("y", week-1)) == 0)) |> # Limit prior week to where outcomes didn't happen
        dplyr::filter(!!sym(paste0("trt", week-1)) <= trt, # Filter to make sure treatment severity monotically increasing
                      !!sym(paste0("sev", week-1)) <= sev) |>
        dplyr::mutate(
          # Prior prob x prob of treatment = max of prior treatment or being treated x prob of outcome x max of being severe or already severe
          prob = prob*pmax(ptrt, !!sym(paste0("trt", week-1)))*py*pmax(psev, !!sym(paste0("sev", week-1)))
        ) %>%
        dplyr::rename_with(
          .fn = ~ sub("([a-z]+)$", paste0("\\1", week), .x), # Add '2' to the end of matching columns
          .cols = sev:psev                     # Specify the range of columns to rename
        )
    }
  )

  ## Frame of probabilities for visits contributed after final index
  post_trt_frame <- frame |>
    dplyr::distinct(sev, psev, trt, y, py) |>
    dplyr::mutate(ptrt = 1) # Treatment doesn't change

  post_trt_indices <- purrr::accumulate(
    4:weeks,
    .init = trt_indices[[3]],
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

  ## Final accumulation
  fup <- purrr::imap_dfr(
    c(trt_indices, post_trt_indices[-1]), ## Don't need to repeat week 3.
    function(x, week){
      x |>
        dplyr::filter(!!sym(paste0("y", week))==1) |>
        dplyr::mutate(ytime = week)
    }
  ) |>
    dplyr::bind_rows(dplyr::last(post_trt_indices) |>
                       dplyr::filter(!!sym(paste0("y", weeks)) == 0)) |> ## Remainder who didn't have outcome
    dplyr::mutate(trajid = row_number())

  fup

}


convert_to_long <- function(fup, weeks = 5){
  fup %>%
    dplyr::mutate(trttime = dplyr::case_when(
      trt1 == 1 ~ 1,
      trt2 == 1 ~ 2,
      trt3 == 1 ~ 3,
      TRUE ~ NA_integer_),
      anyy = !is.na(ytime)) |>
    dplyr::select(c("trajid", paste0("sev",1:weeks), paste0("trt", 1:weeks), trttime, paste0("y", 1:weeks), anyy, ytime)) |>
    pivot_longer(
      cols =  c(sev1:!!sym(paste0("sev", weeks)),
                trt1:!!sym(paste0("trt", weeks)),
                y1:!!sym(paste0("y", weeks))),
      names_to = c(".value", "visit"),  # Split names to create 'visit' column
      names_pattern = "([a-z]+)([0-9]+)"  # Match variable type (enc, sev, trt, y) and visit number
    ) %>%
    dplyr::select(trajid, visit, sev, trt, y, ytime, trttime, anyy) %>%
    dplyr::mutate(
      visit = as.integer(visit)  # Ensure 'visit' is numeric
    ) |>
    dplyr::filter(!is.na(y),
                  is.na(trttime)|visit <= trttime) |>
    dplyr::mutate(
      trtcens = if_else(trt == 0, trttime, NA_real_) - 1,
      admincens = visit + 3,
      eventtime = pmin(trtcens, admincens, ytime, na.rm = T),
      delta = dplyr::case_when(
        is.na(ytime) ~ 0,
        eventtime == trtcens ~ 0,
        eventtime == ytime ~ 1
      ),
      t = eventtime - visit + 1
    ) |>
    filter(visit <= 3) -> h
  #dplyr::select(trajid, sevid, visit, enc, sev0=sev, trt, t, delta, iptw, admin) -> h

  h
}

## Convert the dataset that is one row per ID per index to a dataset that is
## one row per ID per index per time intereval
convert_long_to_longer <- function(h, fup, trt_enc_sev, weeks = 5){
  h$final_t <- h$t

  survSplit(Surv(t, delta) ~ ., # splits each time to event into one row per interval of length 1
            data = h,
            cut = 1:2, # each index contributes 3 time points
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
        trt==1|t_in==0|visit+t_in>3, ## Assign weight of 1 if treatment is 1,
        1,                           ##  if first index (no chance of censoring),
        1-trt_enc_sev[sev+1])        ## if time is after first 3 visits (no chance of censoring)
    ) |>
    dplyr::group_by(index) |>
    dplyr::mutate(
      cum_puncens = cumprod(puncens),
      ipcw = trt + (1-trt)/cum_puncens,
      sev0 = sev[1], # Baseline severity
      ps = trt_by_sev[sev0+1], # Probability of treatment cond. on baseline severity
      iptw = trt/ps + (1-trt)/(1-ps) # Inverse probabilty of treatment weights
    )
}

