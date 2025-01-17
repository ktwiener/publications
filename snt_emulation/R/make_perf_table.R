make_perf_table <- function(scen){
  effects <- readRDS(paste0(dataloc, "performance/scenario", scen, ".rds"))

  spt <- purrr::imap_dfr(
    effects$spt_res,
    function(perf, nm){
      perf |>
        dplyr::filter(param == "lnrr") |>
        dplyr::transmute(
          analyze = "spt",
          target = "spt",
          pop = nm,
          bias = bias,
          ese = ese,
          rmse = rmse,
          bias_mcse
        )
    }
  ) |>
    dplyr::ungroup()

  snt <- purrr::imap(
    effects[-1],
    function(perf, nm){
      nms <- strsplit(nm, "_")[[1]]
      purrr::imap_dfr(
        perf,
        function(each, lbl){
          each |>
            dplyr::filter(param == "lnrr") |>
            dplyr::transmute(
              pop = lbl,
              bias = bias,
              ese = ese,
              rmse = rmse,
              bias_mcse
            )
        }
      ) |>
        dplyr::mutate(
          analyze = nms[1],
          target = nms[2]
        )
    }

  ) |>
    dplyr::bind_rows() |>
    ungroup()

  bind_rows(spt, snt) |>
    dplyr::mutate(scenario = scen) |>
    tidyr::pivot_wider(id_cols = c(scenario, analyze, target), names_from = pop, values_from = c(bias, bias_mcse, rmse, ese)) |>
    dplyr::select(scenario, analyze, target, ends_with("crude"), ends_with("ate"), ends_with("att"))
}


