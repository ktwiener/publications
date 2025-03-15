make_perf_tabler <- function(scen){
  effects <- readRDS(paste0(dataloc, "performance/scenario", scen, ".rds"))

  spt <- effects$spt_res |>
    dplyr::filter(param == "lnrr") |>
    ungroup() |>
    dplyr::transmute(
      analyze = "spt",
      target = "spt",
      pop = "",
      bias = bias,
      rmse = rmse,
      ese = ese
    )

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
              rmse = rmse,
              ese = ese
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
    dplyr::select(scenario, analyze, target, pop, bias, rmse, ese)
}


