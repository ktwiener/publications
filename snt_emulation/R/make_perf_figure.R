
make_perf_figure <- function(scen){
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
          lntruth = truth,
          truth = exp(truth),
          lnrr = expval,
          rr = exp(expval)
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
              lntruth = truth,
              truth = exp(truth),
              bias = bias,
              lnrr = expval,
              rr = exp(expval)
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
    dplyr::filter(!(pop == "crude" & (analyze %in% c("snt", "td")) & target == "spt"))
}
