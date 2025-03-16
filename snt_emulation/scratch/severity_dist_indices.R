indices

purrr::imap_dfr(
  indices,
  function(x, y){
    x[2:3,] |>
      mutate(across(c(spt1:td0), ~as.numeric(str_extract(.x, "^[0-9]+")))) |>
      tidyr::pivot_longer(
        cols = c(spt1:td0),
        names_to = "study_trt",
        values_to = "n"
      ) |>
      dplyr::mutate(
        study = str_extract(study_trt, "^[A-Za-z]+"),  # Extract letters
        trt = as.numeric(str_extract(study_trt, "[0-9]+"))
      ) |>
      dplyr::arrange(study, name, trt) |>
      dplyr::mutate(scenario = y) |>
      dplyr::select(scenario, study, name, trt, n)
  }
) |>
  dplyr::group_by(scenario, study) |>
  dplyr::summarize(
    prop_high = sum(n[name == "High"])/sum(n),
    prop_high_trt = sum(n[name == "High" & trt == 1])/sum(n[trt==1])) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = scenario,
    names_from = study,
    values_from = c(prop_high, prop_high_trt)
  ) |>
  dplyr::select(
    scenario, prop_high_spt, prop_high_trt_spt,
    prop_high_snt, prop_high_trt_snt,
    prop_high_td, prop_high_trt_td
  ) |>
  gt(rowname_col = "scenario") |>
  gtsave("results/tables/index_prop_severity.rtf")

