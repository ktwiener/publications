
make_table2 <- function(adstroke, mests){
  cruderr <- glm(death ~ tpa, family = binomial(link = "log"), data = adstroke)

  crude_rr_est <- broom::tidy(cruderr, conf.int = T) |>
    dplyr::filter(term == "tpa") |>
    dplyr::transmute(
      lbl = "Crude",
      Estimate_rr = exp(estimate),
      ci_rr = sprintf("%3.2f, %3.2f", exp(conf.low), exp(conf.high))
    )

  crudeor <- glm(death ~ tpa, family = "binomial", data = adstroke)

  crude_or_est <- broom::tidy(crudeor, conf.int = T) |>
    dplyr::filter(term == "tpa") |>
    dplyr::transmute(
      lbl = "Crude",
      Estimate_or = exp(estimate),
      ci_or = sprintf("%3.2f, %3.2f", exp(conf.low), exp(conf.high))
    )

  crude_ests <- crude_rr_est |>
    dplyr::left_join(crude_or_est, by = "lbl")

  wght_ests <- mests |>
    dplyr::filter(grepl("lnrr|lnor", Param)) |>
    dplyr::transmute(
      lbl = if_else(grepl("ipt", Param), "IPT weights", "SMR weights"),
      esttype = if_else(grepl("or", Param), "or", "rr"),
      Estimate = exp(Coef),
      ci = sprintf("%3.2f, %3.2f", exp(LCL), exp(UCL))
    ) |>
    tidyr::pivot_wider(id_cols = lbl, names_from = esttype, values_from = c(Estimate, ci)) |>
    dplyr::select(lbl, ends_with("rr"), ends_with("or"))

  bind_rows(crude_ests,
            dplyr::tibble(lbl = "Weighted models",  ci_rr = " ", ci_or = " "),
            wght_ests) |>
    dplyr::mutate(
      across(starts_with("Estimate"), ~if_else(is.na(.x), " ", sprintf("%3.2f", .x)))
    )
}
