
ps_table_by_pct <- function(adstrokeps, nm, data.loc){
  quants <- quantile(adstrokeps$ps, na.rm = T,
                     probs = c(0, 5, 10, 25, 50, 75, 90, 95, 100)/100)


  x <- gsub("%", "", names(quants))

  lbls <- gsub("<100", "100", paste0(lag(x), " to <", x)[-1])
  adstrokeps %>%
    group_by(tpa, death) %>%
    tally() %>%
    mutate(total = sum(n)) %>%
    filter(death == 1) %>%
    mutate(risk = n/total) %>%
    ungroup %>%
    mutate(
      rr = risk[tpa == 1]/risk[tpa==0],
      or = risk[tpa==1]*(1-risk[tpa==0])/(risk[tpa==0]*(1-risk[tpa==1]))
    ) %>%
    mutate(ps_cat = "Overall") %>%
    tidyr::pivot_wider(id_cols = c(ps_cat, rr, or), values_from = c(total, n, risk), names_from = tpa) %>%
    select(ps_cat, total_1, n_1, risk_1, total_0, n_0, risk_0, rr, or) -> overall

  adstrokeps %>%
    mutate(
      ps_cat = cut(ps, breaks = quants, right = FALSE, include.lowest = T),
      ps_cat = factor(ps_cat, labels = lbls)
    ) -> adstrokeps_cat

  adstrokeps_cat %>%
    group_by(tpa) %>%
    dplyr::summarize(mean_ps = formatC(mean(ps), digits = 2, format = "fg")) %>%
    tidyr::pivot_wider(values_from = mean_ps,
                       names_from = tpa, names_prefix = "score") -> overall_score


  overall <- bind_cols(overall, overall_score)

  adstrokeps_cat %>%
    group_by(tpa, ps_cat) %>%
    dplyr::summarize(mean_ps = formatC(mean(ps), digits = 2, format = "fg")) %>%
    arrange(desc(ps_cat)) %>%
    tidyr::pivot_wider(id_cols = ps_cat, values_from = mean_ps,
                       names_from = tpa, names_prefix = "score") -> meanscore

  adstrokeps_cat %>%
    group_by(ps_cat, tpa, death) %>%
    tally() %>%
    mutate(total = sum(n),
           risk = n/total) %>%
    tidyr::pivot_wider(id_cols = c(ps_cat, death), values_from = c(total, n, risk), names_from = tpa) %>%
    dplyr::mutate(
      across(starts_with("total"), ~if_else(is.na(.x) & lag(ps_cat)==ps_cat, lag(.x), .x)),
      across(matches("n_|risk_"), ~if_else(is.na(.x), 0, as.numeric(.x))),
      across(c(n_1, risk_1), ~if_else(is.na(total_1), NA_real_, .x)),
      across(c(n_0, risk_0), ~if_else(is.na(total_0), NA_real_, .x))
    ) %>%
    filter(death == 1) %>%
    dplyr::mutate(
      or = risk_1/(1-risk_1)/(risk_0/(1-risk_0)),
      rr = risk_1/risk_0
    ) %>%
    arrange(desc(ps_cat)) %>%
    ungroup %>%
    left_join(meanscore, by = c("ps_cat")) %>%
    bind_rows(overall) %>%
    mutate(
      dplyr::across(c(risk_0, risk_1, or, rr),
                    ~if_else(!is.na(.x),
                             formatC(.x, digits = 2, format = "f"),
                             "")),
      dplyr::across(c(total_1, n_1, total_0, n_0),
                    ~if_else(!is.na(.x),
                             formatC(.x, digits = 0, format = "f"),
                             ""))
    )  %>%
    select(ps_cat, score1, total_1, n_1, risk_1,
           score0, total_0, n_0, risk_0, or, rr) %>%
    mutate(
      across(starts_with("score"), ~if_else(is.na(.x), "", .x))
      )-> pstable

  pstable %>%
    gt::gt() %>%
    gt::gtsave(sprintf("%s/motivating-example/results/output/ps_table_%s.rtf", data.loc, nm))

  return(pstable)
}

