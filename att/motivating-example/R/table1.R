
make_table1 <- function(weights, wght = "dum"){
  weights$dum <- 1

  weights$agecat_aje <- cut(weights$age, breaks = c(40, 50, 60, 70, 80, 100), right = F,
                            labels = c("40-49", "50-59", "60-69", "70-79", "80-99"))
  tbl <- build_table(wght, weights)

  ## Weighted Ns
  ns <- weights %>%
    dplyr::summarize(
      tpan = sum(tpa*!!sym(wght)),
      notpan = sum((1-tpa)*!!sym(wght))
    )

  list(tbl = tbl, ns = ns)
}


## Table 1 functions
table1cont <- function(var, weights, wght){
  weights %>%
    bind_rows(weights %>% mutate(tpa = 99)) %>%
    group_by(tpa) %>%
    dplyr::summarize(
      cat = attr(weights[[var]], "label"),
      m = wtd.mean(!!sym(var), !!sym(wght)),
      v = wtd.var(!!sym(var), !!sym(wght)),
      sd = sqrt(v)
    ) %>%
    dplyr::transmute(
      cat,
      tpac = factor(tpa, levels = c(0, 1, 99), labels = c("notpap", "tpap", "totalp")),
      p = sprintf("%3.f (%3.1f)", m, sd)
    ) %>%
    select(cat, tpac, p) %>%
    tidyr::pivot_wider(names_from = tpac, values_from = p)
}

table1cat <- function(var, weights, wght, digs = 0){
  weights$dum <- 1
  weights %>%
    mutate(lvl = as.character(!!sym(var))) %>%
    group_by(lvl) %>%
    dplyr::summarize(
      cat = attr(weights[[var]], "label"),
      tpan = sum(tpa*!!sym(wght)),
      notpan = sum((1-tpa)*!!sym(wght))
    ) %>%
    mutate(
      totaln = tpan + notpan,
      totalp = 100*totaln/sum(totaln),
      tpap = 100*tpan/sum(tpan),
      notpap = 100*notpan/sum(notpan),
      across(c(totalp, tpap, notpap),
             ~formatC(.x, digits = 2, format = "f")),
      across(c(tpan, notpan, totaln),
             ~formatC(.x, digits = digs, format = "f"))
    ) %>%
    filter(lvl != "0")

}

build_table <- function(wght, weights){
  bind_rows(
    table1cont("age", weights, wght),
    purrr::map_dfr(
      c(ps_vars),
      table1cat,
      weights,
      wght
    )
  ) %>%
    select(cat, lvl, tpap, notpap, totalp)
}

