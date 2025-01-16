

make_table <- function(measures, measest = "lnrr"){
    measures |>
        dplyr::filter(grepl(measest, Param)) %>%
        dplyr::group_by(positivity, efftype, probw, Param) |>
        dplyr::summarize(
            delta = delta[1],
            expest = mean(Coef),
            bias = mean(bias),
            ese = sqrt(var(Coef)),
            ase = sqrt(mean(se^2)),
            cov = mean(cov),
            mse = sqrt(mean(mse))
        ) %>%
        ungroup %>%
        dplyr::transmute(
            effmeasure = measest,
            trteffect = factor(efftype, levels = c("None", "Homogeneous", "Heterogeneous"),
                               labels = c("No treatment effect", "Homogeneous treatment effect",
                                          "Heterogeneous treatment effect")),
            scenario = positivity,
            estimator = case_when(
                grepl("hajek|smr", Param) ~ "Hajek",
                grepl("ht", Param) ~ "Horvitz-Thompson",
                grepl("crude", Param) ~ "Crude"),
            weight = case_when(
                grepl("smr",Param) ~ "SMR",
                grepl("crude",Param) ~ "Crude",
                TRUE ~ "IPT"),
            pw = probw,
            across(c(bias, ase, ese, mse, cov), ~formatC(.x, digits = 2, format = "f"))
        )  %>%
        arrange(trteffect, scenario, weight, estimator, pw) %>%
        gt(groupname_col = c("trteffect")) %>%
        cols_label(
            effmeasure = "Measure",
            scenario = "Scenario",
            weight = "Weight",
            estimator = "Estimator",
            pw = "P(W=1)",
            bias = "Bias",
            ase = "ASE",
            ese = "ESE",
            mse = "rMSE",
            cov = "Coverage"
        )
}

boxplot <- function(measures, effect = "Homogeneous", ipttype = "hajek") {
  patt <- paste0(ipttype, "|smr")
  as_tibble(measures) |>
    dplyr::select(Param = params, positivity, efftype, probw, delta, bias) |>
    dplyr::filter(
      efftype == effect,
      grepl(patt, Param)
    ) |>
    dplyr::mutate(
      Param = factor(Param, levels = c("ef_hajek_lnrr", "ef_ht_lnrr", "ef_smr_lnrr"), labels = c("IPW", "IPW", "SMR")),
      pw = paste0("P(W=1) = ",probw/100),
      positivity = gsub("Full", "Complete", positivity)
    ) |>
    ggplot(mapping = aes(y = bias, x = Param, color = Param)) +
    geom_hline(yintercept = 0) +
    geom_boxplot(coef = 0, outlier.shape = NA, varwidth = T) +
    scale_y_continuous(limits = c(-1.5, 1.5)) +
    geom_violin(alpha = 0.5, aes(fill = Param)) +
    facet_grid(vars(positivity), vars(pw), switch = "y") +
    scale_color_grey(guide = "none") +
    scale_fill_grey() +
    theme_classic(base_size = 12) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_text(margin = margin(r = -50), size = 12),
          strip.text.y.left = element_text(angle = 0, size = 12, face = "bold"),
          strip.text.x.top = element_text(size = 12, face = "bold"),
          strip.background = element_blank(),
          strip.placement = "outside",
          panel.spacing.x = unit(0, 'lines'),
          plot.margin = unit(c(0, 0, 0, 0.6),
                             "inches"),
          legend.position = "bottom",
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12, face = "bold")
    ) +
    ylab("Error") +
    labs(fill = "Weight type")
}



