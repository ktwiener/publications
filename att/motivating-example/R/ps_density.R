
ps_density <- function(dset, data.loc, nm){
  dset %>%
    dplyr::mutate(tpa_aje = if_else(tpa_aje, "tPA", "No tPA")) %>%
    ggplot2::ggplot(aes(x = ps, fill = tpa_aje)) +
    geom_density( alpha = 0.5) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0.25, 0.5, 0.75, 1)) +
    scale_fill_grey() +
    stat_density(geom="line",position="identity") +
    ggplot2::labs(x = "Propensity score", y = "Probability density", fill = "Treatment") +
    theme_classic()

  ggsave(sprintf("%s/motivating-example/results/output/ps_density_%s.jpg", data.loc, nm), width = 6.5, height = 5.5)
}

