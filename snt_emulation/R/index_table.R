
# Table S3: Descriptive statistics on simulated patients and indices
make_index_table <- function(spop, # sample population
                        pref  # prefix for column names
                        ){

  # Function to output and format median and IQR.
  median_iqr <- function(x){
    qs = quantile(x, c(0.25, 0.5, 0.75))

    sprintf("%s (%s)",formatC(qs[2], format = "fg", digits = 2), formatC(qs[3] - qs[2], format = "fg", digits = 0))
  }

  spop |>
    dplyr::group_by(sim, id) |>
    dplyr::mutate(
      ever_trt = max(trt), # Ever treated ("initiators")
      n_high = sum(sev), # Number of high severity indices per person
      n_low = sum(1-sev) # Number of low severity indices per person
    ) |>
    dplyr::group_by(ever_trt, sim) |> # Group within ever treated and simulation
    dplyr::summarize(
      ppl = n_distinct(id), # Number of distinct people
      high_ind = sum(sev), # Number of high severity indices
      low_ind = sum(1-sev), # Number of low severity indices
      high_pp = mean(n_high), # Average number of high indices per person
      low_pp = mean(n_low) # Average number of low indices per person
    ) |>
    dplyr::summarize(
      # Across all measures, output median and iqr
      across(c(ppl, high_ind, low_ind, high_pp, low_pp), median_iqr)
    ) |>
    # Pivot into the right shape for table
    tidyr::pivot_longer(cols = c(ppl, high_ind, low_ind, high_pp, low_pp)) |>
    tidyr::pivot_wider(id_cols = name, names_from = ever_trt, names_prefix = pref) |>
    # Create grouping for table
    dplyr::mutate(grp = c("",
                          rep("Number of indices included in the study", 2),
                          rep("Average number of indices per person", 2)))

}

index_by_scenario <- function(sample_pop, sample_long_pop){
  purrr::reduce(
    purrr::map2(
      list(sample_pop,
           sample_long_pop |> dplyr::filter(t_in == 0),
           sample_long_pop |> dplyr::filter(t_in == 0, enc == 1)),
      c("spt", "snt", "td"),
      index_table
    ),
    left_join,
    by = c("grp", "name")
  )
}

