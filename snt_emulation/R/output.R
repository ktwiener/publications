library(dplyr)
library(gt)
index_table <- index_by_scenario(sample_pop, sample_long_pop)
index_table |>
  select(grp, name, spt1, spt0,
         snt1, snt0, td1, td0) |>
  gt()
