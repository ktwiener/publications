library(dplyr)
library(gt)


index <- make_index_table(sample)


index |>
  gt::gt(rowname_col = "name", groupname_col = "grp") |>
  gt::tab_spanner(
    label = "Target trial",
    columns = c("spt1", "spt0")
  ) |>
  gt::tab_spanner(
    label = "SNT emulation without laboratory requirement",
    columns = c("snt1", "snt0")
  ) |>
  gt::tab_spanner(
    label = "SNT emulation with laboratory requirement",
    columns = c("td1", "td0")
  ) |>
  gt::cols_label(
    spt1 = "Initiators",
    spt0 = "Non-initiators",
    snt1 = "Initiators",
    snt0 = "Non-initiators",
    td1 = "Initiatiors",
    td0 = "Non-initiators",
  ) |>
  gt::gtsave(filename = "index_table.rtf", path = "snt_emulation/results/tables/")
