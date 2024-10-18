# Inclusion/exclusion for positivity paper


make_incl_pos <- function(stroke){

  dt <- stroke %>%
    dplyr::mutate(
      incl_1_yrs = admity >= 2020,
      incl_2_stroke = (stroke == "infarkt(i63)"),
      incl_3_age = age >= 40 & age < 100,
      incl_4_trt = tpa %in% 0:1 & (iattype2020 %in% c(9)|(iattype2020 == 1 & tpa == 1)),
      incl_5_dth = !is.na(death))

  labels = c("All stroke inpatient admissions from 2014-2021 in participating centers",
             "Patient admitted in 2020-2021",
             "Patient diagnosed with ischemic stroke",
             "Patient age >= 40 and < 100",
             "Patient did not receive intraarterial therapy",
             "Patient not missing mortality status"
  )

  list(dt,
       labels)
}
