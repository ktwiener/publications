

create_trt_var <- function(stroke){
  stroke
}

make_ad <- function(stroke, crit = make_incl_pos){
  stroke_list <- crit(stroke)
  stroke_incl <- stroke_list[[1]]
  incl_crit <- colnames(stroke_incl)[grepl("incl", colnames(stroke_incl))]

  purrr::accumulate(
    bind_cols(TRUE, stroke_incl[incl_crit]),
    function(x, y) x & y) %>%
    purrr::map_dfr(sum, na.rm = T) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::transmute(
      criteria = stroke_list[[2]],
      type = "Inclusion",
      removed = lag(value) - value,
      remaining = value,
      across(c(removed, remaining), ~replace(.x, is.na(.x), 0))
    ) -> attr_tbl

  ad <- dplyr::filter(stroke_incl, if_all(starts_with("incl"))) %>%
    create_aje_vars()

  list(
    attrition = attr_tbl,
    data = ad
  )
}

## Vars in Tobias paper:
### age (5-year increments),
### gender,
### Rankin scale (28) at the time of admission (1–3, 4–5, 6),
### time from event to hospital admission (<1 hour, 1–3 hours, >3 hours),
### paresis (monoplegia, hemiplegia, tetraplegia),
### state of consciousness (awake, somnolent, comatose),
### type of admitting ward (normal, stroke unit, intensive care unit),
### transportation to the hospital (emergency medical service, other qualified transport, private, other),
### aphasia,
### hypertension
### diabetes mellitus
### atrial fibrillation,
### history of other cardiac illnesses,
### previous stroke,
### admitting clinical center.

## Time varying vars (with interaction)
## "Last three 6-month periods compared with the first"
## age (<70 years vs. 70 years),
## time from symptoms to admission to the hospital (<1 hour, 1–3 hours, >3 hours),
## Rankin scale (1–5, 6)

create_aje_vars <- function(dset){
  age_chunk <- 5
  dset %>%
    dplyr::mutate(
      agecatn = floor(age/age_chunk)
    ) %>%
    group_by(agecatn) %>%
    dplyr::mutate(
      ### age (5-year increments),
      agecat_aje = factor(
        if_else(!is.na(agecatn),
                paste0(agecatn*age_chunk, "-", (agecatn+1)*age_chunk-1),
                NA_character_)
      )
    ) %>%
    ungroup %>%
    dplyr::mutate(
      tpa_aje = as.logical(tpa),
      ### gender,
      gender_aje_binary = replace_na(sex - 1),
      gender_aje = sex,
      ### Rankin scale (28) at the time of admission (1–3, 4–5, 6),
      rankin_aje = factor(admit_rank, 1:5, c(rep("1-3", 3), rep("4-5", 2))),
      ### time from event to hospital admission (<1 hour, 1–3 hours, >3 hours),
      sxhours_aje = factor(sxhours,
                           levels = c(1:10),
                           labels = c("<= 1 hour",
                                      rep(">1-3 hours",2),
                                      rep("3-4 hours",2),
                                      rep("> 4 hours", 5))),
      ### paresis (monoplegia, hemiplegia, tetraplegia),
      paresis_aje = replace_na(paresis),
      ### state of consciousness (awake, somnolent, comatose),
      conscious_aje = factor(conscious, levels = c(1:3), labels = c("Awake", "Somnolent/Comatose", "Somnolent/Comatose")),
      ### type of admitting ward (normal, stroke unit, intensive care unit),
      ward_aje = factor(ward, 1:3, c("Stroke unit", "ICU", "General")),
      ### transportation to the hospital (emergency medical service, other qualified transport, private, other),
      ### Coding for this:
      ### 1: by friends/ family/ own -> Private
      ### 2: by emergeny physician/ helicopter -> Emergency medical service
      ### 3: ambulance -> Emergency medical service
      ### 4: other/ inhouse -> Other
      transport_aje = factor(transport, 1:4, c("Private", rep("Emergency medical service", 2), "Other")),
      ### aphasia,
      aphasia_aje = replace_na(aphasia),
      ### hypertension
      htn_aje = replace_na(hxhtn),
      ### diabetes mellitus
      diab_aje = replace_na(hxdiab),
      ### atrial fibrillation,
      afib_aje = replace_na(afib),
      ### history of other cardiac illnesses,
      cardiac_aje = replace_na(hxcardiac),
      ### history of MI
      hxmi_aje = replace_na(hxmi),
      ### previous stroke,
      hxstroke_aje = replace_na(hxstroke),
      ### admitting clinical center.
      center_aje = as.character(center),
      ### Death
      death_aje = as.logical(death),
      ### Quarter
      quarter_aje = factor(admitq),
      ### Living/Home
      home_care_aje = factor(dplyr::case_when(
        home == 2 ~ "At institution",
        care == 1 ~ "Independent at home",
        care == 2 ~ "In care at home",
        TRUE ~ NA_character_
      ))


      ) -> hold

  for (aje in aje_labels()){
    attr(hold[[names(which(aje_labels()==aje))]], "label") <- aje
  }

  return(hold)
}

replace_na <- function(x, lvls = 0:1){
  replace(x, !x %in% lvls, NA_real_)
}



aje_labels <- function(){
  c(
    "agecat_aje"    =  "Age",
    "tpa_aje"       =  "tPA treatment",
    "gender_aje"    =  "Sex",
    "rankin_aje"    =  "Rankin scale at admission",
    "sxhours_aje"   =  "Time from stroke to admission",
    "paresis_aje"   =  "Paresis",
    "conscious_aje" =  "Consciousness at admission",
    "ward_aje"      =  "Admission ward",
    "transport_aje" =  "Transport to hospital",
    "aphasia_aje"   =  "Aphasia",
    "htn_aje"       =  "Hypertension",
    "diab_aje"      =  "Diabetes",
    "afib_aje"      =  "Atrial fibrillation",
    "cardiac_aje"   =  "History of cardiovascular disease (CVD, Afib, MI)",
    "hxstroke_aje"  =  "Prior stroke",
    "center_aje"    =  "Admitting center",
    "death_aje"     =  "In-hospital death",
    "quarter_aje"   =  "Quarter-Year of admission",
    "home_care_aje" =  "Pre-stroke Living and care situation")
}



