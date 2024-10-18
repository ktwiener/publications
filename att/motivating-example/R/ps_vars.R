ps_vars_all <- c("agecat_aje",
             "gender_aje_binary",
             "home_care_aje",
             "rankin_aje",
             "sxhours_aje",
             "paresis_aje",
             "aphasia_aje",
             "conscious_aje",
             "ward_aje",
             "htn_aje",
             "diab_aje",
             "afib_aje",
             "hxstroke_aje")

ps_vars <- ps_vars_all
## 1-9 works due to conscious being combined.
## 1-13 works due to messing with age categories
## >90 causes small cell sizes, causes fitting to diverge. Not sure why!!


## Conscious -> not working bc of small cell size for comatose
### m-estimation 4.4985 vs glm 1.7554
### combined somnolent and comatose, now it works
