
#ps_vars <- c("age", "I(age^2)", "I(age^3)", ps_vars[ps_vars != "agecat_aje"])
ps_formula <- sprintf("%s ~ %s", "tpa_aje", paste0(ps_vars, collapse = "+"))

# glm(tpa ~ ps_vars_conscious_ajeSomnolent + ps_vars_conscious_ajeComatose,
#     data = adstroke_dum,
#     family = binomial)

adstroke6000ps$ps_test <-
  predict(
    glm(as.formula(ps_formula),
        data = adstroke6000ps,
        family = binomial),
    newdata = adstroke6000ps,
    type = "response")


adstroke_dum <- convert_variables(adstroke, ps_vars) # Convert to dummies
ps_vars_dum <- names(adstroke_dum)[grepl("ps_vars_", names(adstroke_dum))]

ps_formula <- sprintf("%s ~ %s", "tpa_aje", paste0(ps_vars_dum, collapse = "+"))


adstroke$ps_test <-
  predict(
    glm(as.formula(ps_formula),
        data = adstroke_dum,
        family = binomial),
    type = "response")



score <- adstrokeps$ps
adstrokeps$ipt <- with(adstrokeps, tpa/score + (1-tpa)/(1-score))
adstrokeps$smr <- with(adstrokeps, tpa*1 + (1-tpa)*score/(1-score))
adstrokeps |>
  dplyr::summarize(
    risk1_ipt = sum(tpa*death*ipt)/6000,
    risk0_ipt = sum((1-tpa)*death*ipt)/6000,
    rr_ipt = risk1_ipt/risk0_ipt,

    risk1_smr = sum(tpa*death*smr)/sum(tpa*smr),
    risk0_smr = sum((1-tpa)*death*smr)/(sum((1-tpa)*smr)),
    rr_smr = risk1_smr/risk0_smr
  )
exp(results6000[c(31,35), 2])

