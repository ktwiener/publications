adstroke <- readRDS(paste0(data.loc,"/motivating-example/adstroke.rds"))

crude_dth <- glm(death ~ rankin_aje, data = adstroke, family = binomial("logit"))
crude_tpa <- glm(tpa ~ rankin_aje, data = adstroke, family = binomial("logit"))

exp(crude_dth$coefficients[2])
exp(crude_tpa$coefficients[2])
