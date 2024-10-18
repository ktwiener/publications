
expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

predict_ps <- function(adstroke,  mests, ps_vars){

  adstroke_dum <- convert_variables(adstroke, ps_vars)

  ps_fit <- mests$Coef[grepl("intercept|ps_vars_", mests$Param)]

  int <- rep(1, nrow(adstroke))

  covs <- data.matrix(adstroke_dum %>% select(starts_with("ps_vars_")))

  covs <- cbind(int, covs)

  adstroke$ps <- expit(covs %*% ps_fit)

  adstroke$ipt <- with(adstroke, tpa/ps + (1-tpa)/(1-ps))
  adstroke$smr <- with(adstroke, tpa*1 + (1-tpa)*ps/(1-ps))

  return(adstroke)
}

