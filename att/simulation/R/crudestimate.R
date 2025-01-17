


crudestimate <- function(cht){
  a <- with(cht, sum(a*y))
  b <- with(cht, sum(a*(1-y)))
  c <- with(cht, sum((1-a)*y))
  d <- with(cht, sum((1-a)*(1-y)))

  r1 <- a/sum(a,b)
  r0 <- c/sum(c,d)

  lnrr <- log(r1/r0)
  lnor <- log(r1*(1-r0)/(r0*(1-r1)))
  se_crude_lnor <- sqrt(sum(1/a, 1/b, 1/c, 1/d))
  se_crude_lnrr <- sqrt(sum(1/a, 1/c, -1/(a+b), -1/(c+d)))

  dplyr::tibble(
    params = paste0("ef_crude_", c("r1", "r0", "lnrr", "lnor")),
    roots = c(r1, r0, lnrr, lnor),
    se = c(NA, NA, se_crude_lnrr, se_crude_lnor)
  )
}







