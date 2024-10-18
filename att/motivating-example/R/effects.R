or_func <- function(risk1, risk0){
  risk1*(1-risk0)/(risk0*(1-risk1))
}

rr_func <- function(risk, index){
  risk[index]/risk[!index]
}
