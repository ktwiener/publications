#############################################
# Program: utils.R
# Programmer: Catie Wiener
#
# Purpose Random utilities
# CDL Review: 01.19.2024
#############################################

# Expit function - Inverse of logit function
# This will return a probability from a
# specified log odds, x
expit <- function(x) 1/(1+exp(-x))


# Logit function - Generates a log-odds from
# a specified probability x
logit <- function(x) log(x/(1-x))


# Function to find the minimum of a vector
# while dealing with the fact that some of the
# vectors that are passed through will be NA.
# This will take any number of arguments, as
# represented by '...' in the function call.
find_min <- function(..., na.rm = TRUE) {
  if (length(...)>0) {min(..., na.rm = na.rm)}
  else {NA}
}

# Derives the outcome probabilities at each
# individual potential index so that the
# specified probability is over the course of
# a year.
individual_prob <- function(p, time){
  1-(1-p)^(1/time)
}

## Possible combinations of n binary variables.
# Function to generate combinations with non-decreasing rows
generate_combinations <- function(n_binaries) {
  # Create all possible combinations using expand.grid
  expand.grid(replicate(n_binaries, 0:1, simplify = FALSE))

}

