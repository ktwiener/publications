### Scenario 1
### Weekly outcome probabilities: 9% versus 16%
#### P(Enc | Low/High) = 0.4
#### RR|Low/High = 0.5

library(tidyverse)


# Solve for p
# Here, m indicates the multiple by which we determine the probability of treatment
# among individuals with high severity disease by multiplying m and the probability
# of treatment among individuals with low severity disease


solve_for_ptrt <- function(p, m, p_low1, p_low2, p_low3, p_high1, p_high2, p_high3,
                           p_enc2, p_enc3, p_outc1, p_outc2){

  # We want to solve the following equation so that the marginal probability of being
  # treated at least once over the 3 weeks is 0.5 in the entire target population.
  # P(1 - [Trt at 1])P(1 - [Trt at 2])P(1 - [Trt at 3]) - 0.5

  # 1 - P Trt 1:
  (1-((p_low1 * p) + (p_high1 * m * p))) *
    # 1 - P Trt 2
    (1 - ((1-p_outc1)*(1-((p_low1 * p) + (p_high1 * m * p)))*
            (p_enc2)*((p_low2 * p) + (p_high2 * m * p)))) *
    # 1 - P Trt 3
    (1 - ((1-p_outc1)*(1-p_outc2)*(1-((p_low1 * p) + (p_high1 * m * p)))*
            (1 - ((1-p_outc1)*(1-((p_low1 * p) + (p_high1 * m * p)))*
                    (p_enc2)*((p_low2 * p) + (p_high2 * m * p))))*
            p_enc3*((p_low3 * p) + (p_high3 * m * p)))) - 0.5

}


probs <- tibble(
  m = 2,
  p_enc_low = 0.4,
  p_enc_high = c(0.4, 0.6),
  p_outc_low = 0.09,
  p_outc_high = 0.16
) %>%
  mutate(

    # marginal probability of low severity at week 1, 2, and 3
    p_low1 = 0.75,
    p_low2 = 0.5,
    p_low3 = 0.75,

    # marginal probability of high severity at weeks 1, 2, and 3
    p_high1 = 1-p_low1,
    p_high2 = 1-p_low2,
    p_high3 = 1-p_low3,

    p_enc1 = 1,
    # Sum of the weighted probability of encs under low versus high severity
    p_enc2 = (p_enc_low * 0.5) + (p_enc_high * 0.5),
    p_enc3 = (p_enc_low * 0.25) + (p_enc_high * 0.75),

    # Probability of outcomes
    p_outc1 = (p_outc_low * 0.75) + (p_outc_high * 0.25),
    p_outc2 = (p_outc_low * 0.5) + (p_outc_high * 0.5),
    p_outc3 = (p_outc_low * 0.25) + (p_outc_high * 0.75)
  ) %>%
  mutate(
    p_low = purrr::pmap_dbl(
      list(m, p_low1, p_low2, p_low3, p_high1, p_high2, p_high3,
           p_enc2, p_enc3, p_outc1, p_outc2),
      ~ uniroot(
        solve_for_ptrt,
        interval = c(0, 1),
        m = ..1,
        p_low1 = ..2,
        p_low2 = ..3,
        p_low3 = ..4,
        p_high1 = ..5,
        p_high2 = ..6,
        p_high3 = ..7,
        p_enc2 = ..8,
        p_enc3 = ..9,
        p_outc1 = ..10,
        p_outc2 = ..11
      )$root
    ),
    p_high = 2 * p_low
  )

