# Conditional probs of outcome
## Make sure probabilities sums to 1.
sum(pop$prob)

## Can check probabilities line up with specifications/expectations
# Prob of trajectories
pop %>%
  group_by(sev1, sev2, sev3) %>%
  dplyr::summarize(
    sevp = sum(prob)
  )


## RR of 0.5 at time 1
pop %>%
  group_by(sev1, y01) %>%
  summarize(p = sum(prob)) %>%
  mutate(t = sum(p), y0p = p/t) -> y0p

pop %>%
  group_by(sev1, y11) %>%
  summarize(p = sum(prob)) %>%
  mutate(t = sum(p), y1p = p/t) -> y1p
y1p$y1p/y0p$y0p

## At time 3?
pop %>%
  group_by(sev1, sev2, sev3) %>%
  summarize(y1p = sum(y13*prob),
            y0p = sum(y03*prob),
            tot = sum(prob))
pop %>%
  group_by(sev1, sev2, sev3, y13) %>%
  summarize(p = sum(prob))
mutate(t = sum(p), y1p = p/t) -> y1p
y1p$y1p/y0p$y0p
