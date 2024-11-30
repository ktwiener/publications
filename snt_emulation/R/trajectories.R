
## All possible combinations of 0,1 over 3 weeks
## Will use this to construct severity trajectories, treatment trajectories,
## and outcomes
binaries <- generate_combinations(weeks)

## Filter to rows where binaries are increasing (progressive disease only)
binaries_inc <- binaries[apply(binaries, 1, function(x) all(diff(x) >= 0)), ]

## Probability of each severity trajectory
severity <- binaries_inc
names(severity) <- paste0("sev", 1:weeks)
severity$prob_traj <- 0.25
severity$sevid <- 1:nrow(severity)
## All possible potential outcomes under no treatment
p0out <- binaries
names(p0out) <- paste0("y0", 1:weeks)

## All possible potential outcomes under treatment
p1out <- binaries
names(p1out) <- paste0("y1", 1:weeks)

## All possible treatment patterns
treatments <- binaries
names(treatments) <- paste0("trt", 1:weeks)

## All possible encounter patterns.
encounters <- binaries
names(encounters) <- paste0("enc", 1:weeks)
encounters <- encounters |>
  dplyr::mutate(enc1 = 1) |>
  dplyr::distinct()

## All possible trajectories with potential outcomes
## This is the blank canvas for all scenarios :)
trajectories <-
  purrr::reduce(
    list(severity, encounters, p0out, p1out),
    dplyr::cross_join
)

## Treatment trajectories with severity


