## Anonymous code for the submission of: 
Causal identification conditions for the effect of treatment in the treated: Illustration using the Northwest Germany Stroke Registry

### Motivating example
Code corresponding to the analysis of the motivating example is located in att/motivating-example. 
- Functions and utilities in R/


### Simulation
Code corresponding to the simulation is located in att/simulation. 
- Functions and utilities in R/
- Run files in scripts to generate data and analyze results
1. `possim.R` creates files that holds settings for each scenario and generates data.
     - will automatically create folder for `data/population/\<current-date\>/`
2. `mest.R` analyzes each sample using M-estimation
    - will need to create folders for `data/\<date of population creation\>/\<measure (or or rr)\>/` before running.
3. `crude.R` estimates the crude estimates.
4. `measures.R` analyzes the samples from each simulation and estimates the simulation performance.
   - will need to create a folder `data/results/raw`
5. `results.R` uses results from measures to create output.
   - will need to create folders `data/results/tables` and `data/results/figures`
