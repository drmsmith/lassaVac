# Preprocessing 

The this directory contains all the files needed for the pre-processing of the raw data files used in the CHIK-X similations. 


## UN-adjusted worldpop data 

(1) `./preprocessing/UN_worldpop/download_worldpop.R`
    - queries and downloads 1-km UN-adjusted worldpop data for a specified country and year 
    - requires custom function in utils.R
(2) `./preprocessing/UN_worldpop/process_worldpop.R`
    - yields df_burden_with_pop_size_2015.csv (used by mobility_processing)


## Mobility data

(1) `./mobility_and_p_spillover/mobility_processing.R`
    - yields the wide format mobility matrix from the long-format raw data 
    - requires df_burden_with_pop_size_2015_spillover i.e. burden with population size must be done before 
(2) `./mobility_and_p_spillover/p_spillover.R`
    - yields the p_spillover field i.e. proportion of total infections_mean = p(spillover)
    - requires population size so df_burden_pop2015 needed! 
(3) `./mobility_and_p_spillover/mat_mob_prob_fudge.R` 
    - yields the final mobility matrix with p(moving from a to b)


## PAHO case data 

(1) `./preprocessing/PAHO_curve_fitting/paho_data_long_to_wide.R`
    - yields df_paho_long_subset 
(2) `./preprocessing/PAHO_curve_fitting/process_outbreaks.R`
    - yields ls and df with outbreak data to which the shapes will be fitted
    - works with a copy of df_paho_long_subset which contains a final column named `outbreak` and is composed of `s` and `e` characters denoting the start and end of a curve 
        - note that curves cannot overlap 
        - for the purposes of fitting, some curves were adjusted by adding leaading and trailing zeroes   
(3) `./preprocessing/PAHO_curve_fitting/fit_hyperbolic_shapes.R`
    - yields final data frame with curve shape parameters from which parameters are sampled in the simualtions  