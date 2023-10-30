# Preprocessing

The this directory contains all the data files and scripts needed for the pre-processing of the raw data files used in the CHIK-X simulations. 

```
./preprocessing
│
│   README.md
│
├───data
│   │   df_countries_burden_estimates_cepi.csv      // Salje et al., infection burden estimates
│   │   df_burden_with_pop_size_2015.csv            // Salje et al., with 2015 UN-adj worldpop data
│   │   KCMD_EUI_GMP_Estimated_trips.csv            // Global Mobilities Project raw data (n trips)
│   │   mat_mobility_2015.csv                       // wide format matrix for 2015 mobility data
│   │   PAHO_data_raw_wide.csv                      // PAHO Chikungunya monthly new cases reported
│   │
│   ├───2015_UNadj_worlpop_data         // 2015 UN-adjusted worldpop tiffs for 113 countries
│   │
│   └───PAHO_case_data_2014-2017        // numerous processed PAHO data/outbreaks/fits
│
│       // scripts processing the data above 
│       // more details in the sections below 
│
├───UN_worldpop                     // download and process UN-adjusted worldpop data
│       download_worldpop.R
│       process_worldpop.R
│       utils.R
│
├───mobility_and_p_spillover        // process GMP data underpinning CHIK-X spread
│       mobility_processing.R
│       p_spillover.R
│       mat_mob_prob_fudge.R
│
└───PAHO_curve_fitting              // fitting hyperbolic curves to PAHO outbreak data
    │   paho_data_long_to_wide.R
    │   process_outbreaks.R
    │   fit_hyperbolic_shapes.R
    │
    └───curves_man_adj              // visualisations to compare fitted curve to data 
```


Note: the details below are not exhaustive but outline the structure of the repository and the order in which `../1_CHIK-X_preprocessing_run.R` would source the files. More information about particular files or processing steps can be found in the comments within the scripts. 


## UN-adjusted worldpop data

1) `download_worldpop.R`

    - queries and downloads 1-km UN-adjusted worldpop data for a specified country and year
    - requires custom function in utils.R

2) `process_worldpop.R`

    - yields df_burden_with_pop_size_2015.csv (used by mobility_processing)


## Mobility data

1) `mobility_processing.R`

    - yields the wide format mobility matrix from the long-format raw data
    - requires df_burden_with_pop_size_2015_spillover i.e. burden with population size must be done before

2) `p_spillover.R`

    - yields the p_spillover field i.e. proportion of total infections_mean = p(spillover)
    - requires population size so df_burden_pop2015 needed!

3) `mat_mob_prob_fudge.R`

    - yields the final mobility matrix with p(moving from a to b)


## PAHO case data

1) `PAHO_curve_fitting/paho_data_long_to_wide.R`

    - yields df_paho_long_subset
    - outbreak reports downloaded from https://www3.paho.org/data/index.php/en/mnu-topics/chikv-en/550-chikv-weekly-en.html

1) `process_outbreaks.R`

    - yields ls and df with outbreak data to which the shapes will be fitted
    - works with a copy of df_paho_long_subset which contains a final column named `outbreak` and is composed of `s` and `e` characters denoting the start and end of a curve
        - note that curves cannot overlap
        - for the purposes of fitting, some curves were adjusted by adding leaading and trailing zeroes

1) `fit_hyperbolic_shapes.R`

    - yields final data frame with curve shape parameters from which parameters are sampled in the simualtions