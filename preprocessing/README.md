# Preprocessing

The this directory contains all the data files and scripts needed for the pre-processing of the raw data files used in the CHIK-X simulations. 

```
./preprocessing
│
│   README.md
│
├───data                        // most data files will be saved here 
│
│
├───mobility_and_p_spillover    // process GMP data underpinning CHIK-X spread
│
├───PAHO_data               // smoothing and converting monthly PAHO outbreak data to daily 
│
├───suitability             // download shape files and aggregate pixel-level suitability 
│
├───UN_worldpop             // download and process UN-adjusted worldpop data
│
└───zika                    // visualise zika spread rates 
```


Note: the details below are not exhaustive but outline the structure of the repository and the order in which `../1_CHIK-X_preprocessing_run.R` would source the files. More information about particular files or processing steps can be found in the comments within the scripts. 



## Mobility data

1) `mobility_processing.R`

    - yields the wide format mobility matrix from the long-format raw data
    - requires df_burden_with_pop_size_2015_spillover i.e. burden with population size must be done before

2) `n_trips_day.R`

    - yields the daily number of trips between any two countries 
    - this is fed directly into model



## PAHO case data

1) `paho_data_long_to_wide.R`

    - yields df_paho_long_subset
    - outbreak reports downloaded from https://www3.paho.org/data/index.php/en/mnu-topics/chikv-en/550-chikv-weekly-en.html

1) `process_outbreaks.R`

    - yields ls and df with outbreak data to which the shapes will be fitted
    - works with a copy of df_paho_long_subset which contains a final column named `outbreak` and is composed of `s` and `e` characters denoting the start and end of a curve
        - note that curves cannot overlap
        - for the purposes of fitting, some curves were adjusted by adding leaading and trailing zeroes

1) `paho_outbreaks_vis.R`

    - convert monthly case data to daily 
    - smooth data using rolling averages 
    - visualise PAHO outbreak data 



## Suitability (geospatial) 

1) `download_gadm_shp.R`

    - query and download country-level (admin0) shape files 

1) `pop_weighted_mean.R` 

    - use geospatial suitability estimates in combination with UN adjusted worldpop data to estimate pixel-level population-weighted mean suitability
    - aggregate to national levels 

1) `p_spillover.R`

    - probability of CHIK-X emergence is encoded into the suitability file
    - it is proportional to the the annual estimated CHIK incidence based on the geospatial estimates of suitability and force of infection (FOI) estimates

1) `suit_plots.R`

    - diagnostic plots to understand the distribution of suitability values in relation to WHO region
    - plot the annual incidence as a percentage of the total population 

1) `interactive_suit_transform.ipynb`

    - Jupyter notebook, requires Jupyter and Python 
    - interactive plot shows the relationship between suitability and defined transformation parameters  
    - help in hyperparameter tuning for higher/lower CHIK-X transmission scenario 



## UN-adjusted worldpop data

1) `download_worldpop.R`

    - queries and downloads 1-km UN-adjusted worldpop data for a specified country and year
    - requires custom function in utils.R

2) `process_worldpop.R`

    - yields df_burden_with_pop_size_2015.csv (used by mobility_processing)



# Zika virus spread 

1) `zika_spread_rates.R`

    - read in and filter Zika spread rates between July 2014 and Dec 2016 

