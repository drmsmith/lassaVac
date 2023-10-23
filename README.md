# CHIK-X

CHIK-X transmission model for 100 days mission evaluation of vaccine impact.

The code in this repository is based on David Smith's code for Lassa-X transmission. David's code is associated with the following preprint: 

> "Projecting health and economic impacts of Lassa vaccination campaigns in West Africa "
This code was developed by OxLiv Consortium members David R M Smith, Joanne Turner, Patrick Fahr, Paul Bessell, Emily Nixon, Koen Pouwels and Deirdre Hollingsworth.

# CHIK-X drivers 

The following files contain the information used to calculate the risk of spread based on mobility data and estimated incidence. 

```
│
│
├───data
└───data_chik
    │   df_burden_with_pop_size_2015.csv            \ based on Henrik's output, contains 2015 pop size 
    │   df_countries_burden_estimates_cepi.csv      \ Henrik's estimates of CHIK burden 
    │   heatmap_2015.pdf                            \ visualise 2015 mobility data
    │   KCMD_EUI_GMP_Estimated_trips.csv            \ GMP mobility data, 2015
    │   mat_mobility_2015.csv                       \ pairwise mobility matrix, 2015 
    │   mobility_processing.R                       \ processing GMP data to get mat_mobility.csv 
    │   utils.R                                     \ helper to download UN-adj data 
    │   worldpop.R                                  \ download and process worldpop data for Henrik's countries
    └───worldpop_2015*      \ not huploaded but would contain 1km aggregated UNadj worldpop data 
```

