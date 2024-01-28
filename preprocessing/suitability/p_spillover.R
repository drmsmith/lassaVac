library('tidyverse')

# read in suitability and who region data
df_burden <- read.csv('data/df_suit_means_pop_weighted_pop_size.csv')
# who regions are fewer than the suitability countries
df_countries_who_regions_codes <- read.csv('methods/df_countries_who_regions_codes.csv')

### Probability of spillover in each catchment area
total_suit <- sum(df_burden$mean_pop_weighted)
### Add column for the proportion of all spillovers occurring in each catchment
df_burden$p_spillover <- df_burden$mean_pop_weighted / total_suit

### add who region
df_burden = right_join(df_burden, df_countries_who_regions_codes, by='country_code') %>% 
    select(
        country_name, country_code, region_name, region_code, mean_suit, mean_pop_weighted, pop_size, p_spillover 
    ) 

# write.csv(df_burden, 'data/df_suit_means_pop_wght_pop_size_who_regions.csv', row.names=F)
