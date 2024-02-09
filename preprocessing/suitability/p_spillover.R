library('tidyverse')

# read in suitability and who region data
# 'data/df_suit_means_pop_weighted_pop_size.csv' ## 2020 data 
df_suit_means <- read.csv('data/2019ppp_df_suit_means_pop_weighted_pop_size.csv')
df_burden <- read.csv('data/sum_chik_incidence_by_country.csv') %>% 
    dplyr::select(country, incidence) %>% rename(annual_incidence = incidence) 
# who regions are fewer than the suitability countries
df_countries_who_regions_codes <- read.csv('methods/misc/df_countries_who_regions_codes.csv')

colnames(df_countries_who_regions_codes)

df_suit_means_who = left_join(df_suit_means, df_countries_who_regions_codes, by='country_code') %>%
    dplyr::select(
        country_name, country_code, region_name, region_code, 
        pop_size, mean_suit, mean_pop_weighted
        )


df_burden$code = countrycode::countrycode(df_burden$country, 'country.name', 'iso3c')
# Some values were not matched unambiguously: Abyei, Aksai Chin, Arunachal Pradesh, 
# Azores Islands, Glorioso Islands, Guantanamo, Hala'ib Triangle, Ilemi Triangle, 
# Jammu-Kashmir, Jarvis Island, Kuril Islands, Ma'tan al-Sarra, Madeira Islands, 
# Midway Is., Netherlands Antilles, Paracel Islands, Spratly Islands
df_burden = drop_na(df_burden)


### Probability of spillover in each catchment area
total_incidence <- sum(df_burden$annual_incidence)
### Add column for the proportion of all spillovers occurring in each catchment
df_burden$p_spillover <- df_burden$annual_incidence / total_incidence


### add who region
df_burden_p_spillover = right_join(df_burden, df_suit_means_who, by=join_by(code==country_code)) %>% 
    dplyr::select(
        country, code, region_name, region_code, 
        pop_size, mean_suit, mean_pop_weighted, 
        annual_incidence, p_spillover 
    ) %>% drop_na 


# add pop density weighted mean suit 
df_pop_weighted_mean_suit = read.csv('data/df_suit_pd_weighted.csv')
df_pop_weighted_mean_suit = df_pop_weighted_mean_suit %>% rename(
    mean_suit_pd=mean_suit, mean_pd_weighted=suit_pd_weightd
    )


df_burden_combined <- left_join(
    rename(df_burden_p_spillover, mean_suit_ppp=mean_suit),
    dplyr::select(df_pop_weighted_mean_suit, code, mean_suit_pd, mean_pd_weighted),
    by = join_by(code==code)
) %>% select(
    country, code, region_name, region_code, pop_size, 
    annual_incidence, p_spillover, mean_suit_ppp, 
    mean_pop_weighted, mean_suit_pd, mean_pd_weighted
    )




# colnames(df_pop_weighted_mean_suit) %>% cat(sep=', ')
# dim(df_burden_p_spillover)

# write.csv(df_burden_p_spillover, 'data/df_suit_means_pop_wght_pop_size_who_p_spillover.csv', row.names=F)
# write.csv(df_burden_p_spillover, 'data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv', row.names=F)

# write.csv(df_burden_combined, 'data/2019ppp_pd_df_suit_means_who_p_spillover.csv', row.names=F)
