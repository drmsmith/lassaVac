library("tidyverse")
library("ggplot2")
library('sf')
library('rgdal')
library("rnaturalearth")
library("rnaturalearthdata")
library('conflicted')
conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    .quiet = T
)


source('model/utils_vis.R')

# CHIK-X simulation results and summaries 
# df_all_sims_long <- read.csv("res/import_model_sim_res_x100.csv")
df_results_summary <- read.csv('res/import_model_sim_summary_x100.csv')
df_full_summary <- read.csv('res/import_model_100_sim_full_summary.csv')

# helper files
df_burden = read.csv('data/df_suit_means_pop_wght_pop_size_who_regions.csv') 
# df_iso_regions = read.csv('methods/misc/ISO-3166-Countries-with-Regional-Codes.csv')
# df_who_regions = read.csv('methods/misc/df_countries_who_regions_codes.csv')




# add 0 for country not being in 
# and NA for no travel info 


## population size
v_pop_files <- list.files("data/2020_UNadj_worldpop_data", pattern = ".tif") %>%
    str_remove("_2020.tif") %>%
    toupper()
## gadm shape files
v_shape_files <- list.files("data/gadm", pattern = ".rds") %>%
    str_remove("gadm41_") %>%
    str_remove("_0_pk.rds") %>%
    toupper()
## mobility
mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
v_mobil_data <- colnames(mat_mob_daily_trips) 
v_burden_codes <- unique(df_burden$country_code)
length(v_shape_files)   # 233
length(v_pop_files)     # 220 
length(v_burden_codes)  # 194
length(v_mobil_data)    # 188

countrycode::countrycode(setdiff(v_burden_codes, v_mobil_data), 'iso3c', 'country.name')

countrycode::countrycode(v_mobil_data[!(v_mobil_data %in% v_burden_codes)], 'iso3c', 'country.name')
countrycode::countrycode(v_burden_codes[!(v_burden_codes %in% v_mobil_data)], 'iso3c', 'country.name')
v_mobil_data[v_mobil_data %in% setdiff(v_burden_codes, v_mobil_data)]
v_burden_codes[v_burden_codes %in% setdiff(v_burden_codes, v_mobil_data)]

ls_all_codes = list(v_shape_files, v_pop_files, v_burden_codes, v_mobil_data)
v_shape_pop_burden_mobil = Reduce(intersect, ls_all_codes)
length(v_shape_pop_burden_mobil)    # 184

# NAs to be added to the following shape files for lack of population or travel information 
v_nas = setdiff(v_shape_files, v_shape_pop_burden_mobil)
# only 8 of these are in df_who_regions
# countrycode::countrycode(v_nas, 'iso3c', 'country.name') 
# df_iso_regions %>% filter(str_detect(name, 'Taiwan'))

# which suitability countries don't have travel info 
# bind_rows(ls_burden_country_region) %>% filter(code %in% v_nas) %>% count(region_code) %>% as.vector




#### plotting ideas 
### average over simulations 
## cutoff at 2 years ??
# map: mean cumulative infections for map 
# map: mean per 100k pop across 100 sims 
# barplot: median + IQR cumulative incidence   
# map: population size, p_spillover
# map: % sim in which country is present  // spillover incidence 
# scatter: spillover ~ pop size (or suitability)


# spillover and pop size from burden 
# country in % sim 


# this gives how frequently a given country appears in the simulations 
# p spillover and pop_size 
percent_pop_size_spillover = df_full_summary %>%
    count(region_name, code) %>% # group_by(region_name)
    left_join(
        df_burden[,c('country_code', 'pop_size', 'p_spillover')], 
        by = join_by(code == country_code)
        ) %>% rename(percent_sims = n)


# take means, medians, and IQRs across different categories
## full duration or duration cut at 2yrs per outbreak 
## cumulative infections and infections per 100k population
df_sum_stats <- df_full_summary %>% 
    arrange(region_name, country) %>% # select(region_name, country, total_infections_all_years)
    mutate(
        country = factor(country, levels=unique(country)), 
        per_100k = 1e5*total_infections_all_years/pop_size, 
        per_100k_2yrs = 1e5*years_1_2/pop_size
        ) %>%
    group_by(country, code, region_name, region_code) %>%
    summarise(
        # pop_size = unique(pop_size),
        # total cumulative mean, median, IQR 
        mean_cumul_infections = mean(total_infections_all_years),
        median_cumul_infections = median(total_infections_all_years),
        q1_cumul_infections = quantile(total_infections_all_years, 0.25),
        q3_cumul_infections = quantile(total_infections_all_years, 0.75),
        # iqr_cumul_infections = IQR(total_infections_all_years),
        # cumulative mean, median, IQR for first 2 years 
        mean_cumul_infections_2yrs = mean(years_1_2),
        median_cumul_infections_2yrs = median(years_1_2),
        q1_cumul_infections_2yrs = quantile(years_1_2, 0.25),
        q3_cumul_infections_2yrs = quantile(years_1_2, 0.75),
        # total cumulative mean, median, IQR per 100k pop
        mean_cumul_infections_per100k = mean(per_100k),
        median_cumul_infections_per100k  = median(per_100k),
        q1_cumul_infections_per100k = quantile(per_100k, 0.25),
        q3_cumul_infections_per100k = quantile(per_100k, 0.75),
        # cumulative mean, median, IQR for first 2 years per 100k pop 
        mean_cumul_infections_2yrs_per100k = mean(per_100k_2yrs),
        median_cumul_infections_2yrs_per100k = median(per_100k_2yrs),
        q1_cumul_infections_2yrs_per100k = quantile(per_100k_2yrs, 0.25),
        q3_cumul_infections_2yrs_per100k = quantile(per_100k_2yrs, 0.75),
        .groups='keep'
    ) %>%      
    left_join(  # join with p_spillover, percent_sim, and ppp_size
        percent_pop_size_spillover[,c('code', 'pop_size', 'p_spillover', 'percent_sims')], by = join_by(code == code)
        ) %>%
    ungroup 


# add 0 for v_shape_pop_burden_mobil
# countries that are in simulations but do not have outbreaks 
##### this is particularly important for maps!!!
v_add_zeros = setdiff(v_shape_pop_burden_mobil, df_sum_stats$code)

df_zeros_NAs = df_burden %>% 
    filter(country_code %in% v_add_zeros) %>% 
    rename(country = country_name, code = country_code) %>% 
    select(country, code, region_name, region_code, pop_size, p_spillover) %>%
    mutate(percent_sims = 0)


# complete summary for map plotting 
df_sum_stats_zeros_NAs = bind_rows(df_sum_stats, df_zeros_NAs)
# optionally set 0 for NAs if needed
df_sum_stats_zeros_NAs[is.na(df_sum_stats_zeros_NAs)] = 0 
ls_sum_stats <- df_sum_stats_zeros_NAs %>% group_by(region_code) %>% group_split()


# how to format colour scale legends??
colnames(df_sum_stats_zeros_NAs)
apply(df_sum_stats_zeros_NAs, 2, max) %>% 
    as.numeric %>% 
    .[!(is.na(.))] %>%
    format(digits=3, scientific=T)


### add 0s for countries where NO OUTBREAK // for percent  
### and NA for countries with NO DATA  --> v_nas 
############ impossible due to lack of who region data 
# use na.value = 'gray' in scale_color/fill 





##################################
#### BAR PLOTS: MEDIAN +- IQR ####
##################################

cepi_prim_cols <- c(
    "#547dbf", "#ffa500", "#db4437", "#9d0f55", "#682860", "#0080A0", "#F9DF79"
)

all_metrics <- c(
    "median_cumul_infections",
    "median_cumul_infections_2yrs",
    "median_cumul_infections_per100k",
    "median_cumul_infections_2yrs_per100k"
    )


region_codes <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")


walk(all_metrics, function(.curr_metric) {
    walk(region_codes, function(.reg_code) {
        dest_dir = "figs/cepi_figs_ideas/barplots/"
        # dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
        if (!dir.exists(dest_dir)) dir.create(dest_dir)
        # run plotting funcs
        gg_metrics_barplot(
            .data = df_sum_stats, 
            .cols_for_scheme = cepi_prim_cols, 
            .region_code = .reg_code, 
            .metric = .curr_metric,
            .dest_dir = dest_dir
        )
    })
}, .progress = T)



df_by_region <- df_full_summary %>% group_by(region_code, region_name) %>% 
    mutate(
        region_code = factor(region_code, levels=unique(region_code)), 
        per_100k = 1e5*total_infections_all_years/pop_size, 
        per_100k_2yrs = 1e5*years_1_2/pop_size
        ) %>% 
        summarise(
            region_totals = median(total_infections_all_years),
            region_totals_2yrs = median(years_1_2),
            region_totals_per100k = median(per_100k),
            region_totals_per100k_2yrs = median(per_100k_2yrs),
            q1_region_totals = quantile(total_infections_all_years, 0.25),
            q3_region_totals = quantile(total_infections_all_years, 0.75),
            q1_region_totals_2yrs = quantile(years_1_2, 0.25),
            q3_region_totals_2yrs = quantile(years_1_2, 0.75),
            q1_region_totals_per100k = quantile(per_100k, 0.25),
            q3_region_totals_per100k = quantile(per_100k, 0.75),
            q1_region_totals_per100k_2yrs = quantile(per_100k_2yrs, 0.25),
            q3_region_totals_per100k_2yrs = quantile(per_100k_2yrs, 0.75),
            .groups='keep'
        ) 


########################
# all regions bar plot # 
########################

region_metrics <- c('region_totals', 'region_totals_2yrs','region_totals_per100k','region_totals_per100k_2yrs')
map(region_metrics, function(.curr_metric) {
    gg_metrics_barplot_region(.data=df_by_region, .metric = .curr_metric)
})

gg_metrics_barplot_region(.data=df_by_region, .metric = .curr_metric)





########################
#### MAP: SPILLOVER ####
########################


### average over simulations?? 
## cutoff at 2 years 
# map: mean cumulative infections for map 
# map: mean per 100k pop across 100 sims 
# map: population size, p_spillover
# map: % sim in which country is present  // spillover incidence 

library('countrycode')

# get world map data for plotting 
world <- ne_countries(scale = "large", returnclass = "sf") %>%
    filter(!str_detect(admin, 'arctica')) %>%
    mutate(country_code = countrycode(world$name, 'country.name', 'iso3c'))
# world <- world[, ]
world$country_code[world$name=='USA'] = 'USA'

wrld_joined_full <- left_join(
    world, df_sum_stats_zeros_NAs,
    by = join_by( == code) # adm0_a3  
) # %>% filter(region_code == 'AFR')



all_metrics <- c(
    "mean_cumul_infections",
    "median_cumul_infections",
    "mean_cumul_infections_2yrs",
    "median_cumul_infections_2yrs",
    "mean_cumul_infections_per100k",
    "median_cumul_infections_per100k",
    "mean_cumul_infections_2yrs_per100k",
    "median_cumul_infections_2yrs_per100k", 
    "pop_size", 
    "p_spillover", 
    "percent_sims"
)

region_codes <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")



walk(all_metrics, function(.curr_metric) {
    walk(region_codes, function(.reg_code) {
        dest_dir <- paste0("figs/cepi_figs_ideas/maps/", .reg_code, collapse = "")
        if (!dir.exists(dest_dir)) dir.create(dest_dir)
        # run plotting funcs
        gg_metrics_map(
            .wrld_joined_full = wrld_joined_full, 
            .region_code = .reg_code, 
            .metric = .curr_metric,
            .dest_dir = dest_dir
        )
    })
}, .progress = T)


# global maps

wrld_joined_full <- left_join(
    world, df_sum_stats_zeros_NAs,
    by = join_by(adm0_a3 == code)
) # %>% filter(region_code == 'AFR')


# colnames(df_sum_stats_zeros_NAs)
# colnames(world) 

# world$name 
# world$abbrev
# world$iso_a3 %>% sort
# world$adm0_a3 %>% sort
# world$brk_a3

# world$iso_a3 %in% world$adm0_a3

# name_to_code = countrycode::countrycode(world$name, 'country.name', 'iso3c')
# name_to_code = name_to_code[!is.na(name_to_code)]

# v_burden_codes[!v_burden_codes %in% name_to_code]
# countrycode::countrycode(v_burden_codes[!v_burden_codes %in% name_to_code], 'iso3c', 'country.name')



dest_dir <- "figs/cepi_figs_ideas/global_maps"
# dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
if (!dir.exists(dest_dir)) dir.create(dest_dir)

walk(all_metrics, function(.curr_metric) {
    # run plotting funcs
    gg_metrics_map_global(
        .wrld_joined_full = wrld_joined_full,
        .metric = .curr_metric,
        .dest_dir = dest_dir
    )
}, .progress = T)


