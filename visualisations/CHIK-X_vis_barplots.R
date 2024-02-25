library("tidyverse")
library("ggplot2")
conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    .quiet = T
)


source('utils/utils_vis.R')

main_dir <- 'res/baseline_inc_0.7_noise_0.1'
incidence_factor <- 0.7

df_all_sims_long <- readRDS(file.path(main_dir, "sim_res_long.RDS"))
df_results_summary <- readRDS(file.path(main_dir, 'sim_summary.RDS'))
df_full_summary <- readRDS(file.path(main_dir, 'sim_full_summary.RDS'))

# helper files
df_burden = read.csv('data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv') 
df_burden$annual_incidence <- df_burden$annual_incidence*incidence_factor




######################
# file preprocessing # 
######################

# this gives how frequently a given country appears in the simulations 
# p spillover and pop_size 
percent_pop_size_spillover = df_full_summary %>% 
    group_by(country, code, region_name, region_code) %>%
    count(region_name, code) %>% # group_by(region_name)
    left_join(
        df_burden[,c('code', 'pop_size', 'p_spillover')], 
        by = join_by(code == code)
        ) %>% rename(percent_sims = n)


# take means, medians, and IQRs across different categories
## full duration or duration cut at 2yrs per outbreak 
## cumulative infections and infections per 100k population
df_sum_stats <- df_full_summary %>% 
    arrange(region_name, country) %>% # select(region_name, country, total_infections_all_years)
    mutate(
        country = factor(country, levels=unique(country)), 
        per_100k = 1e5*total_infections_all_years/pop_size
        ) %>%
    group_by(country, code, region_name, region_code) %>%
    summarise(
        percentage_appearance = n(),
        mean_cumul_infections = mean(total_infections_all_years),
        median_cumul_infections = median(total_infections_all_years),
        q1_cumul_infections = quantile(total_infections_all_years, 0.25),
        q3_cumul_infections = quantile(total_infections_all_years, 0.75),
        mean_cumul_infections_per100k = mean(per_100k),
        median_cumul_infections_per100k  = median(per_100k),
        q1_cumul_infections_per100k = quantile(per_100k, 0.25),
        q3_cumul_infections_per100k = quantile(per_100k, 0.75),
        .groups='keep'
    ) %>%      
    left_join(  # join with p_spillover, percent_sim, and ppp_size
        percent_pop_size_spillover[,c('code', 'pop_size', 'p_spillover', 'percent_sims')], by = join_by(code == code)
        ) %>% ungroup 

# add 0 for v_shape_pop_burden_mobil
# countries that are in simulations but do not have outbreaks 
##### this is particularly important for maps!!!
v_add_zeros = setdiff(df_burden$code, df_sum_stats$code)

df_zeros_NAs = df_burden %>% 
    filter(code %in% v_add_zeros) %>% 
    # rename(country = country_name, code = country_code) %>% 
    select(country, code, region_name, region_code, pop_size, p_spillover) %>%
    mutate(percent_sims = 0)


# complete summary for map plotting 
df_sum_stats_zeros_NAs = bind_rows(df_sum_stats, df_zeros_NAs)
# optionally set 0 for NAs if needed
df_sum_stats_zeros_NAs[is.na(df_sum_stats_zeros_NAs)] = 0 
ls_sum_stats <- df_sum_stats_zeros_NAs %>% group_by(region_code) %>% group_split()




##################################
#### BAR PLOTS: MEDIAN +- IQR ####
##################################

cepi_prim_cols <- c(
    "#547dbf", "#ffa500", "#db4437", "#9d0f55", 
    "#682860", "#0080A0", "#F9DF79"
)

# colnames to loop over 
all_metrics <- c(
    "median_cumul_infections",
    "median_cumul_infections_per100k"
    )

# WHO regions 
region_codes <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
# name colours for plotting purposes 
names(cepi_prim_cols) <- region_codes

# destinations and respective file formats 
barplotdirs <- c("figs/barplots_sorted_freq/svg", "figs/barplots_sorted_freq")
ext_codes <- c('.svg', '.png')

# loop over destination and associated file format 
walk2(ext_codes, barplotdirs, function(.ext_code, .dest){
    # loop over columns to create bar plots 
    walk(all_metrics, function(.curr_metric) {
        walk(region_codes, function(.reg_code) {
            dest_dir = barplotdir
            # dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
            if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
            # run plotting funcs
            fill_col <- cepi_prim_cols[.reg_code]
            gg_metrics_barplot_col_sort(
                .data = df_sum_stats, 
                .cols_for_scheme = cepi_prim_cols, 
                .region_code = .reg_code, 
                .fill_col = fill_col, 
                .metric = .curr_metric,
                .format_extension = .ext_code,
                .dest_dir = .dest
            )
        })
    })
}, .progress = TRUE)




#########################
# BAR PLOT: all regions # 
#########################

# columns to loop over 
region_metrics <- c(
    'region_totals', 'region_totals_per100k'
    )


#########################
# excluding coutnries   # 
# that have 0 outbreaks # 
#########################

# create summaries for plotting 
df_by_region <- df_full_summary %>% 
    group_by(region_code, region_name) %>% 
    mutate(
        region_code = factor(region_code, levels=unique(region_code)), 
        per_100k = 1e5*total_infections_all_years/pop_size
        ) %>% 
        summarise(
            median_per100k = 1e5*median(total_infections_all_years) / sum(pop_size),
            q1_per100k = 1e5*quantile(total_infections_all_years, 0.25) / sum(pop_size),
            q3_per100k = 1e5*quantile(total_infections_all_years, 0.75) / sum(pop_size),
            region_totals = median(total_infections_all_years),
            region_totals_per100k = median(per_100k),
            q1_region_totals = quantile(total_infections_all_years, 0.25),
            q3_region_totals = quantile(total_infections_all_years, 0.75),
            q1_region_totals_per100k = quantile(per_100k, 0.25),
            q3_region_totals_per100k = quantile(per_100k, 0.75),
            .groups='keep'
        ) 


dest_dir = "figs/barplots_region_excl_0_oubtr_pops"
if (!dir.exists(dest_dir)) dir.create(dest_dir)

formats_to_save <- c('.png', '.svg')

# save different formats of all the metrics 
# into the same destination directory 
# since the metrics are fewer
walk(formats_to_save, function(.ext_code){
    walk(region_metrics, function(.curr_metric) {
        gg_metrics_barplot_region(
            .data=df_by_region, .metric = .curr_metric, 
            .dest_dir = dest_dir, .format_extension = .ext_code
            )
    })
}, .progress = TRUE)




#########################
# including coutnries   # 
# that have 0 outbreaks # 
#########################

# this only makes a very subtle difference because 
# most countries are already present across the 100 sims
# see table below for mostly EUR countries that never make it  

# subset countries with 0 outbreaks  
df_zero_infections <- df_burden %>% 
    filter(code %in% v_add_zeros) %>% 
    # rename(country = country_name, code = country_code) %>% 
    select(country, code, region_name, region_code, pop_size) %>%
    mutate(total_infections_all_years = 0)

# merge with df summary  
df_full_summary_with_missing_countries <- df_full_summary %>% 
    full_join(df_zero_infections, by=join_by(
    code==code, country==country, region_name==region_name, 
    region_code==region_code, pop_size==pop_size,
    total_infections_all_years==total_infections_all_years
    ))

# print countries that never experience outbreaks 
# or their counts per region  
df_full_summary_with_missing_countries %>% ungroup %>%
    filter(total_infections_all_years==0 & !complete.cases(simulation)) %>%
    select(simulation, code, region_code, pop_size, total_infections_all_years) %>% 
    count(region_code) %>% arrange(desc(n), region_code)

# create summaries for plotting 
df_by_region <- df_full_summary_with_missing_countries %>% 
    group_by(region_code, region_name) %>% 
    mutate(
        region_code = factor(region_code, levels=unique(region_code)), 
        per_100k = 1e5*total_infections_all_years/pop_size
        # per_100k_2yrs = 1e5*years_1_2/pop_size
        ) %>% 
        summarise(
            median_per100k = 1e5*median(total_infections_all_years) / sum(pop_size),
            q1_per100k = 1e5*quantile(total_infections_all_years, 0.25) / sum(pop_size),
            q3_per100k = 1e5*quantile(total_infections_all_years, 0.75) / sum(pop_size),
            region_totals = median(total_infections_all_years),
            region_totals_per100k = median(per_100k),
            q1_region_totals = quantile(total_infections_all_years, 0.25),
            q3_region_totals = quantile(total_infections_all_years, 0.75),
            q1_region_totals_per100k = quantile(per_100k, 0.25),
            q3_region_totals_per100k = quantile(per_100k, 0.75),
            .groups='keep'
        ) 


dest_dir = "figs/barplots_region_incl_0_oubtr_pops"
if (!dir.exists(dest_dir)) dir.create(dest_dir)

formats_to_save <- c('.png', '.svg')

# save different formats of all the metrics 
# into the same destination directory 
# since the metrics are fewer
walk(formats_to_save, function(.ext_code){
    walk(region_metrics, function(.curr_metric) {
        gg_metrics_barplot_region(
            .data=df_by_region, .metric = .curr_metric, 
            .dest_dir = dest_dir, .format_extension = .ext_code
            )
    })
}, .progress = TRUE)




