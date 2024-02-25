library("tidyverse")
library("ggplot2")
library("sf")
library("rgdal")
library("rnaturalearth")
library("rnaturalearthdata")
library("conflicted")
library("countrycode")
library("ggtextures")
library("ggpattern")
library("grid")
library("ggh4x")
library("magick", quietly = T, warn.conflicts = F)

conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    .quiet = T
)

source('visualisations/utils_vis.R')
source("visualisations/utils_ks_test.R")
source("visualisations/utils_post_proc.R")

main_dir <- 'res/baseline_inc_0.7_noise_0.1'
incidence_factor <- 0.7
dest_dir <- file.path('figs','baseline_zika_and_hists')

df_all_sims_long <- readRDS(file.path(main_dir, "sim_res_long.RDS"))
df_results_summary <- readRDS(file.path(main_dir, 'sim_summary.RDS'))
df_full_summary <- readRDS(file.path(main_dir, 'sim_full_summary.RDS'))

# helper files
df_burden = read.csv('data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv') 
df_burden$annual_incidence <- df_burden$annual_incidence*incidence_factor

# get world map for plotting 
world <- get_worldmap() # takes a minute and throws warnings 




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





##############
#### MAPS ####
##############

# map: mean/median cumulative infections for map 
# map: mean/median per 100k pop across 100 sims 
# map: population size, p_spillover
# map: % sim in which country is present  
# map: probability of spillover/emergence 

# join world map with summary stats 
wrld_joined_full <- left_join(
    world, df_sum_stats_zeros_NAs,
    by = join_by(adm0_a3 == code) # code 
) 

# all colnames to be plotted 
all_metrics <- c(
    "mean_cumul_infections",
    "median_cumul_infections",
    "mean_cumul_infections_per100k",
    "median_cumul_infections_per100k",
    "pop_size", 
    "p_spillover", 
    "percent_sims"
)



######################
# maps by WHO region # 
######################

# who region codes 
region_codes <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")
# where to save plots 
plts_dest_dirs <- c("figs/maps_region", "figs/maps_region/svg")
# and their respective file extensiosn 
file_exts <- c(".png", ".svg")

# loop over extension and directory 
walk2(file_exts[2], plts_dest_dirs[2], function(.fext, .plts_dest_dir){
    # loop over all colnames to be plotted 
    walk(all_metrics, function(.curr_metric) {
        # loop over all WHO regions 
        walk(region_codes, function(.reg_code) {
            dest_dir <- file.path(.plts_dest_dir, .reg_code)
            if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
            # run plotting funcs
            gg_metrics_map(
                .wrld_joined_full = wrld_joined_full, 
                .region_code = .reg_code, 
                .metric = .curr_metric,
                .file_extension = .fext,
                .dest_dir = dest_dir
            )
        })
    }, .progress = T)
})





###############
# global maps #
###############

# where to save plots 
plts_dest_dirs <- c("figs/maps_global", "figs/maps_global/svg")
# and their respective file extensiosn 
file_exts <- c(".png", ".svg")

# loop over extension and directory 
walk2(file_exts[2], plts_dest_dirs[2], function(.fext, .plts_dest_dir){
    if (!dir.exists(.plts_dest_dir)) {
        dir.create(.plts_dest_dir, recursive = TRUE) 
        }
    # loop over all column names to be plotted 
    walk(all_metrics, function(.curr_metric) {
        # run plotting funcs
        gg_metrics_map_global(
            .wrld_joined_full = wrld_joined_full,
            .metric = .curr_metric,
            .file_extension = .fext,
            .dest_dir = .plts_dest_dir
        )
    }, .progress = T)
})





######################
# make regional GIFs #
######################

gif_maker = function(
    vec_img_paths,  # vector of full paths to desired images in correct order
    file_name = 'plot_gif', # how to name the gif
    dest_dir = getwd(),     # directory to save gif in 
    .fps=2) {               # frames per second (multiple of 100)
    # read all images into a list  
    img_list <- lapply(vec_img_paths, image_read)
    # join images 
    img_joined <- image_join(img_list)
    # animate at 2 frames per second by default
    img_animated <- image_animate(img_joined, fps = .fps)
    ## save to disk
    img_path = paste0(dest_dir, '/', file_name, '.gif')
    image_write(image = img_animated, path = img_path)
    gc()
    return(img_animated)
}


walk(region_codes, function(whoregion) {
    dest_dir = file.path('figs/CEPI_pres/maps_region/gifs', whoregion) 
    all_filepaths = list.files(dest_dir, full.names = T)
    gif_maker(all_filepaths, whoregion, dest_dir, .fps = 0.5)
}, .progress = T)
