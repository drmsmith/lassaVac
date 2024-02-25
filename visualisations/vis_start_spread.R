library("tidyverse")
library("ggplot2")
library("ggthemes")
library("cowplot")
library("sf")
library("rgdal")
library("rnaturalearth")
library("rnaturalearthdata")
library("countrycode")


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
mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
all_codes <- colnames(mat_mob_daily_trips)

# ensure mobility data and suitability data are matched 
df_burden <- filter(df_burden, code %in% all_codes) %>% drop_na()
burden_codes <- df_burden$code

# get world map for plotting 
world <- get_worldmap() # takes a minute and throws warnings 



# identify most commonly appearing starting countries 
n_countries <- 4

top_starting_countries <- df_results_summary %>% group_by(code, region_code, simulation) %>%
    filter(timing == 0)  %>% group_by(code,region_code) %>%
    count(code) %>% 
    arrange(desc(n)) %>% head(n=n_countries)
top_starting_countries

# identify most commonly appearing destination countries 
## FROM STARTING COUNTRIES 
ls_dests <- map(top_starting_countries$code, function(.ccode) {
    ind_start_sims  <- df_results_summary %>% 
        group_by(country, code, region_code, simulation) %>%
        filter(timing == 0 & code == .ccode) %>% ungroup %>%
        reframe(sims = unique(simulation)) %>% unlist %>% unname
        
    df_dests <- df_results_summary %>% 
        group_by(country, code, region_code, simulation) %>%
        filter((simulation %in% ind_start_sims)) %>% # (timing != 0) &
        mutate(per100k = 1e5 * IncCumul_U_final / pop_size) %>% 
        group_by(country,code,region_code) %>%
        summarise(
            n = n(), 
            perc_spread = 100*n / length(ind_start_sims),
            mean_outbreak_size = mean(IncCumul_U_final), 
            median_outbreak_size = median(IncCumul_U_final), 
            q1_outbreak_size = quantile(IncCumul_U_final, 0.25),
            q3_outbreak_size = quantile(IncCumul_U_final, 0.75),
            median_outbreak_per100k = median(per100k), 
            q1_outbreak_per100k = quantile(per100k, 0.25),
            q3_outbreak_per100k = quantile(per100k, 0.75) 
        ) %>%
        arrange(desc(n))
    return(df_dests)

})

# df_dests <- ls_dests %>% setNames(top_starting_countries$code) %>% bind_rows(.id='source_country')


# add NAs for plotting 
wrld_joined_nas <- left_join(
    world, df_burden,
    by = join_by(adm0_a3 == code)
) %>% filter(!complete.cases(pop_size))


#####################
# PLOT START - DEST # 
#####################

dest_dirs <- c("figs/start_dest", "figs/start_dest/svg")
file_exts <- c(".png", ".svg")

pngs_and_svg_maps <- map2(file_exts, dest_dirs, function(.file_ext, .savedir) {
    if (!dir.exists(.savedir)) dir.create(.savedir, recursive = TRUE)
    all_start_dest_maps <- map2(
        top_starting_countries$code, ls_dests, 
        function(.start_code, .df) {
            # plot file name 
            file_name <- paste0("global_start_dest_", .start_code, .file_ext, collapse = "")

            # countries that are in simulations but do not have outbreaks 
            ##### this is particularly important for maps!!!
            v_add_zeros = setdiff(burden_codes, .df$code)
            df_zeros_NAs = df_burden %>% 
                filter(code %in% v_add_zeros) %>% 
                select(country, code, region_name, region_code, pop_size, p_spillover) %>%
                mutate(perc_spread = 0)

            # complete summary for map plotting 
            df_sum_stats_zeros_NAs = bind_rows(.df, df_zeros_NAs)

            # join this with map data for plotting 
            wrld_joined_full <- left_join(
                world, df_sum_stats_zeros_NAs,
                by = join_by(code == code) # adm0_a3  
            )
            # get separate starting country data
            wrld_start <- left_join(
                world, data.frame(code=.start_code, perc_spread=1),
                by = join_by(code == code)
                ) %>% filter(code == .start_code)
            # and centroid for mark 
            start_point <- st_centroid(wrld_start$geometry)
            # plot and save 
            plt <- gg_dest_map_global(
                wrld_joined_full, wrld_start, start_point, wrld_joined_nas,
                'perc_spread', .save=T, .dest_dir=.savedir, .file_name=file_name
                )
            return(plt)
    }, .progress = T)
    return(all_start_dest_maps)
}) # %>% setNames(str_replace_all(file_exts, '\\.', ''))








###########################
# vague destination stats #
###########################

# identify most commonly appearing destination countries 
# excluding starting country and independent of it 
df_results_summary %>% group_by(country, code, region_code, simulation) %>%
    filter(timing != 0)  %>% group_by(country, code,region_code) %>%
    count(code) %>% 
    arrange(desc(n)) %>% 
    print(n=168)

# identify most commonly appearing destination regions 
df_results_summary %>%
    filter(timing != 0)  %>% group_by(region_code) %>%
    count(region_code) %>% 
    arrange(desc(n))

# num countries in region 
df_results_summary %>%
    select(code, region_code) %>% 
    distinct() %>%
    group_by(region_code) %>%
    count(region_code) %>% 
    arrange(desc(n))


# identify most commonly appearing destination regions 
#### use df burden for region country counts 
df_results_summary %>%
    filter(timing != 0)  %>% group_by(region_code) %>%
    select(code, region_code) %>%
    distinct() %>%
    count(region_code) %>% rename(freq = n) %>%
    arrange(desc(freq)) %>% 
    left_join(
        df_burden %>%
            select(code, region_code) %>% 
            distinct() %>%
            group_by(region_code) %>%
            count(region_code) %>%
            arrange(desc(n)), 
        by = join_by(region_code==region_code)) %>%
    mutate(prop = freq/n) %>% arrange(desc(prop)) 



colnames(df_results_summary)
df_results_summary %>% group_by(code, region_code, simulation) %>%
    filter(timing == 0) %>% group_by(code) %>%
    mutate(n=n()) %>% ungroup %>%
    mutate(proportion = 100*prop.table(IncCumul_U_final)) %>%
    select(code, region_code, n, proportion) %>%
    distinct(code, .keep_all=TRUE) %>% 
    arrange(desc(n)) # %>% head(n=5)


df_results_summary %>% group_by(simulation) %>% 
    add_count(country) %>%
    mutate(cumul_nspread = cumsum(n)) %>%
    mutate(perc = (100*IncCumul_U_final / sum(IncCumul_U_final))) %>%
    select(code, region_code, perc, cumul_nspread) %>% filter(perc==last(perc)) %>%
    arrange(desc(cumul_nspread))




# percentage total incidence across sims  
# include source countries 
df_results_summary %>% 
    filter(timing == 0) %>% 
    mutate(perc = (100*IncCumul_U_final / sum(IncCumul_U_final))) %>%
    group_by(code, region_code) %>% summarise(perc = sum(perc)) %>%
    arrange(desc(perc))

# percentage total incidence across sims  
# exclude source countries 
df_results_summary %>% 
    filter(timing != 0) %>% 
    mutate(perc = (100*IncCumul_U_final / sum(IncCumul_U_final))) %>%
    group_by(code, region_code) %>% summarise(perc = sum(perc)) %>%
    arrange(desc(perc))

