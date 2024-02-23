library("tidyverse")
library("ggplot2")
library("ggthemes")
library("cowplot")
library("sf")
library("rgdal")
library("rnaturalearth")
library("rnaturalearthdata")
library("countrycode")


source('visualisations/utils_post_proc.R')
source('visualisations/utils_vis.R')

main_dir <- 'res/baseline_inc_0.7_noise_0.1'
# CHIK-X simulation results and summaries 
# df_all_sims_long <- readRDS("res/baseline_inc_17_noise_0.1/sim_res_long.RDS")
df_results_summary <- readRDS(file.path(main_dir, 'sim_summary.RDS'))
df_full_summary <- readRDS('res/baseline_inc_17_noise_0.1/sim_full_summary.RDS')

df_burden = read.csv('data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv') 
mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
all_codes <- colnames(mat_mob_daily_trips)
# ensure mobility data and suitability data are matched 
df_burden <- filter(df_burden, code %in% all_codes) %>% drop_na()
burden_codes <- df_burden$code

# identify most commonly appearing starting countries 
top_starting_countries <- df_results_summary %>% group_by(code, region_code, simulation) %>%
    filter(timing == 0)  %>% group_by(code,region_code) %>%
    count(code) %>% 
    arrange(desc(n)) %>% head(n=4)
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


world <- get_worldmap() # takes a minute and throws warnings 

wrld_joined_nas <- left_join(
    world, df_burden,
    by = join_by(adm0_a3 == code)
) %>% filter(!complete.cases(pop_size))


cepcols  <- read.csv('methods/misc/cepi_color_scheme.csv')
cepcols[2,c(1,3,4,2,5,6)]

source('visualisations/utils_post_proc.R')
source('visualisations/utils_vis.R')

dest_dir = "figs/start_dest"

all_start_dest_maps <- map2(
    top_starting_countries$code, ls_dests, 
    function(.start_code, .df) {
        # plot file name 
        file_name <- paste0("global_start_dest_", .start_code, ".png", collapse = "")

        # countries that are in simulations but do not have outbreaks 
        ##### this is particularly important for maps!!!
        v_add_zeros = setdiff(burden_codes, .df$code)
        df_zeros_NAs = df_burden %>% 
            filter(code %in% v_add_zeros) %>% 
            # rename(country = country_name, ode = country_code) %>% 
            select(country, code, region_name, region_code, pop_size, p_spillover) %>%
            mutate(perc_spread = 0)

        # complete summary for map plotting 
        df_sum_stats_zeros_NAs = bind_rows(.df, df_zeros_NAs) # %>% ungroup %>%
        # map by region
        # ls_sum_stats <- df_sum_stats_zeros_NAs %>% group_by(region_code) %>% group_split()

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
        
        plt <- gg_dest_map_global(
            wrld_joined_full, wrld_start, start_point,
            wrld_joined_nas,
            'perc_spread', .save=T, .dest_dir=dest_dir, .file_name=file_name
            )
        return(plt)
}, .progress = T)




# df_results_summary %>% group_by(country, code, region_code, simulation) %>%
#     filter((timing != 0) & (simulation %in% ind_start_sims)) %>% 
#         group_by(country, code,region_code) %>%
#         count(code) %>% 
#         arrange(desc(n)) %>% 
#         mutate(perc_spread = 100*n / length(ind_start_sims)) %>%
#         print()


#     as.vector(ind_start_sims[,1])
#     # group_by(country, code,region_code) %>%
#     # count(code) %>% 
#     # arrange(desc(n)) %>% 
#     # print(n=168)


# identify most commonly appearing destination countries 
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
# identify most commonly appearing destination regions 
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
    # count(code) %>% 
    mutate(n=n()) %>% ungroup %>%
    mutate(proportion = 100*prop.table(IncCumul_U_final)) %>%
    select(code, region_code, n, proportion) %>%
    distinct(code, .keep_all=TRUE) %>% 
    arrange(desc(n)) # %>% head(n=5)

df_results_summary %>% 
    filter(code == 'IND') %>% summarise(sum(IncCumul_U_final))

df_results_summary %>% group_by(simulation) %>% 
    # transmute(perc = (100*IncCumul_U_final / sum(IncCumul_U_final))) %>%
    add_count(country) %>%
    mutate(cumul_nspread = cumsum(n)) %>%
    mutate(perc = (100*IncCumul_U_final / sum(IncCumul_U_final))) %>%
    select(code, region_code, perc, cumul_nspread) %>% filter(perc==last(perc)) %>%
    arrange(desc(cumul_nspread))# arrange(desc(perc))
    # # filter(IncCumul_U_final == max(IncCumul_U_final)) %>%
    # select(code, region_code, perc) %>%
    # arrange(desc(perc))





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



# save 
ggsave(scenario_rate_plot,
    filename=file.path('figs',pltname), dpi=330, 
    width=3300, height=1400, units='px')
