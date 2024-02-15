library("tidyverse")
library("ggplot2")
library('cowplot')


source('visualisations/utils_post_proc.R')


# CHIK-X simulation results and summaries 
# df_all_sims_long <- read.csv("res/baseline/sim_res_long.csv")
df_results_summary <- read.csv('res/EDA_scenarios/baseline_x9/sim_summary.csv')
df_full_summary <- read.csv('res/EDA_scenarios/baseline_x9/sim_full_summary.csv')

df_burden = read.csv('data/2019ppp_pd_df_suit_means_who_p_spillover.csv') 
mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
all_codes <- colnames(mat_mob_daily_trips)
# ensure mobility data and suitability data are matched 
df_burden <- filter(df_burden, code %in% all_codes) %>% drop_na()

# identify most commonly appearing starting countries 
top_starting_countries <- df_results_summary %>% group_by(code, region_code, simulation) %>%
    filter(timing == 0)  %>% group_by(code,region_code) %>%
    count(code) %>% 
    arrange(desc(n)) %>% head(n=4)

# identify most commonly appearing destination countries 
## FROM STARTING COUNTRIES 
df_dests <- map(top_starting_countries$code, function(.ccode) {
    ind_start_sims  <- df_results_summary %>% 
        group_by(country, code, region_code, simulation) %>%
        filter(timing == 0 & code == .ccode) %>% ungroup %>%
        reframe(sims = unique(simulation)) %>% unlist %>% unname
        
    df_dests <- df_results_summary %>% 
        group_by(country, code, region_code, simulation) %>%
        filter((timing != 0) & (simulation %in% ind_start_sims)) %>% 
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

}) %>% setNames(top_starting_countries$code) %>% bind_rows(.id='starting_country')

head(df_dests)


df_results_summary %>% group_by(country, code, region_code, simulation) %>%
    filter((timing != 0) & (simulation %in% ind_start_sims)) %>% 
        group_by(country, code,region_code) %>%
        count(code) %>% 
        arrange(desc(n)) %>% 
        mutate(perc_spread = 100*n / length(ind_start_sims)) %>%
        print()


    as.vector(ind_start_sims[,1])
    # group_by(country, code,region_code) %>%
    # count(code) %>% 
    # arrange(desc(n)) %>% 
    # print(n=168)


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
