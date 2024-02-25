library("tidyverse")
library("ggplot2")
library('cowplot')


source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')



###############################################
##### INCREASED SUITABILITY -- PROPORTION #####
###############################################


## summarise and plot scenario
main_dir <- "res/scenarios_inc_dec" # "res/scenarios_x5" trial scenarios 
scenario_id <- 'increased_suit_transm_'
pltname = 'incr_suit_transm.png'
plt_ncols = 4
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,


resdirs <- c(
    file.path(main_dir, dir(main_dir, pattern = 'baseline')),
    file.path(main_dir, dir(main_dir, pattern = scenario_id)) %>%
        str_sort(numeric=T, decreasing = T)
)

ids = c(
    'baseline', 
    dir(main_dir, pattern = scenario_id) %>%
        str_sort(numeric=T, decreasing = T) %>% str_replace_all(scenario_id, 'incr. suit. prop. ')
)


figpath <- file.path(main_dir, dir(main_dir, pattern = 'fig'))
if (!dir.exists(figpath)) dir.create(figpath)


####################################
# join all dfs for zika rate plots #
####################################
### note these are not saved 

df_all_scenarios_full_summary <- map2(resdirs, ids, function(.res_dir, .id) {
    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save=FALSE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save=FALSE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(df_all_sims_sum, df_summary_by_year, save=FALSE, res_dir=.res_dir)

    df_full_summary$scenario_id = .id
    return(df_full_summary)
}, .progress = TRUE) %>% 
    bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))


##################
# zika rate tune #
##################

colnames(df_all_scenarios_full_summary)

source('visualisations/utils_post_proc.R')

scenario_rate_plot <- make_scenario_rate_plot(df_all_scenarios_full_summary, ncols=plt_ncols) 
scenario_rate_plot

# save 
ggsave(scenario_rate_plot,
    filename=file.path('figs',pltname), dpi=330, 
    width=3300, height=1400, units='px')





###############################
# distribution of n countries #
# affected by day 100 and 160 # 
###############################


# calculate cumulative spread over time 
spread_cumul_timing_100 <- df_all_scenarios_full_summary %>%
    filter(outbreak_start_day <= 100) %>%
    group_by(scenario_id, simulation) %>%
    count(simulation) %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )


incid_scen_n_by_100 <- ggplot(
    spread_cumul_timing_100,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~scenario_id, ncol=plt_ncols) + 
    geom_histogram(binwidth=1, fill="#e89600") +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 100", 
        y = "Count"
    ) +
    theme_light(base_size = 12) +
    xlim(-1,15)



spread_cumul_timing_160 <- df_all_scenarios_full_summary %>%
    filter(outbreak_start_day <= 160) %>%
    group_by(scenario_id, simulation) %>%
    count(simulation) %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )

incid_scen_n_by_160 <- ggplot(
    spread_cumul_timing_160,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~scenario_id, ncol=plt_ncols) + 
    geom_histogram(binwidth=1, fill="#007492") +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 160", 
        y = "Count"
    ) +
    theme_light(base_size = 12) + xlim(-1,15)




plts = plot_grid(incid_scen_n_by_100,incid_scen_n_by_160, labels = "AUTO", ncol=1) 
plts
ggsave(plts,
    filename=file.path('figs','hists_100_160_incr_suit_prop.png'), dpi=330, 
    width=3500, height=2400, units='px')







###############################################
##### DECREASED SUITABILITY -- PROPORTION #####
###############################################



## summarise and plot scenario
main_dir <- "res/scenarios_inc_dec" # "res/scenarios_x5" trial scenarios 
scenario_id <- 'decreased_suit_transm_'
pltname = 'decr_suit_prop_spread.png'
plt_ncols = 4
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,


resdirs <- c(
    file.path(main_dir, dir(main_dir, pattern = 'baseline')),
    file.path(main_dir, dir(main_dir, pattern = scenario_id)) %>%
        str_sort(numeric=T, decreasing = T)
)

ids = c(
    'baseline', 
    dir(main_dir, pattern = scenario_id) %>%
        str_sort(numeric=T, decreasing = T) %>% str_replace_all(scenario_id, 'decr. suit. prop. ')
)


figpath <- file.path(main_dir, dir(main_dir, pattern = 'fig'))
if (!dir.exists(figpath)) dir.create(figpath)


####################################
# join all dfs for zika rate plots #
####################################
### note these are not saved 

df_all_scenarios_full_summary <- map2(resdirs, ids, function(.res_dir, .id) {
    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save=FALSE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save=FALSE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(df_all_sims_sum, df_summary_by_year, save=FALSE, res_dir=.res_dir)

    df_full_summary$scenario_id = .id
    return(df_full_summary)
}, .progress = TRUE) %>% 
    bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))


##################
# zika rate tune #
##################


scenario_rate_plot <- make_scenario_rate_plot(df_all_scenarios_full_summary, ncols=plt_ncols) 
scenario_rate_plot

# save 
ggsave(scenario_rate_plot,
    filename=file.path('figs',pltname), dpi=330, 
    width=3300, height=1400, units='px')



###############################
# distribution of n countries #
# affected by day 100 and 160 # 
###############################


# calculate cumulative spread over time 
spread_cumul_timing_100 <- df_all_scenarios_full_summary %>%
    filter(outbreak_start_day <= 100) %>%
    group_by(scenario_id, simulation) %>%
    count(simulation) %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )


incid_scen_n_by_100 <- ggplot(
    spread_cumul_timing_100,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~scenario_id, ncol=plt_ncols) + 
    geom_histogram(binwidth=1, fill="#e89600") +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 100", 
        y = "Count"
    ) +
    theme_light(base_size = 12) +
    xlim(-1,15)



spread_cumul_timing_160 <- df_all_scenarios_full_summary %>%
    filter(outbreak_start_day <= 160) %>%
    group_by(scenario_id, simulation) %>%
    count(simulation) %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )

incid_scen_n_by_160 <- ggplot(
    spread_cumul_timing_160,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~scenario_id, ncol=plt_ncols) + 
    geom_histogram(binwidth=1, fill="#007492") +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 160", 
        y = "Count"
    ) +
    theme_light(base_size = 12) + xlim(-1,15)




plts = plot_grid(incid_scen_n_by_100,incid_scen_n_by_160, labels = "AUTO", ncol=1) 
plts
ggsave(plts,
    filename=file.path('figs','hists_100_160_decr_suit_prop.png'), dpi=330, 
    width=3500, height=2400, units='px')




