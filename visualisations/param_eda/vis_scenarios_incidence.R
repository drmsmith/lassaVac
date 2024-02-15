library("tidyverse")
library("ggplot2")
library('cowplot')

source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')


## summarise and plot scenario
main_dir <- "res/scenarios_incidence" # "res/scenarios_x5" trial scenarios 
scenario_id <- "incidence"
pltname = 'incidence_scenarios_spread.png'
plt_ncols = 5
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,


resdirs <- file.path(main_dir, dir(main_dir, pattern = scenario_id)) %>%
    str_sort(numeric=T)

figpath <- file.path(main_dir, dir(main_dir, pattern = 'fig'))
if (!dir.exists(figpath)) dir.create(figpath)


########################################
# make and save individual summary dfs #
########################################

walk(resdirs, function(.res_dir) {
    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save=F)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save=F)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(df_all_sims_sum, df_summary_by_year, save=F, res_dir=.res_dir)

}, .progress = TRUE)


####################################
# join all dfs for zika rate plots #
####################################

filepaths = list.files(main_dir, pattern='sim_full_summary.csv', full.names = T, recursive = T) %>%
    str_sort(numeric=T)

ids = dir(main_dir, pattern = scenario_id) %>%
    str_sort(numeric=T) %>% str_replace('_', ' ')

source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')


df_all_scenarios_full_summary <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- read.csv(.fpath)
    df_full_summary$scenario_id = .id
    return(df_full_summary)
}) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))




##################
# zika rate tune #
##################

scenario_rate_plot <- make_scenario_rate_plot(df_all_scenarios_full_summary, ncols=plt_ncols) 
scenario_rate_plot

# save 
ggsave(scenario_rate_plot, # rate_tune_plot
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
    filename=file.path('figs','hists_100_160_incidence.png'), dpi=330, 
    width=3500, height=2400, units='px')
