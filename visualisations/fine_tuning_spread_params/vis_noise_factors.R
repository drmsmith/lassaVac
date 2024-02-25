library("tidyverse")
library("ggplot2")
library('cowplot')


source('utils/utils_post_proc.R')

## summarise and plot scenario
main_dir <- "res/scenarios_inc_noise_50" # "res/scenarios_x5" trial scenarios 
scenario_id <- "incidence_noise"
pltname = 'incidence_noise_spread.png'
plt_ncols = 3
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
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save=TRUE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save=TRUE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(df_all_sims_sum, df_summary_by_year, save=TRUE, res_dir=.res_dir)

}, .progress = TRUE)


####################################
# join all dfs for zika rate plots #
####################################


filepaths = list.files(main_dir, pattern='sim_full_summary.csv', full.names = T, recursive = T) %>%
    str_sort(numeric=T)
filepaths <- filepaths[c(6,1,5,2,3,4)]

ids = dir(main_dir, pattern = scenario_id) %>%
    str_sort(numeric=T) %>% str_replace_all('_', ' ')
ids <- ids[c(6,1,5,2,3,4)]

df_all_scenarios_full_summary <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- read.csv(.fpath)
    df_full_summary$scenario_id = .id
    return(df_full_summary)
}) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))

colnames(df_all_scenarios_full_summary)

df_all_scenarios_full_summary %>% 
    mutate(
        per_100k = 1e5*total_infections_all_years/pop_size
        ) %>%
    filter(total_infections_all_years > 1e6) %>%
    group_by(code, scenario_id) %>% 
    summarise(
        median_inc = median(per_100k), 
        q1_inc = quantile(per_100k, 0.25),
        q3_inc = quantile(per_100k, 0.75),
        .groups='keep'
    ) %>%
    ggplot(aes(x=code, y=median_inc)) +
    geom_bar(stat = "identity", aes(fill = code)) +
    geom_errorbar(
        aes(x = code, ymin = q1_inc, ymax = q3_inc),
        width = 0.4,
        colour = "grey33",
        alpha = 0.9, linewidth = 1.3 
    ) + 
    theme_light(base_size=14) + 
    guides(fill='none') + 
    facet_wrap(vars(scenario_id)) + 
    labs(x='Country code', y='Infections per 100k (median +- IQR)')

##################
# zika rate tune #
##################



scenario_rate_plot <- make_scenario_rate_plot(df_all_scenarios_full_summary, ncols=plt_ncols) 
scenario_rate_plot

# save 
ggsave(scenario_rate_plot,
    filename=file.path('figs',pltname), dpi=330, 
    width=3300, height=1400, units='px')


