library("tidyverse")
library("ggplot2")
library('cowplot')

source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')


## summarise and plot scenario
main_dir <- "res/scenarios_baseline_calibration" # "res/scenarios_x5" trial scenarios 
scenario_id <- "baseline" # file names follow this 
pltname = 'baseline_calibration_spread.png'
plt_ncols = 3
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,


resdirs <- file.path(main_dir, dir(main_dir, pattern = scenario_id)) %>%
    str_sort(numeric=T)
resdirs <- resdirs[c(3,1,2,6,4,5,9,7,8)]

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
filepaths <- filepaths[c(3,1,2,6,4,5,9,7,8)]

ids = dir(main_dir, pattern = scenario_id) %>%
    str_sort(numeric=T) %>% str_replace_all('_', ' ') %>% 
    str_remove_all('baseline') %>% str_replace('inc ', 'inc x')
ids <- ids[c(3,1,2,6,4,5,9,7,8)]


df_all_scenarios_full_summary <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- read.csv(.fpath)
    df_full_summary$scenario_id = .id
    return(df_full_summary)
}) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))



filepaths = list.files(main_dir, pattern='sim_res_long.csv', full.names = T, recursive = T) %>%
    str_sort(numeric=T)
filepaths <- filepaths[c(3,1,2,6,4,5,9,7,8)]

df_first_day_cases <- map2(filepaths, ids, function(.fpath, .id) {
    df_res_long <- read.csv(.fpath)
    return(df_res_long %>% filter(time_days == 0)) # %>% mutate(scenario_id = .id)
}, .progress=T) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))



# plot( 100 *df_first_day_cases$daily_infections_sim / df_first_day_cases$IncCumul_U_final)

df_first_day_cases %>% 
    group_by(scenario_id) %>% 
    group_by(scenario_id) %>% group_split
    map(function(.df) quantile(unlist(.df[,'ncumul']), probs=seq(0,1,0.05))
        ) %>% 
        setNames(ids) %>% bind_rows(.id='scenario_id')


# get spread by 100 or 160 days 

spread_cumul_timing <- df_all_scenarios_full_summary %>%
    group_by(simulation, scenario_id) %>%
    add_count(simulation, code) %>%
    mutate(
        cumul_nspread = cumsum(n),
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )

ls_dfs_100 <- spread_cumul_timing %>% 
    select(scenario_id, simulation, country, code, outbreak_start_day, n, cumul_nspread) %>%    
    filter(outbreak_start_day < 100) %>% 
    summarise(ncumul = max(cumul_nspread), .groups='keep') %>% arrange(scenario_id) %>% 
    group_by(scenario_id) %>% group_split
    
quantiles_ncountries_100_days <- ls_dfs_100 %>%
    map(function(.df) quantile(unlist(.df[,'ncumul']), probs=seq(0,1,0.05))
    ) %>% setNames(ids) %>% bind_rows(.id='scenario_id')


ls_dfs_160 <- spread_cumul_timing %>% 
    select(scenario_id, simulation, country, code, outbreak_start_day, n, cumul_nspread) %>%    
    filter(outbreak_start_day < 160) %>% 
    summarise(ncumul = max(cumul_nspread), .groups='keep') %>% arrange(scenario_id) %>% 
    group_by(scenario_id) %>% group_split
    
quantiles_ncountries_160_days <- ls_dfs_160 %>%
    map(function(.df) quantile(unlist(.df[,'ncumul']), probs=seq(0,1,0.05))
    ) %>% setNames(ids) %>% bind_rows(.id='scenario_id')

n_countries_quantiles <- list(
    quantiles_ncountries_100_days=quantiles_ncountries_100_days, 
    quantiles_ncountries_160_days=quantiles_ncountries_160_days
)
# saveRDS(n_countries_quantiles, 'res/scenarios_baseline_calibration/n_countries_quantiles.RDS')
q_160_mat = as.matrix(quantiles_ncountries_160_days[,2:21])
rownames(q_160_mat) = unlist(quantiles_ncountries_160_days[,1])
heatmap(q_160_mat)


##################
# zika rate tune #
##################
source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')

scenario_rate_plot <- make_scenario_rate_plot_all_scenarios(
    df_all_scenarios_full_summary, ncols=plt_ncols, ids=ids
) 
scenario_rate_plot

# save 
ggsave(scenario_rate_plot, # rate_tune_plot
    filename=file.path('figs',pltname), dpi=330, 
    width=3300, height=2800, units='px')


###########################
# KOLMOGOROV-SMIRNOV TEST # 
###########################


df_ks_tests <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- read.csv(.fpath)
    # add cumulative timings 
    spread_cumul_timing <- df_full_summary %>%
        group_by(simulation) %>%
        add_count(simulation, code) %>%
        mutate(
            cumul_nspread = cumsum(n),
            simulation = factor(simulation)
        )

    wider_cumul_spread <- make_wider_cumul_spread(spread_cumul_timing)
    # check for NAs 
    # apply(wider_cumul_spread, 1, is.na)

    # subset and summarise cdf for test 
    sum_cumul_spread <- mean_median_trajectory(wider_cumul_spread)

    # zika cumuls for test 
    zika_test_cumuls <- make_zika_test_cumuls()
    # run tests 
    test_means <- ks.test(sum_cumul_spread$mean_nspread, zika_test_cumuls$cumul_nspread)
    test_medians <- ks.test(sum_cumul_spread$median_nspread, zika_test_cumuls$cumul_nspread)

    df_ks_test <- data.frame(
        scenario = .id,
        statistic_mean = test_means$statistic, 
        p_val_mean = test_means$p.value, 
        statistic_median = test_medians$statistic, 
        p_val_median = test_medians$p.value
        )
    # print(test_means)
    # print(test_medians)
    return(df_ks_test)
}, .progress = T) %>% bind_rows

rownames(df_ks_tests) <- NULL

# mean
df_ks_tests %>% select(scenario, statistic_mean, p_val_mean) %>% arrange(statistic_mean)
# median
df_ks_tests %>% select(scenario, statistic_median, p_val_median) %>% arrange(statistic_median)

# mean
df_ks_tests %>% select(scenario, statistic_mean) %>% arrange(statistic_mean)
# median
df_ks_tests %>% select(scenario, statistic_median) %>% arrange(statistic_median)











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
