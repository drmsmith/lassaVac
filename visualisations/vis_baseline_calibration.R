library("tidyverse")
library("ggplot2")
library('cowplot')

source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')


## summarise and plot scenario
# main_dir <- "res/scenarios_baseline_calibration" # "res/scenarios_x5" trial scenarios 
# _PAHO_adjust # _PAHO_adjust_NOT
main_dir <- "res/inc_0.1-9_noise_0.1_x100" 
scenario_id <- "baseline" # file names follow this 
pltname = 'baseline_calibration_spread_pahoadj.png'
plt_ncols = 3
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,


resdirs <- file.path(main_dir, dir(main_dir, pattern = scenario_id)) %>%
    str_sort(numeric=T)
# resdirs <- resdirs[c(3,1,2)]
# resdirs <- resdirs[str_detect(resdirs, '0.2')]

figpath <- file.path(main_dir, dir(main_dir, pattern = 'fig'))
if (!dir.exists(figpath)) dir.create(figpath)


########################################
# make and save individual summary dfs #
########################################

walk(resdirs, function(.res_dir) {
    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save_RDS=TRUE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save_RDS=TRUE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save_RDS=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(
        df_all_sims_sum, df_summary_by_year, save_RDS=TRUE, res_dir=.res_dir
        )

}, .progress = TRUE)


####################################
# join all dfs for zika rate plots #
####################################

filepaths = list.files(main_dir, pattern='sim_full_summary.RDS', full.names = T, recursive = T) %>%
    str_sort(numeric=T)

ids = dir(main_dir, pattern = scenario_id) %>%
    str_sort(numeric=T) %>% # .[str_detect(., '0.2')] %>% 
    str_replace_all('_inc_', 'incidence x') %>%
    str_replace_all('noise', '\nnoise ') %>%
    str_replace_all('_', ' ') %>% 
    str_remove_all('baseline') 
# ids <- ids[c(3,1,2)]

df_all_scenarios_full_summary <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- readRDS(.fpath)
    df_full_summary$scenario_id = .id
    return(df_full_summary)
}) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))

colnames(df_all_scenarios_full_summary)

# filter(df_all_scenarios_full_summary, code %in% c('SMR', 'SYC', 'PLW')) %>% ungroup %>%
#     select(code, scenario_id, simulation, outbreak_start_day,  total_infections_all_years)



##################
# zika rate tune #
##################

scenario_rate_plot <- make_scenario_rate_plot_all_scenarios(
    df_all_scenarios_full_summary, ncols=plt_ncols, ids=ids
) 
scenario_rate_plot

# df_all_scenarios_full_summary %>% ungroup %>% count(simulation)
# df_all_scenarios_full_summary[any(is.na(df_all_scenarios_full_summary))]

# save 
ggsave(scenario_rate_plot, # rate_tune_plot
    filename=file.path('figs',pltname), dpi=330, 
    width=3400, height=3000, units='px')

ggsave(scenario_rate_plot,
    filename=file.path('figs',str_replace(pltname, 'png', 'svg')), dpi=330, 
    width=3400, height=3000, units='px')


###########################
# KOLMOGOROV-SMIRNOV TEST # 
###########################


df_ks_tests <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- readRDS(.fpath)
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
        p_val_median = test_medians$p.value, 
        mae = mean(
            abs(zika_test_cumuls$cumul_nspread - sum_cumul_spread$mean_nspread)
            , na.rm=T)
        )
    # print(test_means)
    # print(test_medians)
    
    return(list(sum_cumul_spread=sum_cumul_spread, df_ks_test=df_ks_test))
}, .progress = T) # %>% bind_rows


df_ks_test <- map(df_ks_tests, ~.x$'df_ks_test') %>% bind_rows
rownames(df_ks_test) <- NULL

# mean
df_ks_test %>% select(scenario, statistic_mean, p_val_mean) %>% arrange(statistic_mean)
# median
df_ks_test %>% select(scenario, statistic_median, p_val_median) %>% arrange(statistic_median)

# mean
df_ks_test %>% select(scenario, statistic_mean) %>% arrange(statistic_mean)
# median
df_ks_test %>% select(scenario, statistic_median) %>% arrange(statistic_median)


# mean absolute error 
df_ks_test %>% select(scenario, mae) %>% arrange(mae)

df_sum_cumul_spread <- map(df_ks_tests, ~.x$'sum_cumul_spread') %>% bind_rows(.id='ids')
rownames(df_ks_test) <- NULL

write.csv(df_ks_test, file.path(main_dir, 'df_ks_mae.csv'), row.names=F)







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
    geom_histogram(binwidth=1, fill="#007492", boundary = 0.5) +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 100", 
        y = "Count"
    ) +
    theme_light(base_size = 12) +
    xlim(0.5,8)



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
    geom_histogram(binwidth=1, fill="#e89600", boundary = 0.5) +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 160", 
        y = "Count"
    ) +
    theme_light(base_size = 12) + xlim(0.5,8)



plts = plot_grid(incid_scen_n_by_100,incid_scen_n_by_160, labels = "AUTO", ncol=1) 
plts

ggsave(plts,
    filename=file.path('figs', 'hists_100_160_incidence.png'), dpi=330, 
    width=2500, height=3000, units='px')

ggsave(plts,
    filename=file.path('figs', 'hists_100_160_incidence.svg'), dpi=330, 
    width=2500, height=3000, units='px')


######################
# cases on first day # 
######################


filepaths = list.files(main_dir, pattern='sim_res_long.RDS', full.names = T, recursive = T) %>%
    str_sort(numeric=T)

df_first_day_cases <- map2(filepaths, ids, function(.fpath, .id) {
    df_res_long <- readRDS(.fpath)
    return(df_res_long %>% filter(time_days == 0) %>% mutate(scenario_id = .id))
}, .progress=T) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))


df_first_day_cases %>% head(n=10)
# plot( 100 *df_first_day_cases$daily_infections_sim / df_first_day_cases$IncCumul_U_final)

dfs_first_day_cases <- df_first_day_cases %>% 
    # group_by(scenario_id) %>% 
    group_by(scenario_id) %>% group_split %>%
    map(function(.df) return(quantile(unlist(.df[,'daily_infections_sim']), probs=seq(0,1,0.05)))
        ) %>% 
        setNames(ids) %>% bind_rows(.id='scenario_id')

dfs_first_day_cases_wide = dfs_first_day_cases %>% t %>% data.frame(row.names = names(.)) %>% .[-1,]
colnames(dfs_first_day_cases_wide) = ids
dfs_first_day_cases_wide$percentile = seq(0,1,0.05)

dfs_first_day_cases_wide %>% 
    reshape2::melt(id='percentile', variable.name='scenario_id', value.name='first_day_cases') %>% 
    mutate(first_day_cases = as.numeric(as.character(first_day_cases))) %>% 
    arrange(first_day_cases) %>%
    ggplot(aes(first_day_cases, percentile)) + 
    geom_line(aes(group=scenario_id, color=scenario_id)) + 
    theme_light() + # facet_wrap(vars(scenario_id), ncol=1) + 
    labs(y='Percentile', x='Incidence on day 1')

ggsave(
    file.path(main_dir, 'figs', 'percentiles_first_day.png'), 
    width=2000, height=1400, units='px', bg='transparent'
    )


###############################
# cases per 100k on first day # 
###############################

dfs_first_day_cases_100k <- df_first_day_cases %>% 
    mutate(daily_infect_per100k = 1e5*daily_infections_sim/pop_size) %>%
    # group_by(scenario_id) %>% 
    group_by(scenario_id) %>% group_split %>%
    map(function(.df) return(quantile(unlist(.df[,'daily_infect_per100k']), probs=seq(0,1,0.05)))
        ) %>% 
        setNames(ids) %>% bind_rows(.id='scenario_id')

dfs_first_day_cases_100k_wide = dfs_first_day_cases_100k %>% t %>% data.frame(row.names = names(.)) %>% .[-1,]
colnames(dfs_first_day_cases_100k_wide) = ids
dfs_first_day_cases_100k_wide$percentile = seq(0,1,0.05)

dfs_first_day_cases_100k_wide %>% 
    reshape2::melt(id='percentile', variable.name='scenario_id', value.name='first_day_cases_100k') %>% 
    mutate(first_day_cases_100k = as.numeric(as.character(first_day_cases_100k))) %>% 
    arrange(first_day_cases_100k) %>%
    ggplot(aes(first_day_cases_100k, percentile)) + 
    geom_line(aes(group=scenario_id, color=scenario_id)) + 
    theme_light() + # facet_wrap(vars(scenario_id), ncol=1) + 
    labs(y='Percentile', x='Incidence per 100k on day 1')

ggsave(
    file.path(main_dir, 'figs', 'percentiles_first_day_100k.png'), 
    width=2000, height=1400, units='px', bg='transparent'
    )


#################################
# get spread by 100 or 160 days #
#################################

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

df_quantiles_ncountries_100_days = quantiles_ncountries_100_days %>% t %>% data.frame(row.names = names(.)) %>% .[-1,]
colnames(df_quantiles_ncountries_100_days) = ids
df_quantiles_ncountries_100_days$percentile = seq(0,1,0.05)

df_quantiles_ncountries_100_days %>% 
    reshape2::melt(id='percentile', variable.name='scenario_id', value.name='n_spread_100') %>% 
    mutate(n_spread_100 = as.numeric(as.character(n_spread_100))) %>% 
    arrange(n_spread_100) %>%
    ggplot(aes(n_spread_100, percentile)) + 
    geom_line(aes(group=scenario_id, color=scenario_id)) + 
    theme_light() + # facet_wrap(vars(scenario_id), ncol=1) + 
    labs(y='Percentile', x='Number of countries with outbreak by day 100')

ggsave(
    file.path(main_dir, 'figs', 'nspread_day_100.png'), 
    width=2000, height=1400, units='px', bg='transparent'
    )




ls_dfs_160 <- spread_cumul_timing %>% 
    select(scenario_id, simulation, country, code, outbreak_start_day, n, cumul_nspread) %>%    
    filter(outbreak_start_day < 160) %>% 
    summarise(ncumul = max(cumul_nspread), .groups='keep') %>% arrange(scenario_id) %>% 
    group_by(scenario_id) %>% group_split
    
quantiles_ncountries_160_days <- ls_dfs_160 %>%
    map(function(.df) quantile(unlist(.df[,'ncumul']), probs=seq(0,1,0.05))
    ) %>% setNames(ids) %>% bind_rows(.id='scenario_id')

df_quantiles_ncountries_160_days = quantiles_ncountries_160_days %>% t %>% data.frame(row.names = names(.)) %>% .[-1,]
colnames(df_quantiles_ncountries_160_days) = ids
df_quantiles_ncountries_160_days$percentile = seq(0,1,0.05)

df_quantiles_ncountries_160_days %>% 
    reshape2::melt(id='percentile', variable.name='scenario_id', value.name='n_spread_160') %>% 
    mutate(n_spread_160 = as.numeric(as.character(n_spread_160))) %>% 
    arrange(n_spread_160) %>%
    ggplot(aes(n_spread_160, percentile)) + 
    geom_line(aes(group=scenario_id, color=scenario_id)) + 
    theme_light() + # facet_wrap(vars(scenario_id), ncol=1) + 
    labs(y='Percentile', x='Number of countries with outbreak by day 160')

ggsave(
    file.path(main_dir, 'figs', 'nspread_day_160.png'), 
    width=2000, height=1400, units='px', bg='transparent'
    )


first_day_n_countries_quantiles <- list(
    dfs_first_day_cases_wide=dfs_first_day_cases_wide,
    dfs_first_day_cases_100k_wide=dfs_first_day_cases_100k_wide,
    df_quantiles_ncountries_100_days=df_quantiles_ncountries_100_days, 
    df_quantiles_ncountries_160_days=df_quantiles_ncountries_160_days
)
saveRDS(
    first_day_n_countries_quantiles, 
    file.path(main_dir, 'first_day_n_countries_quantiles.RDS')
)







#####################
# 0 cases countries #
#####################

df_all_scenarios_full_summary %>% filter(total_infections_all_years == 0) %>% 
    ungroup %>%
    select(
        scenario_id, simulation, code, # pop_size, 
        outbreak_start_day, total_infections_all_years
        ) %>% count(scenario_id, code)

df_all_scenarios_full_summary %>% filter(total_infections_all_years == 0) %>% 
    ungroup %>%
    select(
        scenario_id, simulation, country, code, pop_size
        ) %>% count(code, country, pop_size)

colnames(df_all_scenarios_full_summary)

# 1 PLW   Palau        13493.    12
# 2 SMR   San Marino   32629.     4
# 3 SYC   Seychelles   84374.    31

df_all_scenarios_full_summary %>% filter(total_infections_all_years == 0) %>% 
    ungroup %>%
    select(
        scenario_id, simulation, country, code, pop_size, prop_affected
        )


