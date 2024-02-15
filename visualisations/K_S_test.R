library("tidyverse")
library("stats")
library("ggplot2")
library('cowplot')


source('visualisations/utils_post_proc.R')
source('visualisations/utils_ks_test.R')

df_results_summary <- read.csv('res/EDA_scenarios/baseline_x9/sim_summary.csv')
df_full_summary <- read.csv('res/EDA_scenarios/baseline_x9/sim_full_summary.csv')

# helper files
df_burden = read.csv('data/2019ppp_pd_df_suit_means_who_p_spillover.csv') 


## summarise and plot scenario
main_dir <- "res/scenarios_incidence" # "res/scenarios_x5" trial scenarios 
scenario_id <- "incidence"
pltname = 'incidence_scenarios_spread.png'
plt_ncols = 5
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,

# create zika reference 

zika_test_cumuls <- make_zika_test_cumuls()

# read in data from the following files 

filepaths = list.files(main_dir, pattern='sim_full_summary.csv', full.names = T, recursive = T) %>%
    str_sort(numeric=T)

ids = dir(main_dir, pattern = scenario_id) %>%
    str_sort(numeric=T) %>% str_replace('_', ' ')


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
    print(test_means)
    print(test_medians)
    return(df_ks_test)
}, .progress = T) %>% bind_rows

rownames(df_ks_tests) <- NULL

df_ks_tests %>% select(scenario, statistic_median) %>% arrange(statistic_median)
df_ks_tests %>% select(scenario, statistic_mean) %>% arrange(statistic_mean)



# add cumulative timings 
spread_cumul_timing <- df_full_summary %>%
    group_by(simulation) %>%
    add_count(simulation, code) %>%
    mutate(
        cumul_nspread = cumsum(n),
        simulation = factor(simulation)
    )

# divide into list of sim data frames 
ls_dfs = spread_cumul_timing %>% 
    select(simulation, outbreak_start_day, cumul_nspread) %>% group_split


# wider format with
# rows = days
# cols = simulations 
wider_cumul_spread <- map(ls_dfs, function(.dfspread) {
    reptimes = make_reptimes(.dfspread = .dfspread)
    map2(.dfspread$cumul_nspread, reptimes, function(.nspread, .reptimes) {
        data.frame(cumul_nspread = rep(.nspread, times = .reptimes))
    }) %>%
        bind_rows() %>% mutate(
            days = row_number(),
            simulation = .dfspread$simulation[1]
            )
}) %>% bind_rows %>% 
    pivot_wider(
        names_from=simulation, 
        names_prefix="simulation_",# 'sim_',
        values_from=cumul_nspread, 
        values_fill = NA)

# check for NAs 
# apply(wider_cumul_spread, 1, is.na)

# subset and summarise cdf for test 
sum_cumul_spread <- wider_cumul_spread %>%
    rowwise() %>%
    reframe(
        days,
        mean_nspread =  mean(c_across(starts_with("sim"))),
        median_nspread = median(c_across(starts_with("sim")))
    )



# run tests 
test_means <- ks.test(sum_cumul_spread$mean_nspread, zika_test_cumuls$cumul_nspread)
test_medians <- ks.test(sum_cumul_spread$median_nspread, zika_test_cumuls$cumul_nspread)

test_means$statistic
test_means$p.value

# sum_cumul_spread[730,]
# zika_test_cumuls[730,]
