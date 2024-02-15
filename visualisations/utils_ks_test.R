library("tidyverse")
library("stats")


# create eCDF for Zika spread
make_zika_test_cumuls <- function() {
    df_zika_cumul <- make_df_zika_cumul()

    df_zika_cumul$date <- df_zika_cumul$date %>% str_split(' ') %>% sapply(function(.x).x[1])

    lead_vals = lead(c(df_zika_cumul$date, max(df_zika_cumul$date)), 1)
    lead_vals = lead_vals[!is.na(lead_vals)]
    reptimes = map2(lead_vals, df_zika_cumul$date, function(.t_end, .t_start) { 
        difftime(.t_end, .t_start) %>% as.numeric
    }) %>% unlist %>% unname %>% as.integer


    zika_test_cumuls <- map2(
        df_zika_cumul$cumul_nspread, 
        reptimes, 
        function(.nspread, .reptimes) {
            data.frame(cumul_nspread = rep(.nspread, times = .reptimes)) 
    }) %>% bind_rows %>% mutate(days = row_number()) # %>% filter(days <= 730)

    return(zika_test_cumuls)
}

# how many times to repeat every cumul incidence value in the 
# full-long format data 
make_reptimes <- function(.dfspread) {
    lead_vals = lead(c(.dfspread$outbreak_start_day, 730), 1)
    lead_vals = lead_vals[!is.na(lead_vals)]
    reptimes = lead_vals - .dfspread$outbreak_start_day
    return(reptimes)
}


# reformat into wide format with 
# rows = days, cols = simulations (ID)
make_wider_cumul_spread <- function(spread_cumul_timing) {
    # divide into list of sim data frames 
    ls_dfs = spread_cumul_timing %>% 
        select(simulation, outbreak_start_day, cumul_nspread) %>% group_split

    # wider format with
    # rows = days and cols = simulations 
    wider_cumul_spread <- map(ls_dfs, function(.dfspread) {
        # vector of times to repeat every variable 
        reptimes = make_reptimes(.dfspread = .dfspread)
        map2(.dfspread$cumul_nspread, reptimes, function(.nspread, .reptimes) {
            data.frame(cumul_nspread = rep(.nspread, times = .reptimes))
        }) %>%
            bind_rows() %>% mutate(
                days = row_number(), # days counted as integers from 1:n_rows
                simulation = .dfspread$simulation[1]
                )
    }) %>% bind_rows %>% 
        pivot_wider(
            names_from=simulation, 
            names_prefix="simulation_",# 'sim_',
            values_from=cumul_nspread, 
            values_fill = NA)
    return(wider_cumul_spread)
}

# add columns for mean and median 
# across every time point (day=row) 
mean_median_trajectory <- function(wider_cumul_spread) {
    # subset and summarise cdf for test 
    sum_cumul_spread <- wider_cumul_spread %>%
        rowwise() %>%
        reframe(
            days,
            mean_nspread =  mean(c_across(starts_with("sim"))),
            median_nspread = median(c_across(starts_with("sim")))
        )
    return(sum_cumul_spread)
}
