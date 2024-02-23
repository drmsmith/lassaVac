library("tidyverse")
library("ggplot2")

#############################
# all simulations in one df #
#############################

make_df_all_sims_long <- function(
    res_dir,        # directory with rds simulation results
    save_CSV = FALSE,   # TRUE : saved to res_dir as 'sim_res_long.csv'
    save_RDS = TRUE,    # TRUE : saved to res_dir as 'sim_res_long.RDS'
    dest_dir = NULL # save to alternative to res_dir
    ) {
    # get all file paths
    all_files <-list.files(res_dir, "RDS", full.names = T) %>%
        str_sort(numeric = TRUE) %>% .[str_detect(., 'simulation_')]

    df_all_sims_long <- map(all_files, function(dirname) {
        res <- readRDS(dirname)
        map(res, function(.ls_sim) .ls_sim$df_res_curve) %>% bind_rows()
    }, .progress = T) %>% bind_rows()

    if (save_CSV == TRUE) {
        if (!is.null(dest_dir)) {
            fpath <- file.path(res_dir, "sim_res_long.csv")
            write.csv(df_all_sims_long, fpath, row.names = F)
        } else {
            fpath <- file.path(res_dir, "sim_res_long.csv")
            write.csv(df_all_sims_long, fpath, row.names = F)
        }
    }
    if (save_RDS == TRUE) {
        if (!is.null(dest_dir)) {
            fpath <- file.path(res_dir, "sim_res_long.RDS")
            saveRDS(df_all_sims_long, file = fpath)
        } else {
            fpath <- file.path(res_dir, "sim_res_long.RDS")
            saveRDS(df_all_sims_long, file = fpath)
        }
    }
    return(df_all_sims_long)
}


####################
# summaries in one #
####################

make_df_all_sims_sum <- function(
    res_dir,        # directory with rds simulation results
    save_CSV = FALSE,   # TRUE : saved to res_dir as 'sim_summary.csv'
    save_RDS = TRUE,    # TRUE : saved to res_dir as 'sim_summary.RDS'
    dest_dir = NULL # save to alternative to res_dir
    ) {
    # get all file paths
    all_files <- all_files <-list.files(res_dir, "RDS", full.names = T) %>%
        str_sort(numeric = TRUE) %>% .[str_detect(., 'simulation_')]

    df_all_sims_sum <- map(all_files, function(dirname) {
        res <- readRDS(dirname)
        map(res, function(.ls_sim) .ls_sim$df_res_summary) %>% bind_rows()
    }, .progress = T) %>% bind_rows()

    if (save_CSV == TRUE) {
        if (!is.null(dest_dir)) {
            fpath <- file.path(res_dir, "sim_summary.csv")
            write.csv(df_all_sims_sum, fpath, row.names = F)
        } else {
            fpath <- file.path(res_dir, "sim_summary.csv")
            write.csv(df_all_sims_sum, fpath, row.names = F)
        }
    }
    if (save_RDS == TRUE) {
        if (!is.null(dest_dir)) {
            fpath <- file.path(res_dir, "sim_summary.RDS")
            saveRDS(df_all_sims_sum, file = fpath)
        } else {
            fpath <- file.path(res_dir, "sim_summary.RDS")
            saveRDS(df_all_sims_sum, file = fpath)
        }
    }
    return(df_all_sims_sum)
}




#######################################################
# get summary of years 1, 2, 1+2 and total infections #
#######################################################

make_df_summary_by_year <- function(
    df_all_sims_long,   # output of make_df_all_sims_long
    save_CSV = FALSE,   # TRUE : saved to res_dir as 'sim_summary_by_year.csv'
    save_RDS = TRUE,    # TRUE : saved to res_dir as 'sim_summary_by_year.RDS'
    res_dir = NULL      # defaults to working dir
    ) {
    df_summary_by_year <- df_all_sims_long %>%
        group_by(
            simulation, country, code, region_name, region_code,
            pop_size, timing
        ) %>%
        summarise(
            first_year = sum(
                daily_infections_sim[time_days > timing & time_days < (timing + 365)]
            ),
            second_year = sum(
                daily_infections_sim[time_days > (timing + 365) & time_days < (timing + 365 * 2)]
            ),
            total_infections_all_years = sum(daily_infections_sim),
            years_1_2 = first_year + second_year,
            .groups = "keep"
        ) %>%
        rename(outbreak_start_day = timing)
    if (save_CSV == TRUE) {
        if (is.null(res_dir) == TRUE) {
            res_dir <- getwd()
        }
        fpath <- file.path(res_dir, "sim_summary_by_year.csv")
        write.csv(df_summary_by_year, fpath, row.names = F)
    }
    if (save_RDS == TRUE) {
        if (is.null(res_dir) == TRUE) {
            res_dir <- getwd()
        }
        fpath <- file.path(res_dir, "sim_summary_by_year.RDS")
        saveRDS(df_summary_by_year, file = fpath)
    }
    return(df_summary_by_year)
}

######################
# add duration info  #
######################
# (also optionally the
# pop affected parameter (randomly sampled)
# or the paho curve id)

make_df_full_summary <- function(
    df_all_sims_sum,    # output of make_df_all_sims_sum
    df_summary_by_year, # output of make_df_summary_by_year
    save_CSV = FALSE,   # TRUE : saved to res_dir as 'sim_full_summary.csv'
    save_RDS = TRUE,    # TRUE : saved to res_dir as 'sim_full_summary.RDS'
    res_dir = NULL      # defaults to working dir
    ) {
    df_full_summary <- df_all_sims_sum %>%
        group_by(
            simulation, country, code, region_name, region_code,
            pop_size, timing
        ) %>%
        dplyr::select(
            simulation, country, code, region_name, region_code,
            pop_size, timing, duration
        ) %>%
        rename(outbreak_start_day = timing, outbreak_duration_yrs = duration) %>%
        full_join(
            df_summary_by_year,
            by = c(
                "simulation", "country", "code", "region_name", "region_code",
                "pop_size", "outbreak_start_day"
            )
        )
    if (save_CSV == TRUE) {
        if (is.null(res_dir) == TRUE) {
            res_dir <- getwd()
        }
        fpath <- file.path(res_dir, "sim_full_summary.csv")
        write.csv(df_full_summary, fpath, row.names = F)
    }
    if (save_RDS == TRUE) {
        if (is.null(res_dir) == TRUE) {
            res_dir <- getwd()
        }
        fpath <- file.path(res_dir, "sim_full_summary.RDS")
        saveRDS(df_full_summary, file = fpath)
    }
    return(df_full_summary)
}



#######################################
# plot spread against zika v baseline #
#######################################


make_df_zika_cumul <- function(zika_data = NULL) {
    if (is.null(zika_data)) {
        zika_data <- file.path("preprocessing/data/zika/nextstrain_zika_metadata.tsv")
    }
    zika_phyl_meta <- read.table(zika_data, header = T, sep = "\t")

    df_zika_cumul <- zika_phyl_meta %>%
        filter(date > "2014-06-01" & date < "2016-12-31") %>%
        group_by(country) %>%
        arrange(date) %>%
        summarise(date = first(date)) %>%
        add_count(country) %>%
        arrange(date) %>%
        mutate(
            cumul_nspread = cumsum(n),
            country = factor(country, levels = country),
            date_frac = lubridate::decimal_date(as.Date(date)),
            date_norm = date_frac - min(date_frac)
        ) %>% arrange(date) 

    return(df_zika_cumul)
}


make_spread_plot <- function(
    df_full_summary,    # output of make_df_full_summary
    df_zika_cumul,      # output of make_df_zika_cumul
    extrargs=NULL       # some ggplot params
    ) {

    sim_line_col <- ifelse(
        is.null(extrargs$sim_line_col), 
        "darkgray", extrargs$sim_line_col
        )
    zika_line_col <- ifelse(
        is.null(extrargs$zika_line_col), 
        "black", extrargs$zika_line_col
        )
    fontsize <- ifelse(
        is.null(extrargs$fontsize), 
        16, extrargs$fontsize
        )
    ylab <- ifelse(
        is.null(extrargs$ylab), 
        "Number of countries\nexperiencing outbreaks", extrargs$ylab
        )
    

    spread_cumul_timing <- df_full_summary %>%
        group_by(simulation) %>%
        add_count(simulation, code) %>%
        mutate(
            cumul_nspread = cumsum(n),
            simulation = factor(simulation)
        )

    rate_tune_plot <- ggplot(
        spread_cumul_timing,
        aes(x = outbreak_start_day / 365, y = cumul_nspread)
    ) +
        # geom_line(aes(color=simulation), alpha=0.5) +
        geom_line(aes(group = simulation), color = sim_line_col, linewidth = 1, alpha = 0.45) +
        # scale_color_manual(values = vec_colrs_region, name='WHO region') +
        # scale_color_manual(values = rep("darkgray", times = 100)) +
        geom_line(
            data = df_zika_cumul, aes(x = date_norm, y = cumul_nspread),
            linewidth = 2, color = zika_line_col
        ) +
        guides(color = "none") +
        xlim(0, 2.1) +
        coord_cartesian(expand=FALSE) + 
        # ylim(0, 120) +
        labs(
            x = "Time (years)", # x='Simulation time (years)',
            y = ylab
        ) +
        theme_light(base_size = fontsize)

    return(rate_tune_plot)
}


###############################
# plot cumulative n countries #
# against zika spread rates   # 
# for different scenarios     #
###############################

make_scenario_rate_plot <- function(
    # joined df_full_summary for scenarios, needs a scenario_id column
    df_all_scenarios_full_summary, 
    ncols = 5 # number of columns in facet_wrap 
    ) {
    
    # calculate cumulative spread over time 
    spread_cumul_timing <- df_all_scenarios_full_summary %>%
        group_by(simulation, scenario_id) %>%
        add_count(simulation, code) %>%
        mutate(
            cumul_nspread = cumsum(n),
            simulation = factor(simulation),
            scenario_id = factor(scenario_id, levels = ids)
        )
    
    # prepare zika baseline 
    df_zika_cumul <- make_df_zika_cumul()

    rate_tune_plot <- ggplot(
        spread_cumul_timing,
        aes(x = outbreak_start_day / 365, y = cumul_nspread)
        ) +
        facet_wrap(~scenario_id, ncol=ncols) + 
        geom_line(
            aes(group = simulation), color = "#8ca8d4", # "#bd5e8d", # "darkgray", 
            linewidth = 0.8, alpha = 0.45
            ) +
        geom_line(
            data = df_zika_cumul, aes(x = date_norm, y = cumul_nspread),
            linewidth = 1.5, color = "#2e4569"# "#5f2457" # "black"
        ) + 
        guides(color = "none") +
        xlim(0, 2.1) +
        coord_cartesian(expand=FALSE) + 
        labs(
            x = "Time (years)", 
            y = "Number of countries\nexperiencing outbreaks"
        ) +
        theme_light(base_size = 16)

    return(rate_tune_plot)
}


make_scenario_rate_plot_all_scenarios <- function(
    # joined df_full_summary for scenarios, needs a scenario_id column
    df_all_scenarios_full_summary, 
    ncols = 5, # number of columns in facet_wrap 
    ids = NULL # levels of factor  
    ) {
    
    # calculate cumulative spread over time 
    spread_cumul_timing <- df_all_scenarios_full_summary %>%
        group_by(simulation, scenario_id) %>%
        add_count(simulation, code) %>%
        mutate(
            cumul_nspread = cumsum(n),
            simulation = factor(simulation),
            scenario_id = factor(scenario_id, levels = ids)
        )

    wider_cumul_spread <- spread_cumul_timing %>% 
        group_by(scenario_id) %>% group_split() %>% 
        map(function(spread_cumul_timing) {
            spread_cumul_timing %>%
                group_by(simulation, scenario_id) %>%
                make_wider_cumul_spread()
            }) %>%
        setNames(ids) %>% bind_rows(.id='scenario_id') %>%
        mutate(scenario_id = factor(scenario_id, levels=ids))
    # subset and summarise cdf for test 
    sum_cumul_spread <- wider_cumul_spread %>% 
        group_by(scenario_id) %>% group_split() %>%  
        map(function(wider_cumul_spread) {
            wider_cumul_spread %>% 
                select(!scenario_id) %>%
                mean_median_trajectory()
            }, .progress = T) %>%
        setNames(ids) %>% 
        bind_rows(.id='scenario_id') %>%
        mutate(scenario_id = factor(scenario_id, levels=ids))

        


    # prepare zika baseline 
    df_zika_cumul <- make_df_zika_cumul()

    rate_tune_plot <- ggplot(
        spread_cumul_timing,
        aes(x = outbreak_start_day / 365, y = cumul_nspread)
        ) +
        facet_wrap(~scenario_id, ncol=ncols) + 
        geom_line(
            aes(group = simulation), color = "#8ca8d4", # "#bd5e8d", # "darkgray", 
            linewidth = 0.8, alpha = 0.45
            ) +
        geom_line(
            data = df_zika_cumul, aes(x = date_norm, y = cumul_nspread),
            linewidth = 1, color = "#5f2457",# "#5f2457" # "black"
            alpha=0.9
        ) + 
        geom_line(
            data = sum_cumul_spread, aes(x = days/365, y = mean_nspread),
            linewidth = 1, color = "#2e4569",# "#5f2457" # "black"
            linetype='dotted', alpha=0.9
        ) +
        geom_line(
            data = sum_cumul_spread, aes(x = days/365, y = median_nspread),
            linewidth = 1, color = "#2e4569",# "#5f2457" # "black"
            linetype='dashed', alpha=0.9
        ) +
        guides(color = "none") +
        xlim(0, 2.1) +
        coord_cartesian(expand=FALSE) + 
        labs(
            x = "Time (years)", 
            y = "Number of countries\nexperiencing outbreaks"
        ) +
        theme_light(base_size = 16)
        rate_tune_plot

    return(rate_tune_plot)
}


