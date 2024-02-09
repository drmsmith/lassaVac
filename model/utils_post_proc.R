### spread against baseline 

zika_phyl_meta = read.table('preprocessing/data/zika/nextstrain_zika_metadata.tsv', header=T, sep='\t')


df_zika_cumul <- zika_phyl_meta %>% 
    filter(date > '2014-06-01' & date < '2016-12-31') %>% 
    group_by(country) %>%
    summarise(date = first(date)) %>%
    arrange(date) %>%
    add_count(country) %>%
    mutate(
        cumul_nspread = cumsum(n), 
        country = factor(country, levels = country),
        date_frac = lubridate::decimal_date(as.Date(date)), 
        date_norm = date_frac - min(date_frac)
    )




make_summary_by_year <- function(df_all_sims_long) {
    df_summary_by_year = df_all_sims_long %>%
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
            .groups = 'keep'
        ) %>% 
        rename(outbreak_start_day = timing)
    return(df_summary_by_year)
}


make_full_summary <- function(df_all_sims_sum, df_summary_by_year) {
    df_full_summary = df_all_sims_sum %>%
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
            by=c(
                'simulation', 'country', 'code', 'region_name', 'region_code', 
                'pop_size', 'outbreak_start_day'
            ) 
        )
    return(df_full_summary)
}


make_spread_plot <- function(df_full_summary, df_zika_cumul) {
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
        geom_line(aes(group = simulation), color = 'darkgray', linewidth = 1, alpha = 0.45) +
        # scale_color_manual(values = vec_colrs_region, name='WHO region') +
        # scale_color_manual(values = rep("darkgray", times = 100)) +
        geom_line(
            data = df_zika_cumul, aes(x = date_norm, y = cumul_nspread),
            linewidth = 2, color = "black"
        ) +
        guides(color='none') +
        xlim(0, 2) +
        # ylim(0, 72) +
        labs(
            x = "Time (years)", # x='Simulation time (years)',
            y = "Number of countries\nexperiencing outbreaks"
        ) +
        theme_light(base_size = 16)

    return(rate_tune_plot)
}

