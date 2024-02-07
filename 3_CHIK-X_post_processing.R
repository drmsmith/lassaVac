library("tidyverse")
library("ggplot2")

# every file is a list of three components 
## v_daily_infectious_ppl
## df_res_curve
## df_res_summary

################
# read in data #
################

# res_dir <- "res/imp_mod_2023_suit_incidence"
### slower, lower median 
res_dir <- "res/importation_model_parallelised" ## old results 
# higher median, more spread 
# res_dir = './res/faster_spread'     # save results in this dir 

# get all file paths
all_files <- list.files(res_dir, "RDS", full.names = T) %>%
    str_sort(numeric = TRUE) # %>% .[1:20]



#############################
# all simulations in one df #
#############################

df_all_sims_long <- map(all_files, function(dirname) {
    res <- readRDS(dirname)
    map(res, function(.ls_sim) .ls_sim$df_res_curve) %>% bind_rows()
}, .progress = T) %>% bind_rows()

# head(df_all_sims_long)
# dim(df_all_sims_long)

# save 
# write.csv(df_all_sims_long, "res/import_model_sim_res_x100.csv", row.names = F)


####################
# summaries in one #
####################

df_all_sims_sum <- map(all_files, function(dirname) {
    res <- readRDS(dirname)
    map(res, function(.ls_sim) .ls_sim$df_res_summary) %>% bind_rows()
}, .progress = T) %>% bind_rows()

# head(df_all_sims_long)
# dim(df_all_sims_long)

# save 
# write.csv(df_all_sims_sum, "res/import_model_sim_summary_x100.csv", row.names = F)




#### high-level summary 

# how many countries appear in the simulations 
df_all_sims_sum$code %>% unique %>% length
# total number of simulations 
df_all_sims_sum$simulation %>% unique %>% length
# how many countries per simulation 
counts = df_all_sims_sum %>% group_by(simulation) %>% count()
counts %>% print(n=100)

median(counts$n) %>% unlist %>% median

############## plot ####
########################
# spread ~ m countries # 
########################

# scatter of number of countries spread to
n_spread <- data.frame(unlist(table(df_all_sims_sum$simulation)))
colnames(n_spread) <- c("simulation", "n_countries")

n_spread %>%
    arrange(n_countries) %>%
    ggplot(aes(x = simulation, y = n_countries), alpha = 0.75) +
    geom_point() +
    theme_light() +
    scale_x_discrete(
        breaks = seq(0, 100, by = 5), labels = seq(0, 100, by = 5)
    ) +
    labs(x = "Simulation", y = "Number of countries")


# histogram of outbreak chain lengths
n_spread %>%
    arrange(n_countries) %>%
    ggplot(aes(x = n_countries)) +
    geom_histogram(fill = "#547dbf", alpha = 0.9, binwidth = 1) +
    theme_light() +
    labs(y = "Frequency", x = "Number of countries")


####### plot ####
#################
# spread ~ time #
#################

cepi_prim_cols <- c(
    "#547dbf", "#ffa500", "#db4437", "#9d0f55", "#682860", "#0080A0", "#F9DF79"
)
# timing of spread per region
merge(df_all_sims_sum, n_spread, by = "simulation") %>%
    mutate(simulation = factor(simulation)) %>%
    ggplot(aes(x = timing / 365, y = n_countries)) +
    geom_point(aes(color = simulation), size = 2, alpha = 0.5) + 
    # geom_point(aes(color = region_code,  size = 1/n_countries),alpha = 0.5) + 
    scale_color_manual(
        values = make_cepi_base_col_scheme_col_out(cepi_prim_cols, 100),
        name = "Simulation ID"
    ) +
    geom_vline(
        xintercept = 100/365, col='#c73e32', 
        linewidth=2, linetype='dashed', alpha=0.9
        ) + 
    geom_vline(
        xintercept = 160/365, col='#c73e32', 
        linewidth=2, linetype='dotted', alpha=0.9
        ) + 
    theme_light(base_size = 22) +
    #    facet_wrap(vars(region_code)) +
    labs(x = "Outbreak start (year)", y = "Number of countries affected") +
    xlim(0, 2) +
    # ylim(0, 100) #+
    guides(color='none')

# 'figs/spread_timing_higher_m40.png'
ggsave(
    filename='figs/spread_timing_zika_matched.png', dpi=330, 
    width=3000, height=2000, units='px')


colnames(df_all_sims_sum)


#######################################################
# get summary of years 1, 2, 1+2 and total infections #
#######################################################

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

df_summary_by_year[1:4,]

######################
# add duration info  #
######################
# (also optionally the 
# pop affected parameter (randomly sampled) 
# or the paho curve id)

df_full_summary = df_all_sims_sum %>%
    group_by(
        simulation, country, code, region_name, region_code, 
        pop_size, timing
        ) %>%
        select(
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


df_full_summary[1:4,]


nrow(df_summary_by_year) == nrow(df_full_summary)


# write.csv(df_full_summary, "res/import_model_100_sim_full_summary.csv", row.names = F)




df_all_sims_long %>% filter(simulation==8) %>% .$country %>% unique

df_all_sims_sum %>% filter(simulation==8) %>% 
    mutate(country = factor(
        country, levels=timing
        )) %>% arrange(timing) %>% 
    ggplot(aes(x=timing, y=country, label=country)) +
    geom_line() + 
    geom_text(aes(col=log(IncCumul_U_final))) + 
    theme_light() +
    scale_color_gradient2(
        low = "#ffa500",
        mid = "gray",
        high = "#0080A0", # can optionally use muted()
        midpoint = mean(log(df_all_sims_long$IncCumul_U_final)),
        name = "log outbreak size"
    ) +
    labs(x='Outbreak start time', y='Country')

df_all_sims_sum %>% filter(simulation==8) %>% 
    select(simulation, timing, country) %>% head

df_sim_8 = df_all_sims_long %>% filter(simulation==8)

df_sim_8 %>%  
    group_by(country) %>% filter(time_days == timing) %>%
    select(simulation, time_days, timing, country, daily_infections_sim) %>% head


df_all_sims_sum %>% filter(simulation==8)  %>% filter(duplicated(timing))

dupl_df[1,'timing'] == dupl_df[2,'timing']

v_timings = df_sim_8$timing %>% unique

length(v_timings)

df_sim_8$country %>% unique %>% length
v_timings[duplicated(v_timings)]

df_sim_8 %>% group_by(country) %>% summarise(t_start = unique(timing), .groups = 'keep') %>% 
    arrange(t_start) %>% print(n=61)
# 375 is duplicated 

#######################################
#### plot one simulation over time ####
#######################################


sub_sim = df_all_sims_long %>% filter(simulation == 98) %>%
    mutate(country = factor(country))
ncolrs = length(unique(sub_sim$country))


ggplot(sub_sim, aes(time_years, daily_infections_sim)) + 
    geom_point(aes(time_years, daily_infections_sim, color=country), alpha=0.5) + 
    scale_color_manual(
        values = make_cepi_base_col_scheme_col_out(cepi_prim_cols, ncolrs),
        name = "Simulation ID"
    ) +
    geom_vline(
        xintercept = 100/365, col='#c73e32', 
        linewidth=2, linetype='dashed', alpha=0.9
        ) + 
    geom_vline(
        xintercept = 160/365, col='#c73e32', 
        linewidth=2, linetype='dotted', alpha=0.9
        ) + 
    theme_light(base_size = 22) + 
    xlim(0,4) + 
    labs(x = "Time / years", y = "Daily incidence") +
    guides(color='none')

ggsave(
    filename='figs/simulation_98.png', dpi=330, 
    width=3000, height=2000, units='px')

print(counts, n=100)

