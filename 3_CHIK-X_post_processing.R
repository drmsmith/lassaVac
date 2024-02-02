library("tidyverse")
library("ggplot2")

# every file is a list of three components 
## v_daily_infectious_ppl
## df_res_curve
## df_res_summary

################
# read in data #
################

res_dir <- "res/importation_model_parallelised"
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
df_all_sims_sum %>% group_by(simulation) %>% count() %>% print(n=100)

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

# timing of spread per region
merge(df_all_sims_sum, n_spread, by = "simulation") %>%
    ggplot(aes(x = timing, y = simulation)) +
    geom_point(aes(color = n_countries), alpha = 0.5, size = 1) +
    theme_light() +
    scale_color_gradient2(
        low = "#ffa500",
        mid = "gray",
        high = "#0080A0", # can optionally use muted()
        midpoint = 35,
        name = "n countries\naffected"
    ) +
    facet_wrap(vars(region_code)) +
    labs(x = "Outbreak start (days)", y = "Simulation ID")




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
