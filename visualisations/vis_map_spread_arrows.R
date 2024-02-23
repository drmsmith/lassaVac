library("tidyverse")
library("ggplot2")
library("sf")
library("rgdal")
library("rnaturalearth")
library("rnaturalearthdata")
library("conflicted")
library("countrycode")
library("ggtextures")
library("ggpattern")
library("grid")
library("magick", quietly = T, warn.conflicts = F)

conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    .quiet = T
)


source('visualisations/utils_vis.R')
load("figs/map_ws.RData")

# CHIK-X simulation results and summaries 
# df_all_sims_long <- read.csv("res/import_model_sim_res_x100.csv")
# df_results_summary <- read.csv('res/import_model_sim_summary_x100.csv')
# df_full_summary <- read.csv('res/import_model_100_sim_full_summary.csv')

main_dir <- 'res/baseline_inc_0.7_noise_0.1'

df_results_summary <- readRDS(file.path(main_dir, 'sim_summary.RDS'))
df_full_summary <- readRDS(file.path(main_dir, 'sim_full_summary.RDS'))

# helper files
df_burden = read.csv('data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv') 

df_burden$annual_incidence <- df_burden$annual_incidence*0.7

# helper files
# df_burden = read.csv('data/df_suit_means_pop_wght_pop_size_who_p_spillover.csv') 
# df_iso_regions = read.csv('methods/misc/ISO-3166-Countries-with-Regional-Codes.csv')
# df_who_regions = read.csv('methods/misc/df_countries_who_regions_codes.csv')




# add 0 for country not being in 
# and NA for no travel info 
world <- get_worldmap() # takes a minute and throws warnings 

# this gives how frequently a given country appears in the simulations 
# p spillover and pop_size 
percent_pop_size_spillover = df_full_summary %>% 
    group_by(country, code, region_name, region_code) %>%
    count(region_name, code) %>% # group_by(region_name)
    left_join(
        df_burden[,c('code', 'pop_size', 'p_spillover')], 
        by = join_by(code == code)
        ) %>% rename(percent_sims = n)


# take means, medians, and IQRs across different categories
## full duration or duration cut at 2yrs per outbreak 
## cumulative infections and infections per 100k population
df_sum_stats <- df_full_summary %>% 
    arrange(region_name, country) %>% # select(region_name, country, total_infections_all_years)
    mutate(
        country = factor(country, levels=unique(country)), 
        per_100k = 1e5*total_infections_all_years/pop_size
        # per_100k_2yrs = 1e5*years_1_2/pop_size
        ) %>%
    group_by(country, code, region_name, region_code) %>%
    summarise(
        # pop_size = unique(pop_size),
        # total cumulative mean, median, IQR 
        mean_cumul_infections = mean(total_infections_all_years),
        median_cumul_infections = median(total_infections_all_years),
        q1_cumul_infections = quantile(total_infections_all_years, 0.25),
        q3_cumul_infections = quantile(total_infections_all_years, 0.75),
        mean_cumul_infections_per100k = mean(per_100k),
        median_cumul_infections_per100k  = median(per_100k),
        q1_cumul_infections_per100k = quantile(per_100k, 0.25),
        q3_cumul_infections_per100k = quantile(per_100k, 0.75),
        .groups='keep'
    ) %>%      
    left_join(  # join with p_spillover, percent_sim, and ppp_size
        percent_pop_size_spillover[,c('code', 'pop_size', 'p_spillover', 'percent_sims')], by = join_by(code == code)
        ) %>%
    ungroup 

# add 0 for v_shape_pop_burden_mobil
# countries that are in simulations but do not have outbreaks 
##### this is particularly important for maps!!!
v_add_zeros = setdiff(df_burden$code, df_sum_stats$code)

df_zeros_NAs = df_burden %>% 
    filter(code %in% v_add_zeros) %>% 
    # rename(country = country_name, code = country_code) %>% 
    select(country, code, region_name, region_code, pop_size, p_spillover) %>%
    mutate(percent_sims = 0)


# complete summary for map plotting 
df_sum_stats_zeros_NAs = bind_rows(df_sum_stats, df_zeros_NAs)
# optionally set 0 for NAs if needed
df_sum_stats_zeros_NAs[is.na(df_sum_stats_zeros_NAs)] = 0 
ls_sum_stats <- df_sum_stats_zeros_NAs %>% group_by(region_code) %>% group_split()


# how to format colour scale legends??
colnames(df_sum_stats_zeros_NAs)
apply(df_sum_stats_zeros_NAs, 2, max) %>% 
    as.numeric %>% 
    .[!(is.na(.))] %>%
    format(digits=3, scientific=T)





dest_dir <- file.path("figs", "outbreak_progression")
if (!dir.exists(dest_dir)) dir.create(dest_dir)



n_spread <- data.frame(unlist(table(df_results_summary$simulation)))
colnames(n_spread) <- c("simulation", "n_countries")
n_spread

sim_no <- 78 # 94

df_sim = df_full_summary %>% filter(simulation==sim_no) %>% arrange(outbreak_start_day) %>%
    mutate(
        per_100k = 1e5*total_infections_all_years/pop_size#, 
        # per_100k_2yrs = 1e5*years_1_2/pop_size
    )


wrld_joined_full <- left_join(
    world, df_sim,
    by = join_by(adm0_a3 == code)
) # %>% filter(region_code == 'AFR')


wrld_joined_nas <- left_join(
    world, df_burden,
    by = join_by(adm0_a3 == code)
) %>% filter(!complete.cases(pop_size))

# get data set without current region
wrld_fltrd <- filter(wrld_joined_full, !code %in% ccodes_filter)


ccodes_filter <- df_sim$code


cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
guide_lab <- "Infections\nper 100,000"



outbr_map <- ggplot(data = wrld_joined_full) +
    geom_sf(aes(fill = per_100k), color = "darkgrey", linewidth = 0.2) +
    geom_sf(
        data = wrld_fltrd, fill = "gainsboro", color = NA 
    ) +
    scale_fill_gradientn(
        colors = cepi_cols,
        na.value = "gray25", # "lightgray",
        name = guide_lab
    ) +
    # geom_sf_pattern(
    #     data=wrld_joined_nas, color = "darkgrey", 
    #     linewidth = 0.2, 
    #     pattern = 'stripe', pattern_density = 0.15, 
    #     pattern_fill = 'gray33', pattern_colour = 'darkgrey', 
    #     pattern_spacing = 0.01
    # ) + 
    theme_light(base_size = 16) +
    guides(color = "none") +
    coord_sf(expand=FALSE)


wrld_fltrd_outbreaks <- filter(wrld_joined_full, code %in% ccodes_filter)
df_coords <- map(wrld_fltrd_outbreaks$geometry, st_centroid) %>%
    map(~c(lon=.x[1], lat=.x[2])) %>% bind_rows

start_point <- st_centroid(wrld_start$geometry)

source_dest <- data.frame(
    start = wrld_fltrd_outbreaks$code, 
    dest = dplyr::lead(wrld_fltrd_outbreaks$code, 1)
    ) %>% drop_na

start_dest_coords <- cbind(
    df_coords, 
    lead(df_coords)
) %>% drop_na
names(start_dest_coords) <- c('start_lon', 'start_lat', 'dest_lon', 'dest_lat')

df_start_dest_code_coords <- cbind(source_dest, start_dest_coords)

df_start_dest_code_coords$curvature <- runif(
    nrow(df_start_dest_code_coords), -0.2, 0.2
    )

# 
# annotations <- tibble(
#     arrow_from = paste(
#     df_start_dest_code_coords$start_lon, 
#     df_start_dest_code_coords$start_lat,
#     sep=', '
#     ),
#     arrow_to = paste(
#     df_start_dest_code_coords$dest_lon, 
#     df_start_dest_code_coords$dest_lat,
#     sep=', '
#     ),
#     curvature = runif(nrow(df_start_dest_code_coords), -0.2, 0.2)
# ) %>%
#     separate(arrow_from, into = c("x", "y")) %>%
#     separate(arrow_to, into = c("xend", "yend")) 
# 


df_start_dest_code_coords %>% tibble %>%
    pwalk(function(...) {
        # collect all values in the row in a one-rowed data frame
        current <- tibble(...)


        # update the plot object with global assignment
        outbr_map <<- outbr_map +
            # for each annotation, add an arrow
            geom_curve(
                data = current,
                aes(
                    x = start_lon, xend = dest_lon,
                    y = start_lat, yend = dest_lat
                ),
                # that's the whole point of doing this loop:
                curvature = current %>% pull(curvature),
                linewidth = 0.4,
                arrow = arrow(
                    length = unit(0.02, "npc")
                )
                
            )
    })

outbr_map <- outbr_map + labs(x='', y='')

ggsave(
    plot=outbr_map, filename="figs/arrow_map.png", width = 4400, height = 2000, 
    units='px', bg='transparent', dpi=330
    )




########################
# infections per 100k  #
########################

walk2(n_spread$simulation, n_spread$n_countries, function(.simid, .nspread) {
    
    df_sim = df_full_summary %>% 
        filter(simulation==.simid) %>% arrange(outbreak_start_day) %>%
        mutate(per_100k = 1e5*total_infections_all_years/pop_size)
    
    wrld_joined_full <- left_join(
        world, df_sim,
        by = join_by(adm0_a3 == code)
    ) 
    
    cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
    guide_lab <- "Infections\nper 100,000"
    
    ccodes_filter <- df_sim$code
    # get data set without outbreak countires
    wrld_fltrd <- filter(wrld_joined_full, !code %in% ccodes_filter)
    # get data set with only outbreak countries 
    wrld_fltrd_outbreaks <- filter(wrld_joined_full, code %in% ccodes_filter)
    df_coords <- map(wrld_fltrd_outbreaks$geometry, st_centroid) %>%
        map(~c(lon=.x[1], lat=.x[2])) %>% bind_rows
    
    
    outbr_map <- ggplot(data = wrld_joined_full) +
        geom_sf(aes(fill = per_100k), color = "darkgrey", linewidth = 0.2) +
        geom_sf(
            data = wrld_fltrd, fill = "gainsboro", color = NA 
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        coord_sf(expand=FALSE)
    

    
    source_dest <- data.frame(
        start = wrld_fltrd_outbreaks$code, 
        dest = dplyr::lead(wrld_fltrd_outbreaks$code, 1)
    ) %>% drop_na
    
    start_dest_coords <- cbind(
        df_coords, 
        lead(df_coords)
    ) %>% drop_na
    names(start_dest_coords) <- c('start_lon', 'start_lat', 'dest_lon', 'dest_lat')
    
    df_start_dest_code_coords <- cbind(source_dest, start_dest_coords)
    
    df_start_dest_code_coords$curvature <- runif(
        nrow(df_start_dest_code_coords), -0.2, 0.2
    )
    
    
    df_start_dest_code_coords %>% tibble %>%
        pwalk(function(...) {
            # collect all values in the row in a one-rowed data frame
            current <- tibble(...)
            
            
            # update the plot object with global assignment
            outbr_map <<- outbr_map +
                # for each annotation, add an arrow
                geom_curve(
                    data = current,
                    aes(
                        x = start_lon, xend = dest_lon,
                        y = start_lat, yend = dest_lat
                    ),
                    # that's the whole point of doing this loop:
                    curvature = current %>% pull(curvature),
                    linewidth = 0.4,
                    arrow = arrow(
                        length = unit(0.02, "npc")
                    )
                    
                )
        })
    
    outbr_map <- outbr_map + labs(x='', y='')
    fname <- file.path(
        "figs/outbr_arrows", 
        paste0("arrow_map_sim_", .simid,"_x", .nspread, ".png")
        )
    
    ggsave(
        plot=outbr_map, filename=fname, width = 4400, height = 1700, 
        units='px', bg='transparent', dpi=330
    )
}, .progress=T)





########################
# raw infection totals #
########################
walk2(n_spread$simulation, n_spread$n_countries, function(.simid, .nspread) {
    
    df_sim = df_full_summary %>% 
        filter(simulation==.simid) %>% arrange(outbreak_start_day) %>%
        mutate(per_100k = 1e5*total_infections_all_years/pop_size)
    
    wrld_joined_full <- left_join(
        world, df_sim,
        by = join_by(adm0_a3 == code)
    ) 
    
    
    cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
    guide_lab <- "Infections"
    
    ccodes_filter <- df_sim$code
    # get data set without outbreak countires
    wrld_fltrd <- filter(wrld_joined_full, !code %in% ccodes_filter)
    # get data set with only outbreak countries 
    wrld_fltrd_outbreaks <- filter(wrld_joined_full, code %in% ccodes_filter)
    df_coords <- map(wrld_fltrd_outbreaks$geometry, st_centroid) %>%
        map(~c(lon=.x[1], lat=.x[2])) %>% bind_rows
    
    
    outbr_map <- ggplot(data = wrld_joined_full) +
        geom_sf(
            aes(fill = total_infections_all_years), 
            color = "darkgrey", linewidth = 0.2
            ) +
        geom_sf(
            data = wrld_fltrd, fill = "gainsboro", color = NA 
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        coord_sf(expand=FALSE)
    
    
    source_dest <- data.frame(
        start = wrld_fltrd_outbreaks$code, 
        dest = dplyr::lead(wrld_fltrd_outbreaks$code, 1)
    ) %>% drop_na
    
    start_dest_coords <- cbind(
        df_coords, 
        lead(df_coords)
    ) %>% drop_na
    names(start_dest_coords) <- c('start_lon', 'start_lat', 'dest_lon', 'dest_lat')
    
    df_start_dest_code_coords <- cbind(source_dest, start_dest_coords)
    
    df_start_dest_code_coords$curvature <- runif(
        nrow(df_start_dest_code_coords), -0.2, 0.2
    )

    
    df_start_dest_code_coords %>% tibble %>%
        pwalk(function(...) {
            # collect all values in the row in a one-rowed data frame
            current <- tibble(...)
            
            
            # update the plot object with global assignment
            outbr_map <<- outbr_map +
                # for each annotation, add an arrow
                geom_curve(
                    data = current,
                    aes(
                        x = start_lon, xend = dest_lon,
                        y = start_lat, yend = dest_lat
                    ),
                    # that's the whole point of doing this loop:
                    curvature = current %>% pull(curvature),
                    linewidth = 0.4,
                    arrow = arrow(
                        length = unit(0.02, "npc")
                    )
                    
                )
        })
    
    outbr_map <- outbr_map + labs(x='', y='')
    fname <- file.path(
        "figs/outbr_arrows", 
        paste0("arrow_map_sim_", .simid,"_x", .nspread, ".png")
    )
    
    ggsave(
        plot=outbr_map, filename=fname, width = 4400, height = 1700, 
        units='px', bg='transparent', dpi=330
    )
}, .progress=T)

