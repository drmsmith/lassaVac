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

source('utils/utils_vis.R')
source("utils/utils_ks_test.R")
source("utils/utils_post_proc.R")

main_dir <- 'res/baseline_inc_0.7_noise_0.1'
incidence_factor <- 0.7
dest_dir <- file.path('figs','baseline_zika_and_hists')

df_all_sims_long <- readRDS(file.path(main_dir, "sim_res_long.RDS"))
df_results_summary <- readRDS(file.path(main_dir, 'sim_summary.RDS'))
df_full_summary <- readRDS(file.path(main_dir, 'sim_full_summary.RDS'))

# helper files
df_burden = read.csv('data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv') 
df_burden$annual_incidence <- df_burden$annual_incidence*incidence_factor

# get world map for plotting 
world <- get_worldmap() # takes a minute and throws warnings 




######################
# file preprocessing # 
######################

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
        ) %>%
    group_by(country, code, region_name, region_code) %>%
    summarise(
        percentage_appearance = n(),
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
        ) %>% ungroup 

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





n_spread <- data.frame(unlist(table(df_results_summary$simulation)))
colnames(n_spread) <- c("simulation", "n_countries")
n_spread

sim_no <- 1# 78 # 94

df_sim = df_full_summary %>% filter(simulation==sim_no) %>% arrange(outbreak_start_day) %>%
    mutate(
        per_100k = 1e5*total_infections_all_years/pop_size#, 
        # per_100k_2yrs = 1e5*years_1_2/pop_size
    )


wrld_joined_full <- left_join(
    world, df_sim,
    by = join_by(adm0_a3 == code)
)


wrld_joined_nas <- left_join(
    world, df_burden,
    by = join_by(adm0_a3 == code)
) %>% filter(!complete.cases(pop_size))

ccodes_filter <- df_sim$code
# get data set without current region
wrld_fltrd <- filter(wrld_joined_full, !adm0_a3 %in% ccodes_filter)



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

wrld_joined_right$adm0_a3 %>% sort
ccodes_filter %>% sort

world[world$adm0_a3=='VCT',]
wrld_joined_full[world$adm0_a3=='VCT',]
df_sim[df_sim$code=='VCT',]

wrld_fltrd_outbreaks <- filter(wrld_joined_full, adm0_a3 %in% ccodes_filter) %>% 
    arrange(outbreak_start_day) %>% filter(!is.na(outbreak_start_day))

df_coords <- map(wrld_fltrd_outbreaks$geometry, st_centroid) %>%
    map(~c(lon=.x[1], lat=.x[2])) %>% bind_rows

source_dest <- data.frame(
    start = ccodes_filter, 
    dest = dplyr::lead(ccodes_filter, 1)
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
                arrow = arrow(length = unit(0.02, "npc")),
                alpha=0.8
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

dest_dir <- file.path("figs", "maps_spread_arrows_infections_svg")
if (!dir.exists(dest_dir)) dir.create(dest_dir)

save_extension <- '.svg' # or '.svg'

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
    wrld_fltrd <- filter(wrld_joined_full, !adm0_a3 %in% ccodes_filter)
    # get data set with only outbreak countries 
    wrld_fltrd_outbreaks <- filter(wrld_joined_full, adm0_a3 %in% ccodes_filter) %>% 
        arrange(outbreak_start_day) %>% filter(!is.na(outbreak_start_day))
    df_coords <- map(wrld_fltrd_outbreaks$geometry, st_centroid) %>%
        map(~c(lon=.x[1], lat=.x[2])) %>% bind_rows
    
    
    outbr_map <- ggplot(data = wrld_joined_full) +
        geom_sf(
            aes(fill = per_100k), 
            color = "darkgrey", linewidth = 0.2
            ) +
        geom_sf(
            data = wrld_fltrd, fill = "gainsboro", color = NA 
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab, labels=comma
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        coord_sf(expand=FALSE)
       
        
    source_dest <- data.frame(
        start = ccodes_filter, 
        dest = dplyr::lead(ccodes_filter, 1)
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
                    arrow = arrow(length = unit(0.02, "npc")), 
                    alpha = 0.8
                    
                )
        })
    
    outbr_map <- outbr_map + labs(x='', y='')
    fname <- file.path(
        dest_dir, 
        paste0("arrow_map_sim_", .simid,"_x", .nspread, save_extension)
        )
    
    ggsave(
        plot=outbr_map, filename=fname, width = 4400, height = 1700, 
        units='px', bg='transparent', dpi=330
    )
}, .progress=T)





########################
# raw infection totals #
########################

dest_dir <- file.path("figs", "maps_spread_arrows_per100k_svg")
if (!dir.exists(dest_dir)) dir.create(dest_dir)

save_extension <- '.svg' # or '.svg'

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
    wrld_fltrd <- filter(wrld_joined_full, !adm0_a3 %in% ccodes_filter)
    # get data set with only outbreak countries 
    wrld_fltrd_outbreaks <- filter(wrld_joined_full, adm0_a3 %in% ccodes_filter) %>% 
        arrange(outbreak_start_day) %>% filter(!is.na(outbreak_start_day))
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
            name = guide_lab, labels=comma
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        coord_sf(expand=FALSE)


    source_dest <- data.frame(
        start = ccodes_filter, 
        dest = dplyr::lead(ccodes_filter, 1)
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
                    arrow = arrow(length = unit(0.02, "npc")),
                    alpha = 0.8
                )
        })
    
    outbr_map <- outbr_map + labs(x='', y='')
    fname <- file.path(
        dest_dir, 
        paste0("arrow_map_sim_", .simid,"_x", .nspread, save_extension)
    )
    
    ggsave(
        plot=outbr_map, filename=fname, width = 4400, height = 1700, 
        units='px', bg='transparent', dpi=330
    )
}, .progress=T)


















#############################
# OUTBREAK PROGRESSION GIFS # 
#############################


# check n countries spread 
n_spread <- data.frame(unlist(table(df_results_summary$simulation)))
colnames(n_spread) <- c("simulation", "n_countries")
n_spread


dest_dir <- file.path("figs", "outbreak_progression")
if (!dir.exists(dest_dir)) dir.create(dest_dir)

# filter simulation data 
sim_no <- 94
df_sim = df_full_summary %>% filter(simulation==sim_no) %>% arrange(outbreak_start_day) %>%
    mutate(
        per_100k = 1e5*total_infections_all_years/pop_size
    )


# full map for plotting 
wrld_joined_full <- left_join(
    world, df_sim,
    by = join_by(adm0_a3 == code)
)

# add NAs 
wrld_joined_nas <- left_join(
    world, df_burden,
    by = join_by(adm0_a3 == code)
) %>% filter(!complete.cases(pop_size))


# loop over every stage of the map and add 
outbr_map <- gg_map_outbreak_progress(
    .wrld_joined_full=wrld_joined_full,
    .wrld_joined_nas=wrld_joined_nas, 
    .metric='per_100k', # total_infections_all_years
    .filter_codes=ccodes_filter,
    .grob=grob,
    .start_day=start_day,
    .dest_dir = dest_dir
    )

 

gif_maker = function(
    vec_img_paths,  # vector of full paths to desired images in correct order
    file_name = 'plot_gif', # how to name the gif
    dest_dir = getwd(),     # directory to save gif in 
    .fps=2) {               # frames per second (multiple of 100)
    # read all images into a list  
    img_list <- lapply(vec_img_paths, image_read)
    # join images 
    img_joined <- image_join(img_list)
    # animate at 2 frames per second by default
    img_animated <- image_animate(img_joined, fps = .fps)
    ## save to disk
    img_path = paste0(dest_dir, '/', file_name, '.gif')
    image_write(image = img_animated, path = img_path)
    gc()
    return(img_animated)
}


source('utils/utils_model.R')
all_filepaths = list.files(dest_dir, full.names = T)
gif_maker(all_filepaths, "outbreak_progression", dest_dir, .fps = 1)
