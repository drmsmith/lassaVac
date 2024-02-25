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
library("ggh4x")
library("magick", quietly = T, warn.conflicts = F)

conflicts_prefer(
    dplyr::select(),
    dplyr::filter(),
    .quiet = T
)


source('visualisations/utils_vis.R')
# load("figs/map_ws.RData")

# CHIK-X simulation results and summaries 
# df_all_sims_long <- read.csv("res/import_model_sim_res_x100.csv")
# df_results_summary <- read.csv('res/import_model_sim_summary_x100.csv')
# df_full_summary <- read.csv('res/import_model_100_sim_full_summary.csv')

main_dir <- 'res/baseline_inc_0.7_noise_0.1'

df_all_sims_long <- readRDS(file.path(main_dir, "sim_res_long.RDS"))
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


#### outbreak progression 

df_full_summary %>% group_by(simulation) %>% count %>% arrange(n) %>% print(n=100)

outbrk_data <- df_all_sims_long %>% filter(simulation==88) %>% arrange(timing)
outbrk_data$country <- factor(outbrk_data$country, levels=unique(outbrk_data$country))

df_timing = outbrk_data[,c('timing', 'country')] %>% unique
df_timing$timing = df_timing$timing/365.25

colnames(outbrk_data)
head(outbrk_data)
cepi_prim_cols <- c(
    "#547dbf", "#ffa500", "#db4437", "#9d0f55", "#682860", "#0080A0", "#F9DF79"
)
nclrs <- length(unique(outbrk_data$country))
v_cols <- sample(colorRampPalette(cepi_prim_cols)(nclrs), nclrs)

# plot daily cases 
outbreak_plts <- ggplot(outbrk_data, aes(x=time_years, y=daily_infections_sim)) +
    facet_wrap(vars(country), scales = "free_y", ncol = 4) + # country or code
    geom_point(aes(col = country), size = 0.3) +
    geom_line(aes(col = country), linewidth=0.5) +
    geom_vline(
        data=df_timing, aes(xintercept = timing), 
        color="gainsboro", linetype='dashed', lwd=0.8) + 
    scale_color_manual(values = v_cols) +
    theme_light() +
    theme(legend.position = "none") + 
    labs(x='Time (years)', y='Daily cases') + 
    coord_cartesian(expand=FALSE)
    
# get all facet ylims 
df_ylims <- map(ggplot_build(outbreak_plts)$layout$panel_params, ~data.frame(ymin=0, ymax=.x$y.range[2])) %>% bind_rows
# create new scales for positions 
position_scales <- pmap(df_ylims, function(ymin, ymax) {
    return(scale_y_continuous(labels = comma, limits = c(ymin, ymax*1.1)))
})
# use the ggh4x package func to adjust all facet wrap scales 
outbreak_plts <- outbreak_plts + facetted_pos_scales(y = position_scales)



ggsave(outbreak_plts,
    filename = 'figs/single_sim_outbreak_data.png', dpi=330, bg='transparent', 
    width = 10, height = 6, units = 'in'
)
ggsave(outbreak_plts,
    filename = 'figs/single_sim_outbreak_data.svg', dpi=330, bg='transparent', 
    width = 10, height = 6, units = 'in'
)


#### plotting ideas 
### average over simulations 
## cutoff at 2 years ??
# map: mean cumulative infections for map 
# map: mean per 100k pop across 100 sims 
# barplot: median + IQR cumulative incidence   
# map: population size, p_spillover
# map: % sim in which country is present  // spillover incidence 
# scatter: spillover ~ pop size (or suitability)


# spillover and pop size from burden 
# country in % sim 

spread_cumul_timing_100 <- df_full_summary %>%
    filter(outbreak_start_day <= 100) %>%
    group_by(simulation) %>%
    count() %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation), 
        day = 'day 100'
        )

spread_cumul_timing_160 <- df_full_summary %>%
    filter(outbreak_start_day <= 160) %>%
    group_by(simulation) %>%
    count() %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation), 
        day = 'day 160'
        )

spread_cumul_timing_hists <- bind_rows(spread_cumul_timing_100, spread_cumul_timing_160)

get_plot_limits <- function(plot) {
    gb = ggplot_build(plot)
    xmin = gb$layout$panel_params[[1]]$x.range[1]
    xmax = gb$layout$panel_params[[1]]$x.range[2]
    ymin = gb$layout$panel_params[[1]]$y.range[1]
    ymax = gb$layout$panel_params[[1]]$y.range[2]
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}


incid_scen_n_by_day <- ggplot(
    spread_cumul_timing_hists,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~day, ncol=2) + 
    geom_histogram(aes(fill=day), binwidth=1, boundary = 0.5) +
    scale_fill_manual(
        values=c("#007492", "#e89600")) + # name='Number of\ncountries\naffected by' 
    labs(
        x = "Number of countries experiencing outbreaks", 
        y = "Count"
    ) +
    guides(fill='none') + 
    theme_light(base_size = 14) +
    coord_cartesian(expand=FALSE) +  
    scale_x_continuous(breaks=pretty_breaks(10)) + 
    xlim(0,7) 
    
incid_scen_n_by_day <- incid_scen_n_by_day + ylim(0, get_plot_limits(incid_scen_n_by_day)$ymax * 1.05)

ggsave(incid_scen_n_by_day,
    filename=file.path('figs', 'hists_100_160_baseline.png'), dpi=330, 
    width=3000, height=2000, units='px')

ggsave(incid_scen_n_by_day,
    filename=file.path('figs', 'hists_100_160_baseline.svg'), dpi=330, 
    width=3000, height=2000, units='px')




spread_cumul_timing <- df_full_summary %>%
    group_by(simulation) %>%
    add_count(code) %>%
    mutate(
        cumul_nspread = cumsum(n),
        simulation = factor(simulation)
    )


wider_cumul_spread <- spread_cumul_timing %>% 
    group_by(simulation) %>%
    make_wider_cumul_spread() 

# subset and summarise cdf for test 
sum_cumul_spread <- wider_cumul_spread %>% 
        mean_median_trajectory()
        

ncols = 1

# prepare zika baseline 
df_zika_cumul <- make_df_zika_cumul()

rate_tune_plot <- ggplot(
    spread_cumul_timing,
    aes(x = outbreak_start_day / 365, y = cumul_nspread)
    ) +
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
    
rate_tune_plot <- rate_tune_plot + ylim(0, get_plot_limits(rate_tune_plot)$ymax * 1.05)
rate_tune_plot


ggsave(rate_tune_plot,
    filename=file.path('figs', 'baseline_zika_spread.png'), dpi=330, 
    width=2000, height=1500, units='px')

ggsave(rate_tune_plot,
    filename=file.path('figs', 'baseline_zika_spread.svg'), dpi=330, 
    width=2000, height=1500, units='px')



baseline_plots = plot_grid(rate_tune_plot,incid_scen_n_by_day, labels = "AUTO", ncol=2) 
baseline_plots

ggsave(baseline_plots,
    filename=file.path('figs', 'zika_spread_and_histograms.png'), dpi=330, 
    width=3300, height=1700, units='px')

ggsave(baseline_plots,
    filename=file.path('figs', 'zika_spread_and_histograms.svg'), dpi=330, 
    width=3300, height=1700, units='px')




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
        percentage_appearance = n(),
        mean_cumul_infections = mean(total_infections_all_years),
        median_cumul_infections = median(total_infections_all_years),
        q1_cumul_infections = quantile(total_infections_all_years, 0.25),
        q3_cumul_infections = quantile(total_infections_all_years, 0.75),
        # iqr_cumul_infections = IQR(total_infections_all_years),
        # cumulative mean, median, IQR for first 2 years 
        # mean_cumul_infections_2yrs = mean(years_1_2),
        # median_cumul_infections_2yrs = median(years_1_2),
        # q1_cumul_infections_2yrs = quantile(years_1_2, 0.25),
        # q3_cumul_infections_2yrs = quantile(years_1_2, 0.75),
        # # total cumulative mean, median, IQR per 100k pop
        mean_cumul_infections_per100k = mean(per_100k),
        median_cumul_infections_per100k  = median(per_100k),
        q1_cumul_infections_per100k = quantile(per_100k, 0.25),
        q3_cumul_infections_per100k = quantile(per_100k, 0.75),
        # cumulative mean, median, IQR for first 2 years per 100k pop 
        # mean_cumul_infections_2yrs_per100k = mean(per_100k_2yrs),
        # median_cumul_infections_2yrs_per100k = median(per_100k_2yrs),
        # q1_cumul_infections_2yrs_per100k = quantile(per_100k_2yrs, 0.25),
        # q3_cumul_infections_2yrs_per100k = quantile(per_100k_2yrs, 0.75),
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


# how to format colour scale legends??
colnames(df_sum_stats_zeros_NAs)
apply(df_sum_stats_zeros_NAs, 2, max) %>% 
    as.numeric %>% 
    .[!(is.na(.))] %>%
    format(digits=3, scientific=T)


### add 0s for countries where NO OUTBREAK // for percent  
### and NA for countries with NO DATA  --> v_nas 
############ impossible due to lack of who region data 
# use na.value = 'gray' in scale_color/fill 


# save.image('figs/map_ws.RData')


##################################
#### BAR PLOTS: MEDIAN +- IQR ####
##################################

cepi_prim_cols <- c(
    "#547dbf", "#ffa500", "#db4437", "#9d0f55", "#682860", "#0080A0", "#F9DF79"
)


all_metrics <- c(
    "median_cumul_infections",
    "median_cumul_infections_per100k"
    # "median_cumul_infections_2yrs",
    # "median_cumul_infections_2yrs_per100k"
    )


region_codes <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")

names(cepi_prim_cols) <- region_codes


barplotdir <- "figs/barplots"
walk(all_metrics, function(.curr_metric) {
    walk(region_codes, function(.reg_code) {
        dest_dir = barplotdir
        # dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
        if (!dir.exists(dest_dir)) dir.create(dest_dir)
        # run plotting funcs
        fill_col <- cepi_prim_cols[.reg_code]
        gg_metrics_barplot_region_col_sort(
            .data = df_sum_stats, 
            .cols_for_scheme = cepi_prim_cols, 
            .region_code = .reg_code, 
            .fill_col = fill_col, 
            .metric = .curr_metric,
            .dest_dir = dest_dir
        )
    })
}, .progress = T)





barplotdir <- "figs/barplots"
walk(all_metrics, function(.curr_metric) {
    walk(region_codes, function(.reg_code) {
        dest_dir = barplotdir
        # dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
        if (!dir.exists(dest_dir)) dir.create(dest_dir)
        # run plotting funcs
        gg_metrics_barplot(
            .data = df_sum_stats, 
            .cols_for_scheme = cepi_prim_cols, 
            .region_code = .reg_code, 
            .metric = .curr_metric,
            .dest_dir = dest_dir
        )
    })
}, .progress = T)


# df_by_region <- df_full_summary %>% group_by(region_code, region_name) %>% 
#     mutate(
#         region_code = factor(region_code, levels=unique(region_code)), 
#         per_100k = 1e5*total_infections_all_years/pop_size, 
#         per_100k_2yrs = 1e5*years_1_2/pop_size
#         ) %>% 
#         summarise(
#             region_totals = median(total_infections_all_years),
#             region_totals_2yrs = median(years_1_2),
#             region_totals_per100k = median(per_100k),
#             region_totals_per100k_2yrs = median(per_100k_2yrs),
#             q1_region_totals = quantile(total_infections_all_years, 0.25),
#             q3_region_totals = quantile(total_infections_all_years, 0.75),
#             q1_region_totals_2yrs = quantile(years_1_2, 0.25),
#             q3_region_totals_2yrs = quantile(years_1_2, 0.75),
#             q1_region_totals_per100k = quantile(per_100k, 0.25),
#             q3_region_totals_per100k = quantile(per_100k, 0.75),
#             q1_region_totals_per100k_2yrs = quantile(per_100k_2yrs, 0.25),
#             q3_region_totals_per100k_2yrs = quantile(per_100k_2yrs, 0.75),
#             .groups='keep'
#         ) 


df_by_region <- df_full_summary %>% group_by(region_code, region_name) %>% 
    mutate(
        region_code = factor(region_code, levels=unique(region_code)), 
        per_100k = 1e5*total_infections_all_years/pop_size
        # per_100k_2yrs = 1e5*years_1_2/pop_size
        ) %>% 
        summarise(
            median_per100k = 1e5*median(total_infections_all_years) / sum(pop_size),
            q1_per100k = 1e5*quantile(total_infections_all_years, 0.25) / sum(pop_size),
            q3_per100k = 1e5*quantile(total_infections_all_years, 0.75) / sum(pop_size),
            region_totals = median(total_infections_all_years),
            # region_totals_2yrs = median(years_1_2),
            region_totals_per100k = median(per_100k),
            # region_totals_per100k_2yrs = median(per_100k_2yrs),
            q1_region_totals = quantile(total_infections_all_years, 0.25),
            q3_region_totals = quantile(total_infections_all_years, 0.75),
            # q1_region_totals_2yrs = quantile(years_1_2, 0.25),
            # q3_region_totals_2yrs = quantile(years_1_2, 0.75),
            q1_region_totals_per100k = quantile(per_100k, 0.25),
            q3_region_totals_per100k = quantile(per_100k, 0.75),
            # q1_region_totals_per100k_2yrs = quantile(per_100k_2yrs, 0.25),
            # q3_region_totals_per100k_2yrs = quantile(per_100k_2yrs, 0.75),
            .groups='keep'
        ) 




barplot(df_by_region$median_per_100k, names=region_codes)

metric='median_per100k'
ymin_lims = 'q1_per100k'
ymax_lims = 'q3_per100k'

# metric='region_totals'
# ymin_lims = 'q1_region_totals'
# ymax_lims = 'q3_region_totals'

guide_lab <- ifelse(
    str_detect(metric, "per100k"),
    "Infections per \n100,000 population",
    "Infections"
)

light2 = c("#8ca8d4","#ffc354","#e78279","#bd5e8d","#9a6f94","#54aabf","#fbeaa5")

light1 = c("#cad7eb","#ffe3b0","#f4c5c1","#e1b5ca","#d0bcce","#b0d8e2","#fdf5d5")


v_cols <- colorRampPalette(light2)(6) %>%
    unlist() %>%
    unname()

df_by_region %>%
    ggplot(aes(x = region_code, y = .data[[metric]])) +
    geom_bar(stat = "identity", aes(fill = region_code)) +
    geom_errorbar(
        aes(x = region_code, ymin = .data[[ymin_lims]], ymax = .data[[ymax_lims]]),
        width = 0.4,
        colour = "grey33",
        alpha = 0.9, linewidth = 1.3
    ) +
    scale_fill_manual(values = v_cols) +
    theme_light(base_size = 16) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
    guides(fill = "none") +
    labs(x = "", y = guide_lab)

dest_filename <- paste0(
    .dest_dir, "/", "region_", .metric, ".png",
    collapse = ""
)



########################
# all regions bar plot # 
########################

region_metrics <- c(
    'region_totals', 'region_totals_per100k'
    # 'region_totals_2yrs', 'region_totals_per100k_2yrs'
    )
dest_dir = "figs/barplots_region/"
# dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
if (!dir.exists(dest_dir)) dir.create(dest_dir)
walk(region_metrics, function(.curr_metric) {
    gg_metrics_barplot_region(.data=df_by_region, .metric = .curr_metric, .dest_dir = dest_dir)
})










########################
#### MAP: SPILLOVER ####
########################


### average over simulations?? 
## cutoff at 2 years 
# map: mean cumulative infections for map 
# map: mean per 100k pop across 100 sims 
# map: population size, p_spillover
# map: % sim in which country is present  // spillover incidence 


# get world map data for plotting 
world <- get_worldmap()

wrld_joined_full <- left_join(
    world, df_sum_stats_zeros_NAs,
    by = join_by(code == code) # adm0_a3  
) # %>% filter(region_code == 'AFR')



all_metrics <- c(
    "mean_cumul_infections",
    "median_cumul_infections",
    # "mean_cumul_infections_2yrs",
    # "median_cumul_infections_2yrs",
    "mean_cumul_infections_per100k",
    "median_cumul_infections_per100k",
    # "mean_cumul_infections_2yrs_per100k",
    # "median_cumul_infections_2yrs_per100k", 
    "pop_size", 
    "p_spillover", 
    "percent_sims"
)

region_codes <- c("AFR", "AMR", "EMR", "EUR", "SEAR", "WPR")



walk(all_metrics, function(.curr_metric) {
    walk(region_codes, function(.reg_code) {
        dest_dir <- paste0("figs/maps_region/", .reg_code, collapse = "")
        if (!dir.exists(dest_dir)) dir.create(dest_dir)
        # run plotting funcs
        gg_metrics_map(
            .wrld_joined_full = wrld_joined_full, 
            .region_code = .reg_code, 
            .metric = .curr_metric,
            .dest_dir = dest_dir
        )
    })
}, .progress = T)




# global maps

wrld_joined_full <- left_join(
    world, df_sum_stats_zeros_NAs,
    by = join_by(adm0_a3 == code)
) # %>% filter(region_code == 'AFR')


dest_dir <- "figs/maps_global"
# dest_dir <- paste0(dest_dir, .reg_code, collapse = "")
if (!dir.exists(dest_dir)) dir.create(dest_dir)


walk(all_metrics, function(.curr_metric) {
    # run plotting funcs
    gg_metrics_map_global(
        .wrld_joined_full = wrld_joined_full,
        .metric = .curr_metric,
        .dest_dir = dest_dir
    )
}, .progress = T)




# get simulation outbreak course 


# 64 # 90 is amazing 
# pick a simulation from n_spread 

wrld_joined_full <- left_join(
    world, df_sum_stats_zeros_NAs,
    by = join_by(code == code) # adm0_a3  
) # %>% filter(region_code == 'AFR')


n_spread <- data.frame(unlist(table(df_results_summary$simulation)))
colnames(n_spread) <- c("simulation", "n_countries")
n_spread

sim_no <- 94

df_sim = df_full_summary %>% filter(simulation==sim_no) %>% arrange(outbreak_start_day) %>%
    mutate(
        per_100k = 1e5*total_infections_all_years/pop_size#, 
        # per_100k_2yrs = 1e5*years_1_2/pop_size
    )


dest_dir <- file.path("figs", "outbreak_progression")
if (!dir.exists(dest_dir)) dir.create(dest_dir)

# walk(1:nrow(df_sim), function(.sim_index) {
#     ccodes_filter <- df_sim$code[1:.sim_index]
#     start_day <- df_sim$outbreak_start_day[.sim_index]
#     grob <- grobTree(
#         textGrob(
#             paste("Outbreak start day: ", start_day, sep=''), x=0.05,  y=0.1, hjust=0,
#             gp=gpar(col="black", fontsize=15, fontface="bold")
#             ))

#     gg_map_outbreak_progress(
#         .wrld_joined_full=wrld_joined_full,
#         .wrld_joined_nas=wrld_joined_nas, 
#         .metric='per_100k', #years_1_2
#         .filter_codes=ccodes_filter,
#         .grob=grob,
#         .start_day=start_day,
#         .dest_dir = dest_dir
#     )
# }, .progress = T)




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

load("figs/map_ws.RData")
source('visualisations/utils_vis.R')

dest_dir <- file.path("figs", "outbreak_progression")
if (!dir.exists(dest_dir)) dir.create(dest_dir)



n_spread <- data.frame(unlist(table(df_results_summary$simulation)))
colnames(n_spread) <- c("simulation", "n_countries")
n_spread

sim_no <- 94

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

ccodes_filter <- df_sim$code


start_day <- 999
grob <- grobTree(
    textGrob(
        paste("Outbreak start day: ", start_day, sep=''), x=0.05,  y=0.1, hjust=0,
        gp=gpar(col="black", fontsize=15, fontface="bold")
        ))

cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
guide_lab <- "Infections\nper 100,000"

# get data set without current region
wrld_fltrd <- filter(wrld_joined_full, !code %in% ccodes_filter)
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
    geom_sf_pattern(
        data=wrld_joined_nas, color = "darkgrey", 
        linewidth = 0.2, 
        pattern = 'stripe', pattern_density = 0.15, 
        pattern_fill = 'gray33', pattern_colour = 'darkgrey', 
        pattern_spacing = 0.01
    ) + 
    theme_light(base_size = 16) +
    guides(color = "none") +
    #     scale_color_manual(values = 'black') + #, na.value='white') +
    coord_sf(expand=FALSE) + 
    annotation_custom(grob)



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


# outbr_map <- gg_map_outbreak_progress(
#     .wrld_joined_full=wrld_joined_full,
#     .wrld_joined_nas=wrld_joined_nas, 
#     .metric='per_100k', #years_1_2
#     .filter_codes=ccodes_filter,
#     .grob=grob,
#     .start_day=start_day,
#     .dest_dir = dest_dir
#     )

 

# source('model/utils.R')
# all_filepaths = list.files(dest_dir, full.names = T)
# gif_maker(all_filepaths, "outbreak_progression", dest_dir, .fps = 1)


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


##### MAKE GIFS 

walk(region_codes, function(whoregion) {
    # whoregion <- 'AFR'
    dest_dir = file.path('figs/CEPI_pres/maps_region/gifs', whoregion) 
    all_filepaths = list.files(dest_dir, full.names = T)
    gif_maker(all_filepaths, whoregion, dest_dir, .fps = 0.5)
}, .progress = T)









df_results_summary %>% count(simulation)


df_start_dest_code_coords$curvature <- runif(
    nrow(df_start_dest_code_coords), -0.2, 0.2
    )


annotations <- tibble(
    arrow_from = paste(
    df_start_dest_code_coords$start_lon, 
    df_start_dest_code_coords$start_lat,
    sep=', '
    ),
    arrow_to = paste(
    df_start_dest_code_coords$dest_lon, 
    df_start_dest_code_coords$dest_lat,
    sep=', '
    ),
    curvature = runif(nrow(df_start_dest_code_coords), -0.2, 0.2)
) %>%
    separate(arrow_from, into = c("x", "y")) %>%
    separate(arrow_to, into = c("xend", "yend")) 



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
                    x = start_lon,
                    xend = dest_lon,
                    y = start_lat,
                    yend = dest_lat
                ),
                # that's the whole point of doing this loop:
                curvature = current %>% pull(curvature),
                size = 0.2,
                arrow = arrow(
                    length = unit(0.005, "npc")
                )
            )
    })

outbr_map
