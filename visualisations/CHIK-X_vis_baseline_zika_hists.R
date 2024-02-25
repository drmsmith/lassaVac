library("tidyverse")
library("ggplot2")
library("conflicted")
library("grid")
library("cowplot")
library("ggh4x")

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




##############################
# single simulation separate # 
# outbreak plots as panels   # 
##############################

# subset simulation 
sim_no = 88 
outbrk_data <- df_all_sims_long %>% filter(simulation==sim_no) %>% arrange(timing)
outbrk_data$country <- factor(outbrk_data$country, levels=unique(outbrk_data$country))

# convert days to decimal years 
df_timing = outbrk_data[,c('timing', 'country')] %>% unique
df_timing$timing = df_timing$timing/365.25


outbreak_plts <- plot_sim_daily_infections(.data = outbrk_data)
outbreak_plts <- correct_facet_yaxis (.gg_plt = outbreak_plts, .yax_factor = 1.1)

# save
ggsave(outbreak_plts,
    filename = 'figs/paho_cases_and_single_simulation/single_sim_outbreak_data.png', 
    dpi=330, bg='transparent', width = 10, height = 6, units = 'in'
)
ggsave(outbreak_plts,
    filename = 'figs/paho_cases_and_single_simulation/single_sim_outbreak_data.svg', 
    dpi=330, bg='transparent', width = 10, height = 6, units = 'in'
)




#####################################
# histograms of number of countries #
# affected by days 100 and 160      #        
#####################################


# get number of countries with outbreaks by 
# day 100
spread_cumul_timing_100 <- df_full_summary %>%
    filter(outbreak_start_day <= 100) %>%
    group_by(simulation) %>%
    count() %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation), 
        day = 'day 100'
        )

# get number of countries with outbreaks by 
# day 100
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


# create histogram with facet ~day
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
    
# adjust yaxis to give breathing space 
incid_scen_n_by_day <- incid_scen_n_by_day + 
    ylim(0, get_plot_limits(incid_scen_n_by_day)$ymax * 1.05)

# save 
ggsave(incid_scen_n_by_day,
    filename=file.path(dest_dir, 'hists_100_160_baseline.png'), 
    dpi=330, width=3000, height=2000, units='px')

ggsave(incid_scen_n_by_day,
    filename=file.path(dest_dir, 'svg', 'hists_100_160_baseline.svg'), dpi=330, 
    width=3000, height=2000, units='px')





###############################
# eCDF and Zika spread tuning # 
###############################
        

cumul_dfs <- get_spread_cdf(df_full_summary)
spread_cumul_timing <- cumul_dfs$spread_cumul_timing
sum_cumul_spread <- cumul_dfs$sum_cumul_spread
rate_tune_plot <- make_scenario_rate_plot_single_scenario(
    spread_cumul_timing, sum_cumul_spread
    )


    
rate_tune_plot <- rate_tune_plot + ylim(0, get_plot_limits(rate_tune_plot)$ymax * 1.05)
# rate_tune_plot

# save
ggsave(rate_tune_plot,
    filename=file.path(dest_dir, 'baseline_zika_spread.png'), dpi=330, 
    width=2000, height=1500, units='px')

ggsave(rate_tune_plot,
    filename=file.path(dest_dir, 'svg', 'baseline_zika_spread.svg'), dpi=330, 
    width=2000, height=1500, units='px')




########################
# grid: Zika and hists #  
########################

# put rate and hists on a grid 
baseline_plots = plot_grid(rate_tune_plot, incid_scen_n_by_day, labels = "AUTO", ncol=2) 
# baseline_plots

# save
ggsave(baseline_plots,
    filename=file.path(dest_dir, 'zika_spread_and_histograms.png'), dpi=330, 
    width=3300, height=1700, units='px')

ggsave(baseline_plots,
    filename=file.path(dest_dir, 'svg', 'zika_spread_and_histograms.svg'), dpi=330, 
    width=3300, height=1700, units='px')

