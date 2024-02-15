library("tidyverse")
library("ggplot2")
library('cowplot')


source('visualisations/utils_post_proc.R')



main_dir <- "res/scenarios_suit_inc_6" # "res/scenarios_x5" trial scenarios 
scenario_id <- "suit_fact"
pltname = 'suitability_scenarios_spread_fact_k.png'
plt_ncols = 4
### str_replace('suit_fact_k', 'factor ')
### width=3300, height=1400, 


resdirs <- file.path(main_dir, dir(main_dir, pattern = scenario_id)) %>%
    str_sort(numeric=T)

figpath <- file.path(main_dir, dir(main_dir, pattern = 'fig'))
if (!dir.exists(figpath)) dir.create(figpath)

########################################
# make and save individual summary dfs #
########################################

walk(resdirs, function(.res_dir) {
    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save=TRUE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save=TRUE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(df_all_sims_sum, df_summary_by_year, save=TRUE, res_dir=.res_dir)

}, .progress = TRUE)


####################################
# join all dfs for zika rate plots #
####################################


filepaths = list.files(main_dir, pattern='sim_full_summary.csv', full.names = T, recursive = T) %>%
    str_sort(numeric=T, decreasing = T)
    ##### DECREASING FOR SUITABILITY 

ids = dir(main_dir, pattern = scenario_id) %>%
    str_sort(numeric=T, decreasing = T) %>% str_replace('suit_fact_k', 'factor ')
    ##### DECREASING FOR SUITABILITY 


df_all_scenarios_full_summary <- map2(filepaths, ids, function(.fpath, .id) {
    df_full_summary <- read.csv(.fpath)
    df_full_summary$scenario_id = .id
    return(df_full_summary)
}) %>% bind_rows %>% mutate(scenario_id = factor(scenario_id, levels=ids))




##################
# zika rate tune #
##################

scenario_rate_plot <- make_scenario_rate_plot(df_all_scenarios_full_summary, ncols=plt_ncols) 

# save 
ggsave(scenario_rate_plot,
    filename=file.path('figs',pltname), dpi=330, 
    width=3300, height=1400, units='px')




## show suitability 
df_burden <- read.csv("data/2019ppp_pd_df_suit_means_who_p_spillover.csv")
colnames(df_burden)

df_tranformed_incidences = map(c(0.5, 0.7, 0.9, 1), function(.inc_fact) {
    df_out = data.frame(
        annual_incidence = df_burden$mean_pd_weighted ^ .inc_fact, 
        code = df_burden$code, 
        region_code = df_burden$region_code
        )
    df_out$incidence_factor = paste0('factor ',.inc_fact, sep='')
    return(df_out)
}) %>% bind_rows


fctr_lvls <- unique(df_tranformed_incidences$incidence_factor) %>% sort(decreasing=T)

trans_suit_hists <- df_tranformed_incidences %>%
    mutate(incidence_factor = factor(incidence_factor,levels=fctr_lvls)) %>%
    ggplot(aes(x = annual_incidence)) +
    geom_histogram(aes(fill = region_code)) +
    facet_wrap(~incidence_factor, ncol = 4) +
    theme_light(base_size = 16) +
    scale_fill_manual(values = c(
        "#547dbf", "#682860", "#ffa500", "#F9DF79", "#db4437", "#9d0f55"
    ), name = "WHO region") +
    # guides(fill='none') +
    theme(legend.position = "bottom") + 
    labs(x = "Population density-weighted\nmean suitability", y = "\nCount")

trans_suit_hists
# save 
ggsave(trans_suit_hists,
    filename=file.path('figs','trans_suit_fact_k_hists.png'), dpi=330, 
    width=3500, height=1400, units='px')



plts = plot_grid(scenario_rate_plot,trans_suit_hists, labels = "AUTO", ncol=1) 
ggsave(plts,
    filename=file.path('figs','trans_suit_fact_k_hists_spread.png'), dpi=330, 
    width=3500, height=2400, units='px')






###############################
# distribution of n countries #
# affected by day 100 and 160 # 
###############################


# calculate cumulative spread over time 
spread_cumul_timing_100 <- df_all_scenarios_full_summary %>%
    filter(outbreak_start_day <= 100) %>%
    group_by(scenario_id, simulation) %>%
    count(simulation) %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )


incid_scen_n_by_100 <- ggplot(
    spread_cumul_timing_100,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~scenario_id, ncol=plt_ncols) + 
    geom_histogram(binwidth=1, fill="#e89600") +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 100", 
        y = "Count"
    ) +
    theme_light(base_size = 12) + 
    xlim(-1,12)
incid_scen_n_by_100


spread_cumul_timing_160 <- df_all_scenarios_full_summary %>%
    filter(outbreak_start_day <= 160) %>%
    group_by(scenario_id, simulation) %>%
    count(simulation) %>% 
    rename(cumul_nspread = n) %>%
    mutate(
        simulation = factor(simulation),
        scenario_id = factor(scenario_id, levels = ids)
    )

incid_scen_n_by_160 <- ggplot(
    spread_cumul_timing_160,
    aes(x = cumul_nspread)
    ) +
    facet_wrap(~scenario_id, ncol=plt_ncols) + 
    geom_histogram(binwidth=1, fill="#007492") +
    labs(
        x = "Number of countries experiencing\noutbreaks by day 160", 
        y = "Count"
    ) +
    theme_light(base_size = 12) + xlim(-1,12)



plts = plot_grid(incid_scen_n_by_100,incid_scen_n_by_160, labels = "AUTO", ncol=1) 
plts
ggsave(plts,
    filename=file.path('figs','hists_100_160_suit_fact_k.png'), dpi=330, 
    width=3500, height=2400, units='px')
