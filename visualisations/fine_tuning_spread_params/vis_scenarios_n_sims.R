library("tidyverse")
library("ggplot2")
library('cowplot')


source('utils/utils_post_proc.R')

## summarise and plot scenario
main_dir <- "res/scenarios_1000_sims/1000_sim_10224" 
scenario_id <- "n simulations"
pltname = 'scenarios_n_simulations.png'
plt_ncols = 5
### str_replace('_', ' ') for simulation_id
### figsize width=3300, height=1800,

########################################
# make and save individual summary dfs #
########################################

# long format full simulation results with daily infections 
## SAVING the files is very expensive 
df_all_sims_long <- make_df_all_sims_long(main_dir, save=FALSE)
# shorter simulation summary with one row per outbreak/country 
## SAVING the files is very expenive 
df_all_sims_sum <- make_df_all_sims_sum(main_dir, save=FALSE)

# get summary of years 1, 2, 1+2 and total infections
df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
# add duration info and other summary variables 
df_full_summary <- make_df_full_summary(
    df_all_sims_sum, df_summary_by_year, save=FALSE, res_dir=main_dir
    )



####################################
# join all dfs for zika rate plots #
####################################

all_fpaths <- list.files(main_dir, full.names=T, pattern='RDS')

scenario_names <- c(
    'df_sample_10_1', 'df_sample_10_2',
    'df_sample_50_1', 'df_sample_50_2',
    'df_sample_100_1', 'df_sample_100_2'
    )


set.seed(1122024)

# copy files 
walk2(c(10,10,50,50,100,100), scenario_names, function(.nsims, .scenario_name) {
    dest_fpath <- file.path(main_dir, '..',  .scenario_name)
    dir.create(dest_fpath)
    simpaths <- sample(all_fpaths, .nsims)
    file.copy(simpaths, dest_fpath)
})

sampledirs <- dir(file.path(main_dir, '..'), pattern='sample', full.name=T) %>%
    str_sort(numeric=T)

df_sampled_res <- map(sampledirs, function(.res_dir) {
    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save=FALSE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save=FALSE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(df_all_sims_sum, df_summary_by_year, save=FALSE)
    return(df_full_summary)
}, .progress = TRUE)
names(df_sampled_res) = scenario_names

df_zika_cumul <- make_df_zika_cumul() 

plt_args <- list(
    list(sim_line_col='#f4c5c1', zika_line_col='#c73e32', fontsize=14),
    list(sim_line_col='#f4c5c1', zika_line_col='#c73e32', fontsize=14, ylab=''), 
    list(sim_line_col='#e1b5ca', zika_line_col='#8f0e4d', fontsize=14, ylab=''),
    list(sim_line_col='#e1b5ca', zika_line_col='#8f0e4d', fontsize=14, ylab=''),
    list(sim_line_col='#d0bcce', zika_line_col='#5f2457', fontsize=14),
    list(sim_line_col='#d0bcce', zika_line_col='#5f2457', fontsize=14, ylab='')
)

sampled_plots <- map2(df_sampled_res, plt_args,  ~make_spread_plot(.x, df_zika_cumul, .y))

full_1000_rate_plot <- make_spread_plot(
    df_full_summary, df_zika_cumul, 
    extrargs = list(sim_line_col='#cad7eb', zika_line_col='#4c72ae', fontsize=14)
    )

read.csv('methods/misc/cepi_color_scheme.csv')

# # save 
# ggsave(scenario_rate_plot,
#     filename=file.path('figs',pltname), dpi=330, 
#     width=3300, height=1400, units='px')



# plts_10 <- plot_grid(sampled_plots[[1]],sampled_plots[[2]], ncol=1) 
# plts_50 <- plot_grid(sampled_plots[[3]],sampled_plots[[4]], ncol=1)
# plts_100 <- plot_grid(sampled_plots[[5]],sampled_plots[[6]], ncol=1) 
# plts_10_50_100 <- plot_grid(plts_10,plts_50,plts_100, ncol=3, labels='AUTO') 
# plts <- plot_grid(
#     plts_10_50_100, full_1000_rate_plot, rel_widths = c(3, 2),
#     ncol=2, labels=c('','D'))
# plts

plts_10 <- plot_grid(sampled_plots[[1]],sampled_plots[[2]], ncol=2) 
plts_50 <- plot_grid(sampled_plots[[3]],sampled_plots[[4]], ncol=2)
plts_10_50 <- plot_grid(plts_10,plts_50, ncol=2, labels='AUTO') 
plts_100 <- plot_grid(sampled_plots[[5]],sampled_plots[[6]], ncol=2) 
plts_100_full <- plot_grid(
    plts_100, full_1000_rate_plot, rel_widths = c(1, 1),
    ncol=2, labels=c('C','D'))
plts <- plot_grid(
    plts_10_50, plts_100_full, ncol=1)
plts


ggsave(plts,
    filename=file.path('figs','plt_nsim.png'), dpi=330, 
    width=3700, height=2300, units='px')
