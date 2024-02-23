library('tidyverse')
library('conflicted')
conflicts_prefer(
    dplyr::filter(),
    stats::lag(),
    .quiet = T
)
library('doParallel')
library('foreach')
library('progress')


source('visualisations/utils_post_proc.R')


incidence_factors <- rep(c(0.9, 1), times=1) # seq(0.1, 0.5, by=0.1) # seq(0.6,1, by=0.1)
noise_params <- rep(c(0.1), each=length(incidence_factors))
# suit_vals_new = ifelse(suit_vals < prop_boost, sqrt(2*suit_vals)/2, suit_vals)
scenario_id <- 'baseline'

    # incidence_factors -> .inc_fact
walk2(incidence_factors, noise_params, function(.incidence_factor, .noise_param) {
    # seed for every scenario
    set.seed(31124)
    # set.seed(10224) # for 1000 simulations  

    # utility funcs
    source('model/utils.R')
    # modelling daily infections / sampling curve shapes
    source('model/CHIK-X_sim.R')


    # define res. dir. and sim. hyper-params
    # scenario_name <- paste0('incidence_x', .inc_fact)
    scenario_name <- paste0(scenario_id, '_inc_', .incidence_factor, '_noise_', .noise_param)
    res_dir = as.character(file.path('res/scenarios', scenario_name))
    n_simulations = 100                     # number of simulations
    duration_spread <- 365*2                # simulation time span / days
    infectiousness_duration <- 7            # infected individual can infect / days
    outbreak_size_adj <- .noise_param       # outbreak size ranges runif(1-param, 1+param)
    increased_lower_suitability <- FALSE    # TRUE: boost suitability for lower suitability countries 
    prop_boost <- 0.5                       # proportion of suitability to boost


    # these factors transform 
    # pop-weighted suitability
    # f * suit ^ k 
        ## 0.1 and 10 old suit data scaling 
    fact_f = 1 # .fact_f
    fact_k = 1 #.fact_k # 0.01 effectively makes p_outbreak=1 
    incidence_factor = .incidence_factor# 6 for varying outbreak size (constant)

    # list to be fed to simulation function 
    simulation_hyperparameters = list(
        res_dir = res_dir, 
        n_simulations = n_simulations, 
        duration_spread = duration_spread, 
        infectiousness_duration = infectiousness_duration,
        fact_f = fact_f,
        fact_k = fact_k, 
        incidence_factor = incidence_factor, 
        outbreak_size_adj = outbreak_size_adj
    )


    # annual incidence, population size, suitability 
    df_burden <- read.csv("data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv")
    # 2019ppp_pd_df_suit_means_who_p_spillover = wrong incidence
        # when changing df burden make sure that the corresponding 
        # suitability columns are named appropriately in chik-x_sim.R
        # e.g. df_burden$mean_pop_wght_transfrm <- fact_f * df_burden$mean_ppp_weighted^fact_k  


    ####### MODIFY ANNAL INCIDENCE #######
    df_burden$annual_incidence = df_burden$annual_incidence * incidence_factor# .inc_fact

    ###### MODIFY SUITABILITY ######
    if (increased_lower_suitability == TRUE) {
        df_burden$mean_ppp_weighted = ifelse(
            df_burden$mean_ppp_weighted < prop_boost,  # used to be mean_pd_weighted
            sqrt(2*df_burden$mean_ppp_weighted)/2, 
            df_burden$mean_ppp_weighted
            )
    }



    # mobility data (daily trips between src and dest)
    # mobility info for 188 countries
    mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
    all_codes <- colnames(mat_mob_daily_trips)
    rownames(mat_mob_daily_trips) <- all_codes

    # ensure mobility data and suitability data are matched 
    # since there are fewer countries on the mobility data set 
    # 220 vs 188 (suit vs mobility)
    # final number is 184 for some reason
    df_burden <- filter(df_burden, code %in% all_codes) %>% drop_na()

    # paho case data 
    df_paho_daily_cases <- read.csv("data/df_paho_daily_cases_fltrd_lagged_smooth.csv")
    # df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
    paho_codes <- unique(df_paho_daily_cases$code)

    # data files used by simulation 
    data_files <- list(
        df_burden = df_burden, 
        mat_mob_daily_trips = mat_mob_daily_trips,
        df_paho_daily_cases = df_paho_daily_cases
        )

    # FINALLY, RUN SIMULATION
    f_run_sim(sim_hyperparams=simulation_hyperparameters, data_files = data_files, parallel = TRUE)

})



### summarise and plot scenario

main_dir <- 'res/scenarios'
# resdirs = list.dirs(main_dir)[3:12]
resdirs = dir(main_dir, pattern=scenario_id, full.name=T) %>%
    str_sort(numeric=T)
# only 0.2 runif window
# resdirs <- resdirs[str_detect(resdirs, '0.2')]

figpath = file.path(main_dir, 'figs')
if (!dir.exists(figpath)) dir.create(figpath)

walk(resdirs, function(.res_dir) {
    # get all file paths

    # long format full simulation results with daily infections 
    df_all_sims_long <- make_df_all_sims_long(.res_dir, save_RDS=TRUE)
    # shorter simulation summary with one row per outbreak/country 
    df_all_sims_sum <- make_df_all_sims_sum(.res_dir, save_RDS=TRUE)

    # get summary of years 1, 2, 1+2 and total infections
    df_summary_by_year <- make_df_summary_by_year(df_all_sims_long, save_RDS=FALSE)
    # add duration info and other summary variables 
    df_full_summary <- make_df_full_summary(
        df_all_sims_sum, df_summary_by_year, save_RDS=TRUE,  res_dir=.res_dir
        )

    # plot spread against zika v baseline
    df_zika_cumul <- make_df_zika_cumul()
    rate_tune_plot <- make_spread_plot(df_full_summary, df_zika_cumul)
    # name and save plot 
    pltname = paste0(str_remove(.res_dir, main_dir), '.png')
    figpath = file.path(figpath, pltname)
    ggsave(
        filename = figpath, width = 3500, height = 2500, 
        units = "px", dpi = 400, bg = "transparent"
    )

})
