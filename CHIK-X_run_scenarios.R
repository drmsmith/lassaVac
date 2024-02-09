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

source('model/utils_post_proc.R')

incidence_factors <- 1:10 # init = c(1,10) 


walk(incidence_factors, function(.inc_fact) {
    # seed for every scenario
    set.seed(31124)

    # utility funcs
    source('model/utils.R')
    # modelling daily infections / sampling curve shapes
    source('model/CHIK-X_sim.R')


    # define res. dir. and sim. hyper-params
    scenario_name <- paste0('incidence_x', .inc_fact)
    res_dir = as.character(file.path('res/scenarios', scenario_name))
    n_simulations = 5                      # number of simulations
    duration_spread <- 365*2                # simulation time span
    infectiousness_duration <- 7    # infected individual can infect days

    # these factors transform 
    # pop-weighted suitability
    # f * suit ^ k 
    fact_f = 1
    fact_k = 1 # 0.01 this effectively makes p_outbreak=1 
    ## 0.1 and 10

    # list to be fed to simulation function 
    simulation_hyperparameters = list(
        res_dir = res_dir, 
        n_simulations = n_simulations, 
        duration_spread = duration_spread, 
        infectiousness_duration = infectiousness_duration,
        fact_f = fact_f,
        fact_k = fact_k
    )


    # suitability (instead of estd. infections
    df_burden <- read.csv("data/2019ppp_pd_df_suit_means_who_p_spillover.csv")
    # 2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover
        # when changing df burden make sure that the corresponding 
        # suitability columns are named appropriately in chik-x_sim.R
        # e.g. df_burden$mean_pop_wght_transfrm <- fact_f * df_burden$mean_pd_weighted^fact_k  

    ####### MODIFY ANNAL INCIDENCE #######
    df_burden$annual_incidence = df_burden$annual_incidence * .inc_fact

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
    df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
    df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
    paho_codes <- df_paho_outbreak_sizes$code


    data_files <- list(
        df_burden = df_burden, 
        mat_mob_daily_trips = mat_mob_daily_trips,
        df_paho_daily_cases = df_paho_daily_cases, 
        df_paho_outbreak_sizes = df_paho_outbreak_sizes
    )

    # FINALLY, RUN SIMULATION
    f_run_sim(sim_hyperparams=simulation_hyperparameters, data_files = data_files, parallel = TRUE)

})






## summarise and plot scenario

main_dir <- 'res/scenarios/'
resdirs = list.dirs(main_dir)[3:4]
figpath = file.path(main_dir, 'figs')
if (!dir.exists(figpath)) dir.create(figpath)

walk(resdirs, function(.res_dir) {
    # get all file paths
    all_files <- list.files(.res_dir, "RDS", full.names = T) %>%
        str_sort(numeric = TRUE) 

    df_all_sims_long <- map(all_files, function(dirname) {
        res <- readRDS(dirname)
        map(res, function(.ls_sim) .ls_sim$df_res_curve) %>% bind_rows()
    }, .progress = T) %>% bind_rows()

    df_all_sims_sum <- map(all_files, function(dirname) {
        res <- readRDS(dirname)
        map(res, function(.ls_sim) .ls_sim$df_res_summary) %>% bind_rows()
    }, .progress = T) %>% bind_rows()

    #### high-level summary 

    # how many countries appear in the simulations 
    cat(.res_dir)
    cat('\nN sims: ')
    # total number of simulations 
    df_all_sims_sum$simulation %>% unique %>% length %>% print
    cat('\nN countries in total: ')
    df_all_sims_sum$code %>% unique %>% length %>% cat
    cat('\nN countries spread: ')
    # how many countries per simulation 
    counts = df_all_sims_sum %>% group_by(simulation) %>% count()
    cat('\nmedian N countries spread: ')
    median(counts$n) %>% unlist %>% median %>% cat
    cat('\n')
    counts %>% .$n %>% print # print(n=100)


    df_summary_by_year <- make_summary_by_year(df_all_sims_long)
    df_full_summary <- make_full_summary(df_all_sims_sum, df_summary_by_year)
    rate_tune_plot <- make_spread_plot(df_full_summary, df_zika_cumul)
    pltname = paste0(str_remove(.res_dir, main_dir), '.png')
    figpath = file.path(figpath, pltname)

    ggsave(
        filename = figpath,
        width = 3500, height = 2500, units = "px",
        dpi = 400,
        bg = "transparent"
    )

})







