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



source('model/utils.R')


# # define simulation parameters
# set.seed(31124)
# # unlink(res_dir)
# ##### THESE RESULTS HAVE 
# ##### OUTBREAK SIZE ~ 0.2-0.6 * POP SIZE 
# # res_dir = './res/imp_mod_2023_suit_incidence'     # save results in this dir 
# # res_dir = './res/slow_spread'     # save results in this dir 
# ### fact_f and fact_k   100 sims wth 0.05 and 10 
# ### fact_f and fact_k   100 sims with 0.1 and 12 

# ##### NOW OUTBREAK SIZE = ANNUAL INCIDENCE 
# res_dir = './res/outbreak_annual_inc'

# n_simulations = 10                      # number of simulations
# duration_spread <- 365*2                # simulation time span
# infectiousness_duration <- 7    # infected individual can infect days

# # these factors transform 
# # pop-weighted suitability
# # f * suit ^ k 
# fact_f = 1
# fact_k = 0.01
# ## 0.1 and 10



# # initialise results dir 
# if (!dir.exists(res_dir)) {dir.create(res_dir, recursive = T)}


# # suitability (instead of estd. infections)
# # suitability info for 220 countries
# df_burden <- read.csv("data/df_suit_means_pop_wght_pop_size_who_p_spillover.csv")
# colnames(df_burden)

# # mobility data (daily trips between src and dest)
# # mobility info for 188 countries
# mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
# all_codes <- colnames(mat_mob_daily_trips)
# rownames(mat_mob_daily_trips) <- all_codes

# # ensure mobility data and suitability data are matched 
# # since there are fewer countries on the mobility data set 
# # 220 vs 188 (suit vs mobility)
# # final number is 184 for some reason
# df_burden <- filter(df_burden, code %in% all_codes) %>% drop_na()



# # paho case data
# df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
# df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
# paho_codes <- df_paho_outbreak_sizes$code


#########################
#### PARALLELISATION ####
#########################

# library('doParallel')
# library('foreach')
# library('progress')

# f <- function(iterator){
#   pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
#   count <- 0
#   function(...) {
#     count <<- count + length(list(...)) - 1
#     setTxtProgressBar(pb, count)
#     flush.console()
#     list(...) # this can feed into .combine option of foreach
#   }
# }


# totalCores = detectCores()
# # cl <- makeCluster(totalCores[1]-1, type='SOCK')
# cl <- parallel::makeCluster(4, type='PSOCK')
# registerDoParallel(cl)
# clusterEvalQ(cl,  library('tidyverse'))
# clusterEvalQ(cl,  source('model/utils.R'))
# # clusterEvalQ(cl, set.seed(31124))




###############################
# start simulation, init cond #
###############################

f_init_sim <- function(sim_i) {
    # matrix of spread across catchments
    # (to be updated in loop with spread model)
    m_spread <- matrix(0, nrow = nrow(df_burden), ncol = duration_spread)
    colnames(m_spread) <- 1:duration_spread
    rownames(m_spread) <- df_burden$code

    # starting catchment
    # identify first catchment, add to vector of infected catchments, update spread matrix
    catchment0 <- sample(df_burden$code, size = 1, prob = df_burden$p_spillover)
    catchment0_pop_size <- df_burden$pop_size[df_burden$code == catchment0]

    # initialise vec_catchments_infected
    vec_catchments_infected <- catchment0
    # fill rest of row with 1 (assumption is that catchments do not return to susceptible)
    m_spread[catchment0, ] <- 1

    # subset relevant data
    df_catchment0 <- df_burden[
        df_burden$code == catchment0,
        c("country", "code", "region_name", "region_code", "pop_size", "annual_incidence")
    ]


    # generate outbreak data
    # generate_outbreak_by_pop_size = outbreak size = factor * population size 
    # generate_outbreak_by_annual_incidence = outbreak size = factor * annual incidence 
    # fix_pop_affected == prop_adj in the two functions 
    outbreak_0 <- generate_outbreak_by_annual_incidence(
        sim_day = 0,
        df_country_data = df_catchment0,
        prop_adj = 1,       # outbreak size = 1 * annual_incidence 
        infectiousness_duration = infectiousness_duration,
        sim_i = sim_i
    )

    # list$'country code' = ls_outbreak_info
    ls_outbreaks <- setNames(list(outbreak_0), catchment0)

    return(list(
        ls_outbreaks=ls_outbreaks, 
        m_spread=m_spread, 
        vec_catchments_infected=vec_catchments_infected))
}

f_spread_outbreak <- function(
    vec_catchments_infected,
    ls_outbreaks,
    m_spread,
    duration_spread, 
    mat_mob_daily_trips, 
    df_burden, 
    sim_i
    ) {
    # go through outbreak and update matrices day-by-day
    for (day_i in 1:duration_spread) {
        # for each catchment currently infected
        for (catchment_infect_j in vec_catchments_infected) {
            # get outbreak data and info
            outbrk <- ls_outbreaks[[catchment_infect_j]]

            # n_infectious people on day_i from source country
            n_infectious <- outbrk$v_daily_new_infections[day_i - outbrk$df_res_summary$timing]
            # population size, source country
            pop_size <- outbrk$df_res_summary$pop_size
            # proportion of infectious population
            prop_infectious <- n_infectious / pop_size
            # all catchments not currently infected
            uninfected_ccodes <- df_burden$code[!df_burden$code %in% vec_catchments_infected]
            # filter out ccodes with no travel between countries to save comp time
            no_travel <- all_codes[mat_mob_daily_trips[catchment_infect_j, ] == 0]
            # leave countries not yet infected but where there is travel between source and dest
            uninfected_travel_ccodes <- uninfected_ccodes[!uninfected_ccodes %in% no_travel]

            # evaluate probability of spread and outbreak to all catchments not currently infected
            for (catchment_suscept_k in uninfected_travel_ccodes) {
                # p infectious moving a -> b
                n_travellers <- mat_mob_daily_trips[catchment_infect_j, catchment_suscept_k]
                n_infectious_travellers <- n_travellers * prop_infectious
                prop_infectious_traveller <- n_infectious_travellers / n_travellers


                ##### CALCULATE P INFECTIOUS PERSON TRAVELLING #####
                ####################################################
                # probability of an infected person travelling
                travel_infect_i_j <- rbinom(1, 1, prop_infectious_traveller)

                # probability of outbreak starting = suitability
                ####################################################### CHANGE BACK TO POP WEIGHTED
                p_outbreak <- df_burden$mean_pop_wght_transfrm[df_burden$code == catchment_suscept_k]
                outbreak_i_j <- rbinom(1, 1, p_outbreak)


                # if infected travels AND starts outbreak
                # update infected catchments and corresponding m_spread matrix
                if ((travel_infect_i_j == 1) & (outbreak_i_j == 1)) {
                    # add to infected countries
                    vec_catchments_infected <- append(
                        vec_catchments_infected,
                        catchment_suscept_k
                    )
                    # indicate start date
                    m_spread[catchment_suscept_k, day_i:ncol(m_spread)] <- 1

                    # subset relevant data
                    df_suscept_country <- df_burden[
                        df_burden$code == catchment_suscept_k,
                        c("country", "code", "region_name", "region_code", "pop_size", "annual_incidence")
                    ]

                    # generate outbreak data
                    new_outbreak <- generate_outbreak_by_annual_incidence(
                        sim_day = day_i, df_country_data = df_suscept_country,
                        prop_adj = 1, infectiousness_duration = infectiousness_duration,
                        sim_i = sim_i
                    )
                    # append as 'country code' = ls_outbreak_info
                    ls_outbreaks <- append(
                        ls_outbreaks, setNames(list(new_outbreak), catchment_suscept_k)
                    )
                }
            }
        }
    }
    return(list(
        vec_catchments_infected = vec_catchments_infected,
        ls_outbreaks = ls_outbreaks,
        m_spread = m_spread
    ))
}



f_one_sim <- function(
    sim_i, 
    duration_spread, 
    .print = FALSE, 
    mat_mob_daily_trips, 
    df_burden
){
    start.time.total <- Sys.time()
    if (.print==TRUE) { 
        cat(paste0("Simulation ", sim_i, "\n", collapse = ""))
    }
    # initialise outbreak with spillover event
    init_outbreak <- f_init_sim(sim_i)
    ls_outbreaks <- init_outbreak$ls_outbreaks
    m_spread <- init_outbreak$m_spread 
    vec_catchments_infected <- init_outbreak$vec_catchments_infected
    

    # cat(paste0("Day ", day_i, "\n", collapse = ""))
    iterators_updtd <- f_spread_outbreak(
        vec_catchments_infected=vec_catchments_infected, 
        ls_outbreaks=ls_outbreaks,
        m_spread=m_spread,
        duration_spread=duration_spread,
        mat_mob_daily_trips=mat_mob_daily_trips, 
        df_burden=df_burden,
        sim_i=sim_i
        )
    vec_catchments_infected=iterators_updtd$vec_catchments_infected
    ls_outbreaks=iterators_updtd$ls_outbreaks
    m_spread=iterators_updtd$m_spread
        
    return(ls_outbreaks)
}


f_sim_loop <- function(
    n_simulations, mat_mob_daily_trips, df_burden
    ) {
    for (sim_i in 1:n_simulations) {
        start.time <- Sys.time()

        ls_outbreaks_sim_i <- f_one_sim(
            sim_i, duration_spread, .print = TRUE,
            mat_mob_daily_trips=mat_mob_daily_trips, 
            df_burden=df_burden
            )
        filename <- paste0(res_dir, "/simulation_", sim_i, ".RDS", collapse = "")
        saveRDS(ls_outbreaks_sim_i, file = filename)

        end.time <- Sys.time()
        time.taken <- round(end.time - start.time, 2)
        print(time.taken)
    }
}


f <- function(iterator){
  pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb, count)
    flush.console()
    list(...) # this can feed into .combine option of foreach
  }
}




f_sim_loop_parallel <- function(n_simulations, mat_mob_daily_trips, df_burden) {

    totalCores = detectCores()
    # cl <- makeCluster(totalCores[1]-1, type='SOCK')
    cl <- parallel::makeCluster(4, type='PSOCK')
    registerDoParallel(cl)
    clusterEvalQ(cl,  library('tidyverse'))
    clusterEvalQ(cl,  source('model/utils.R'))
    # clusterEvalQ(cl, set.seed(31124))

    foreach(sim_i = icount(n_simulations), .combine = f(n_simulations)) %dopar% {
        # start.time.total <- Sys.time()
        
        ls_outbreaks_sim_i <- f_one_sim(sim_i, duration_spread, .print = FALSE)
        filename <- paste0(res_dir, "/simulation_", sim_i, ".RDS", collapse = "")
        saveRDS(ls_outbreaks_sim_i, file = filename)

        # end.time <- Sys.time()
        # time.taken <- round(end.time - start.time, 2)
        # print(time.taken)
    }

    stopCluster(cl)

}



f_run_sim <- function(sim_hyperparams, parallel = FALSE) {
    # define and validate parameters from list
    n_simulations <- sim_hyperparams$n_simulations
    if (is.na(as.integer(n_simulations)) | n_simulations<=0) {
        stop('Invalid number of simulations (must be a non-negative int)')
    }
    duration_spread <- sim_hyperparams$duration_spread
    if (is.na(as.integer(duration_spread)) | duration_spread<=0) {
        stop('Invalid simulation duration (must be a non-negative int)')
    }
    res_dir <- sim_hyperparams$res_dir
    if (!is.character(res_dir)) {
        stop('Invalid results directory (must be a string)')
    }

    infectiousness_duration <- sim_hyperparams$infectiousness_duration
    if (is.na(as.integer(infectiousness_duration)) | infectiousness_duration<=0) {
        stop('Invalid infectiousness duration (must be a non-negative int)')
    }
    fact_f <- sim_hyperparams$fact_f
    fact_k <- sim_hyperparams$fact_k
    if (
        !is.numeric(fact_f) | !is.numeric(fact_k) |
        fact_f <= 0 | fact_k <= 0
        ) {
        stop('Invalid suitability transformation factor(s) (must be a non-negative numeric)')
    } # NOTE fact_f > 1 will likely lead to suitability > 1 and NaNs for rbinom 

    # initialise results dir
    if (!dir.exists(res_dir)) {
        dir.create(res_dir, recursive = T)
    }

    #########################
    # transform suitability #
    #########################
    df_burden$mean_pop_wght_transfrm <- fact_f * df_burden$mean_pop_weighted^fact_k
    # cat("\nburden\n")
    # cat(mean(df_burden$mean_pop_wght_transfrm))
    # cat("\n")
    ###################
    # run simulations #
    ###################

    start.time.total <- Sys.time()

    if (parallel == TRUE) {
        f_sim_loop_parallel(
            n_simulations,
            mat_mob_daily_trips=mat_mob_daily_trips,
            df_burden=df_burden
            )
    } else {
        f_sim_loop(
            n_simulations,
            mat_mob_daily_trips=mat_mob_daily_trips,
            df_burden=df_burden
            )
    }

    cat(paste('\nSimulations saved in "', res_dir, '".\n', sep = ""))
    end.time.total <- Sys.time()
    time.taken <- round(end.time.total - start.time.total, 2)
    print(time.taken)
    
}




########################################
# SINGLE RUN VERSION WITH NESTED LOOPS #
########################################

# start.time.total <- Sys.time()

# for (sim_i in 1:n_simulations) {
# # foreach(sim_i = icount(n_simulations), .combine = f(n_simulations)) %dopar% {
#     cat(paste0("Simulation ", sim_i, "\n", collapse = ""))
#     start.time <- Sys.time()


#     # matrix of spread across catchments
#     # (to be updated in loop with spread model)
#     m_spread <- matrix(0, nrow = nrow(df_burden), ncol = duration_spread)
#     colnames(m_spread) <- 1:duration_spread
#     rownames(m_spread) <- df_burden$code

#     # starting catchment
#     # identify first catchment, add to vector of infected catchments, update spread matrix
#     catchment0 <- sample(df_burden$code, size = 1, prob = df_burden$p_spillover)
#     catchment0_pop_size <- df_burden$pop_size[df_burden$code == catchment0]

#     # initialise vec_catchments_infected
#     vec_catchments_infected <- catchment0
#     # fill rest of row with 1 (assumption is that catchments do not return to susceptible)
#     m_spread[catchment0, ] <- 1

#     # subset relevant data
#     df_catchment0 <- df_burden[
#         df_burden$code == catchment0,
#         c("country", "code", "region_name", "region_code", "pop_size", "annual_incidence")
#     ]


#     # generate outbreak data
#     # generate_outbreak_by_pop_size = outbreak size = factor * population size 
#     # generate_outbreak_by_annual_incidence = outbreak size = factor * annual incidence 
#     # fix_pop_affected == prop_adj in the two functions 
#     outbreak_0 <- generate_outbreak_by_annual_incidence(
#         sim_day = 0,
#         df_country_data = df_catchment0,
#         prop_adj = 1,       # outbreak size = 1 * annual_incidence 
#         infectiousness_duration = infectiousness_duration,
#         sim_i = sim_i
#     )

#     # list$'country code' = ls_outbreak_info
#     ls_outbreaks <- setNames(list(outbreak_0), catchment0)


#     # go through outbreak and update matrices day-by-day
#     for (day_i in 1:duration_spread) {
#         # cat(paste0("Day ", day_i, "\n", collapse = ""))
#         # for each catchment currently infected
#         for (catchment_infect_j in vec_catchments_infected) {
#             outbrk <- ls_outbreaks[[catchment_infect_j]] # get outbreak data and info

#             # n_infectious people on day_i from source country
#             n_infectious <- outbrk$v_daily_new_infections[day_i - outbrk$df_res_summary$timing]
#             # population size, source country
#             pop_size <- outbrk$df_res_summary$pop_size
#             # proportion of infectious population
#             prop_infectious <- n_infectious / pop_size
#             # all catchments not currently infected
#             uninfected_ccodes <- df_burden$code[!df_burden$code %in% vec_catchments_infected]
#             # filter out ccodes with no travel between countries to save comp time
#             no_travel <- all_codes[mat_mob_daily_trips[catchment_infect_j, ] == 0]
#             # leave countries not yet infected but where there is travel between source and dest
#             uninfected_travel_ccodes <- uninfected_ccodes[!uninfected_ccodes %in% no_travel]

#             # evaluate probability of spread and outbreak to all catchments not currently infected
#             for (catchment_suscept_k in uninfected_travel_ccodes) {
#                 # p infectious moving a -> b
#                 n_travellers <- mat_mob_daily_trips[catchment_infect_j, catchment_suscept_k]
#                 n_infectious_travellers <- n_travellers * prop_infectious
#                 prop_infectious_traveller <- n_infectious_travellers / n_travellers

#                 ##### CALCULATE P INFECTIOUS PERSON TRAVELLING #####
#                 ####################################################
#                 # probability of an infected person travelling
#                 travel_infect_i_j <- rbinom(1, 1, prop_infectious_traveller)

#                 # probability of outbreak starting = suitability
#                 ####################################################### CHANGE BACK TO POP WEIGHTED
#                 p_outbreak <- df_burden$mean_pop_wght_transfrm[df_burden$code == catchment_suscept_k]
#                 outbreak_i_j <- rbinom(1, 1, p_outbreak)


#                 # if infected travels AND starts outbreak
#                 # update infected catchments and corresponding m_spread matrix
#                 if ((travel_infect_i_j == 1) & (outbreak_i_j == 1)) {
#                     # add to infected countries
#                     vec_catchments_infected <- append(
#                         vec_catchments_infected,
#                         catchment_suscept_k
#                     )
#                     # indicate start date
#                     m_spread[catchment_suscept_k, day_i:ncol(m_spread)] <- 1

#                     # subset relevant data
#                     df_suscept_country <- df_burden[
#                         df_burden$code == catchment_suscept_k,
#                         c("country", "code", "region_name", "region_code", "pop_size", "annual_incidence")
#                     ]

#                     # generate outbreak data
#                     new_outbreak <- generate_outbreak_by_annual_incidence(
#                         sim_day = day_i, df_country_data = df_suscept_country,
#                         prop_adj = 1, infectiousness_duration = infectiousness_duration,
#                         sim_i = sim_i
#                     )
#                     # append as 'country code' = ls_outbreak_info
#                     ls_outbreaks <- append(
#                         ls_outbreaks, setNames(list(new_outbreak), catchment_suscept_k)
#                     )
#                 }
#             }
#         }
#     }



#     filename <- paste0(res_dir, "/simulation_", sim_i, ".RDS", collapse = "")
#     saveRDS(ls_outbreaks, file = filename)

#     end.time <- Sys.time()
#     time.taken <- round(end.time - start.time, 2)
#     print(time.taken)
# }


# # stopCluster(cl)

# cat(paste('Simulations saved in "', res_dir, '".\n', sep = ""))
# end.time.total <- Sys.time()
# time.taken <- round(end.time.total - start.time.total, 2)
# print(time.taken)


# str(ls_outbreaks)
