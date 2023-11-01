library('conflicted')
library('tidyverse')
library('MESS')
conflicts_prefer(
    dplyr::filter(),
    dplyr::lag(),
    .quiet = T
)

###############
## SIM FUNC ##
##############

# utility funcs 
source('model/utils.R')

# curve_shape_params = read.csv("data/shape_params_PAHO_cases_adj.csv")
# curve_shape_params$peak_time_abs = sample_params$peak_time - sample_params$t_min
# write.csv(curve_shape_params, file='chikX/data/shape_params_PAHO_cases_adj.csv', row.names=F)


# sample curve parameters from PAHO hyperbolic function fits 
# simulate an outbreak with a given amplitude 
# adjust for any delay in the outbreak start time 
# and filter out start using .threshold to reflect 
# start time + lag until detection 
    # .threshold = # currently defined as 1% of the peak size
    # this also avoids long left-tails, 
    # leading to more reasonable outbreak durations 
###### VERSION WHICH USES A THRESHOLD OF INFECTIONS TO AVOID LONG LEFT TAILS 
# sample_curve <- function(.curve_shape_params = curve_shape_params,
#                          .amplitude = amplitude,
#                          .outbreak_timing = outbreak_timing
#                          # .threshold = threshold
# ) {
#     sample_params <- sample_n(.curve_shape_params, 1)
#     # peak_time = sample_params$peak_time_abs #+ t_lag -- now using middle of 5 years
#     new_s_l <- rnorm(n = 1, mean = sample_params$s_l, sd = sample_params$s_l_SE * 0.25)
#     new_s_r <- rnorm(n = 1, mean = sample_params$s_r, sd = sample_params$s_r_SE * 0.25)
#     # negative params not accepted
#     new_s_l <- ifelse(new_s_l < 0, abs(new_s_l) - sample_params$s_l, new_s_l)
#     new_s_r <- ifelse(new_s_r < 0, abs(new_s_r) - sample_params$s_r, new_s_r)
#     # <1 not accepted
#     # new_s_l = ifelse(new_s_l < 1, 1, new_s_l)
#     # new_s_r = ifelse(new_s_r < 1, 1, new_s_r)
#     # first simulate over about 5 years
#     sim_length <- 5 * 365
#     time_d <- seq(from = 0, to = 5, length.out = 5 * 365)
#     outbreak_res <- shin_curve(
#         xs = time_d, amplitude = .amplitude,
#         h_transl = 2.5, # peak_time,
#         s_l = new_s_l, s_r = new_s_r
#     )
#     # remove daily infections < 1
#     # round down to get integer infections
#     outbreak_res <- ifelse(outbreak_res < 1, 0, outbreak_res) %>% floor()
#     # keep only positive values and their corresponding times
#     inds <- which(outbreak_res > 0)
#     # add an extra index if possible to reach 0 at the end
#     if (max(inds) < length(inds)) {
#         inds <- c(inds, (max(inds) + 1))
#     }
#     # subset positive daily infections
#     outbreak_res <- outbreak_res[inds]
#     # add 0s for days before outbreak start (hence -1)
#     outbreak_res <- c(rep(0, times = .outbreak_timing - 1), outbreak_res)
#     # account for the later start of the outbreak
#     t_lag <- (.outbreak_timing / 365.25) # convert outbreak start into years
#     # total outbreak duration
#     dt <- (length(outbreak_res) / 365.25) + t_lag

#     # adjust time to match the length of the outbreak
#     time_d <- seq(from = 0, to = dt, length.out = length(outbreak_res))
#     peak_ampltd <- max(outbreak_res)
#     peak_time <- time_d[which(outbreak_res == peak_ampltd)[1]]

#     # filter for cases less than starting threshold
#     # .threshold = quantile(outbreak_res, 0.4)
#     .threshold <- 0.01 * peak_ampltd
#     df_outbreak_res <- data.frame(
#         time_years = time_d,
#         daily_infections_sim = outbreak_res
#     ) %>%
#         filter(!(
#             (daily_infections_sim < .threshold) &
#                 (time_years <= peak_time)
#         ))
    
#     df_outbreak_res$time_years = seq(from=t_lag, to=t_lag+max(time_d), length.out=nrow(df_outbreak_res))

#     # this df is useful for diagnostics
#     simulation_info <- data.frame(
#         amplitude = .amplitude,
#         h_transl = peak_time,
#         s_l = new_s_l,
#         s_r = new_s_r,
#         dt = dt
#     )
#     # large list of different components returned
#     return(list(
#         outbreak_res = df_outbreak_res$daily_infections_sim, # sim res
#         time_d = df_outbreak_res$time_years, # time_years
#         simulation_info = simulation_info, # diagnostics
#         sample_params = sample_params
#     )) # diagnostics
# }



###### VERSION WHICH USES SCALING
sample_curve <- function(.curve_shape_params = curve_shape_params,
                         .amplitude = amplitude,
                         .outbreak_timing = outbreak_timing
                         # .threshold = threshold
) {
    sample_params <- sample_n(.curve_shape_params, 1)
    # peak_time = sample_params$peak_time_abs #+ t_lag -- now using middle of 5 years
    new_s_l <- rnorm(n = 1, mean = sample_params$s_l, sd = sample_params$s_l_SE * 0.25)
    new_s_r <- rnorm(n = 1, mean = sample_params$s_r, sd = sample_params$s_r_SE * 0.25)
    # negative params not accepted
    new_s_l <- ifelse(new_s_l < 0, abs(new_s_l) - sample_params$s_l, new_s_l)
    new_s_r <- ifelse(new_s_r < 0, abs(new_s_r) - sample_params$s_r, new_s_r)
    # <1 not accepted
    # new_s_l = ifelse(new_s_l < 1, 1, new_s_l)
    # new_s_r = ifelse(new_s_r < 1, 1, new_s_r)
    # first simulate over about 5 years
    sim_length <- 5 * 365
    time_d <- seq(from = 0, to = 5, length.out = 5 * 365)
    outbreak_res <- shin_curve(
        xs = time_d, amplitude = .amplitude,
        h_transl = 2.5, # peak_time,
        s_l = new_s_l, s_r = new_s_r
    )
    # get first peak size 
    peak_ampltd <- floor(max(outbreak_res))
    # create a data frame which will be tidied up to get curve 
    df_sim_res <- data.frame(
        # time_years = time_d,     # remove vals < 1 and round down to get integer infections
        daily_infections_sim = ifelse(outbreak_res < 1, 0, outbreak_res) %>% floor(),
        derived = c(0, diff(outbreak_res))
    ) %>% filter(daily_infections_sim > 0) # remove daily infections < 1
    # threshold is 1% of absolute value of the derivative 
    # (i.e. dropping all values where there is almost no change in the daily infection values )
    fltrd <- filter(df_sim_res, (abs(derived) > max(derived) * 0.01))

    # between the first and the last value pick the larger 
    scaling_f <- max(fltrd[c(1, nrow(fltrd)), "daily_infections_sim"])
    # subtract scaling_f and rescale by the original amplitude 
    fltrd$daily_inf_adj <- peak_ampltd * ((fltrd$daily_infections_sim - scaling_f) / (peak_ampltd - scaling_f))
    fltrd$daily_inf_adj <- ifelse(fltrd$daily_inf_adj >= 0, fltrd$daily_inf_adj, 0) %>% floor()
    # keep non-negative values 
    fltrd <- filter(fltrd, daily_inf_adj > 0)
    
    # account for the later start of the outbreak
    t_lag <- (.outbreak_timing / 365.25) # convert outbreak start into years
    # total outbreak duration
    dt <- (nrow(fltrd) / 365.25) + t_lag

    # adjust time to match the length of the outbreak
    sim_res <- c(rep(0, times = .outbreak_timing), fltrd$daily_inf_adj)
    time_d <- seq(from = 0, to = dt, length.out = length(sim_res))
    peak_time <- time_d[which(fltrd$daily_inf_adj == peak_ampltd)[1]]


    # this df is useful for diagnostics
    simulation_info <- data.frame(
        amplitude = .amplitude,
        h_transl = peak_time,
        s_l = new_s_l,
        s_r = new_s_r,
        dt = dt
    )
    # large list of different components returned
    return(list(
        outbreak_res = sim_res, # sim res
        time_d = time_d, # time_years
        simulation_info = simulation_info, # diagnostics
        sample_params = sample_params
    )) # diagnostics
}







# one run of the simulation
# takes in vec_regions to loop over
# their associated data frame of initial conditions
# and simulation number
f_sim <- function(vec_catchments_i = vec_catchments_i,
                  df_initialConditions_i = df_initialConditions_i,
                  .simulation_i = simulation_i) {
    # loop over all countries to which CHIK-X spread in a given simulation_i
    # for catchment_j in initialConditions_i, begin simulation
    # for(catchment_j in vec_catchments_i){
    list_diseaseX_i <- map(vec_catchments_i, function(catchment_j) {
        if (!is.null(catchment_j)) {
            # extract population size etc of catchment_j
            df_initialConditions_j <- dplyr::filter(df_initialConditions_i, code == catchment_j)
            popSize <- df_initialConditions_j$total_pop_size
            outbreak_timing <- df_initialConditions_j$timing
            code <- df_initialConditions_j$code
            country <- df_initialConditions_j$country
            amplitude <- df_initialConditions_j$amplitude

            # run sample curve params and get simulation output
            outbreak_sim <- sample_curve(
                .curve_shape_params = curve_shape_params,
                .amplitude = amplitude,
                .outbreak_timing = outbreak_timing
            )
            # get result vector and time vectors out 
            outbreak_res <- outbreak_sim$outbreak_res
            time_d <- outbreak_sim$time_d

            # cumulative incidence - numeric integral 
            # very similar to sum over all res 
            cum_U <- MESS::auc(seq_along(outbreak_res), outbreak_res,
                type = "spline", subdivisions = 1e6
            )

            # create output df 
            df_diseaseX_k <- data.frame(
                time_years = time_d,
                daily_infections_sim = outbreak_res
            ) %>% mutate(
                country = country,
                code = code,
                timing = outbreak_timing,
                simulation = .simulation_i,
                IncCumul_U_final = cum_U
            )
            # comment out below to return only sim_res aka df_disease_k
            # supplementary table with params for debugging
            sim_params_info <- list(
                simulation_info = cbind(
                    data.frame(
                        country = country,
                        code = code,
                        timing = outbreak_timing,
                        simulation = .simulation_i,
                        IncCumul_U_final = cum_U
                    ),
                    outbreak_sim$simulation_info
                ),
                sampled_params = outbreak_sim$sample_params # from PAHO data
            )

            list_diseaseX_i_out <- list(
                sim_res = df_diseaseX_k,
                sim_params_info = sim_params_info
            )
        }
    })
    return(list_diseaseX_i)
}



# quick check if function runs as desired 
if (!interactive()) { # not run when file is sourced 
    f_sim(df_initialConditions_i = list_initial_conditions[[10]], 
          vec_catchments_i = levels(factor(list_initial_conditions[[10]]$code)),
          .simulation_i = 10) %>% bind_rows %>%
        ggplot(aes(time_years, daily_infections_sim)) + 
        geom_point(aes(color=country)) + 
        facet_wrap(~country) + 
        guides(color='none') + theme_light()
        
}






################
### SIM LOOP ###
################

# run model for n_sim simulations and save 
# each simulation in a separate .RData file 
f_sim_run = function(n_sim=100, .dest_dir = NULL){
    if (is.null(.dest_dir)==T || file.exists(.dest_dir)==F) {
        stop(paste('invalid dest_dir:\n"', .dest_dir,'"', sep='')) 
    } else {
        ### For each set of initial conditions
        # .packages = c('magrittr', 'tidyverse', 'deSolve')
        # foreach(simulation_i = icount(n_simulations), .combine = f(n_simulations)) %dopar% {
        # for(simulation_i in first_sim:n_simulations){
        walk(1:n_sim, function(simulation_i){
            
            # select initialConditions for model run i
            df_initialConditions_i = list_initial_conditions[[simulation_i]]
            # init_conditaions.R returns NULL if no establishment 
            if (!is.null(df_initialConditions_i)) { 
                vec_catchments_i = levels(factor(df_initialConditions_i$code))
                
                # simulate outbreaks by sampling curves 
                list_diseaseX_i = f_sim(
                    vec_catchments_i = vec_catchments_i, 
                    df_initialConditions_i = df_initialConditions_i, 
                    .simulation_i = simulation_i)
                
                #### SAVE RES #### 
                # cook up correct file name depending on input style  
                fname = ifelse(
                    endsWith(.dest_dir, '/'), 
                    paste(.dest_dir, 
                          'list_diseaseX_i_simulation_', simulation_i, 
                          '.RData', sep=''),
                    paste(.dest_dir, 
                          '/list_diseaseX_i_simulation_', simulation_i, 
                          '.RData', sep='')
                )
                save(list_diseaseX_i, file = fname)
                } 
        }, .progress=T)
        cat(paste('Simulations saved in "', .dest_dir, '".\n',  sep=''))
    }
}


# quick check if function runs as desired 
if (!interactive()) { # not run when file is sourced 
    f_sim_run(n_sim = 2,.dest_dir = 'chikX/res')
    # ecdf() for cumulative distribution 
}
