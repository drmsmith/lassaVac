library('tidyverse')

###############
## SIM FUNC ##
##############

# utility funcs 
source('chikX/model/utils.R')
curve_shape_params = read.csv("chikX/data/shape_params_PAHO_cases_adj.csv")
# curve_shape_params$peak_time_abs = sample_params$peak_time - sample_params$t_min
# write.csv(curve_shape_params, file='chikX/data/shape_params_PAHO_cases_adj.csv', row.names=F)

# curve_shape_params[,c('code', 's_l', 's_l_SE', 's_r', 's_r_SE')] %>%
#     mutate(slp = s_l_SE/s_l*100, srp = s_r_SE/s_r*100)



sample_curve = function(
        .curve_shape_params = curve_shape_params, 
        .amplitude = amplitude, 
        .outbreak_timing = outbreak_timing
        ) {
    sample_params = sample_n(.curve_shape_params, 1)
    # peak_time = sample_params$peak_time_abs #+ t_lag -- now using middle of 5 years
    new_s_l = rnorm(n = 1, mean = sample_params$s_l, sd = sample_params$s_l_SE*0.25)
    new_s_r = rnorm(n = 1, mean = sample_params$s_r, sd = sample_params$s_r_SE*0.25)
    # negative params not accepted 
    new_s_l = ifelse(new_s_l < 0, abs(new_s_l) - sample_params$s_l, new_s_l)
    new_s_r = ifelse(new_s_r < 0, abs(new_s_r) - sample_params$s_r, new_s_r)
    # <1 not accepted 
    # new_s_l = ifelse(new_s_l < 1, 1, new_s_l)
    # new_s_r = ifelse(new_s_r < 1, 1, new_s_r)
    # first simulate over about 5 years
    time_d = seq(from=0, to=5, length.out=5*365)
    outbreak_res = shin_curve(xs=time_d, amplitude=.amplitude, 
                              h_transl=2.5,#peak_time, 
                              s_l=new_s_l, s_r=new_s_r)
    # remove daily infections < 1  
    # round down to get integer infections 
    outbreak_res = ifelse(outbreak_res<1,0,outbreak_res) %>% floor 
    # keep only positive values and their corresponding times
    inds = which(outbreak_res > 0)
    # add an extra index if possible to reach 0 at the end 
    if (max(inds)<length(inds)) { inds = c(inds, (max(inds)+1)) }
    # subset positive daily infections
    outbreak_res = outbreak_res[inds]
    
    # account for the later start of the outbreak 
    t_lag = (.outbreak_timing/365.25) # convert outbreak start into years
    # total outbreak duration 
    #          # PAHO end time    # PAHO start time   # sim start time 
    # dt = sample_params$t_max - sample_params$t_min + t_lag
            # sim outbreak length in yr     # delayed start 
    dt = (length(outbreak_res) / 365.25) + t_lag 
    n_obs = dt*365
    # add 0s for days before outbreak start 
    outbreak_res = c(rep(0, times=.outbreak_timing), outbreak_res)
    # adjust time to match the length of the outbreak 
    time_d = seq(from=0, to=dt, length.out=length(outbreak_res))
    peak_time = time_d[which(outbreak_res == max(outbreak_res))]

    # this df is useful for diagnostics 
    simulation_info = data.frame(
        amplitude = .amplitude,
        h_transl = peak_time,
        s_l = new_s_l,
        s_r = new_s_r,
        dt = dt
        )
    # large list of different components returned  
    return(list(outbreak_res=outbreak_res, time_d=time_d, # results 
                simulation_info=simulation_info, # diagnostics 
                sample_params=sample_params)) # diagnostics
    
}

# amplitude=2000
# outbreak_timing = 365
# res1 = sample_curve()
# plot(res1[[2]], res1[[1]])
# 
# cbind(res1[[3]], res1[[4]])

f_sim = function(vec_catchments_i = vec_catchments_i, 
                 df_initialConditions_i = df_initialConditions_i, 
                 .simulation_i = simulation_i){
    # loop over all countries to which CHIK-X spread in a given simulation_i
    # for catchment_j in initialConditions_i, begin simulation
    # for(catchment_j in vec_catchments_i){
    list_diseaseX_i = map(vec_catchments_i, function(catchment_j){
        if (!is.null(catchment_j)) {
            # extract population size of catchment_j
            df_initialConditions_j = dplyr::filter(df_initialConditions_i, code == catchment_j)
            popSize = df_initialConditions_j$total_pop_size
            outbreak_timing = df_initialConditions_j$timing
            code = df_initialConditions_j$code
            country = df_initialConditions_j$country
            amplitude = df_initialConditions_j$amplitude
            
            
           outbreak_sim = sample_curve(.curve_shape_params = curve_shape_params,
                                       .amplitude = amplitude,
                                       .outbreak_timing = outbreak_timing) 
           outbreak_res = outbreak_sim$outbreak_res
           time_d = outbreak_sim$time_d
            
            # cumulative incidence 
            cum_U = MESS::auc(seq_along(outbreak_res),outbreak_res,
                              type='spline', subdivisions = 1e6)
            
            df_diseaseX_k = data.frame(
                time_years = time_d,
                daily_infections_sim = outbreak_res) %>% mutate(
                    country = country,
                    code = code,
                    timing = outbreak_timing,
                    simulation = .simulation_i,
                    IncCumul_U_final = cum_U
                )
            # comment out below to return only sim_res aka df_disease_k
            # supplementary table with params for debugging
            sim_params_info = list(
                simulation_info = cbind(
                    data.frame(
                        country = country,
                        code = code,
                        timing = outbreak_timing,
                        simulation = .simulation_i,
                        IncCumul_U_final = cum_U),
                    outbreak_sim$simulation_info
                    ),
                sampled_params = outbreak_sim$sample_params # from PAHO data
            )

            list_diseaseX_i_out = list(sim_res = df_diseaseX_k,
                                       sim_params_info = sim_params_info)
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
}





# ecdf() for cumulative distribution 

# test = do.call(rbind, list_diseaseX_i)
# 
# test2 = test%>%filter(vacc_strategy == "none")
# sum(test2$IncCumul_U_final)
# 
# ggplot(test_df%>%filter(country == "Nigeria"), aes(x = time_adj, y = E+I+VE+VI, colour = factor(vacc_strategy), linetype = factor(vacc_dosing)))+
#   geom_line()+
#   theme_light()+
#   facet_wrap(facets = vars(catchment), scales = "free_y")

