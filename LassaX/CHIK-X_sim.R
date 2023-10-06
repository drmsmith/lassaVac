library('tidyverse')

###############
# parallelise #
###############

# library('doParallel')
# library('foreach')
# library('progress')

# totalCores = detectCores()
# # cl <- makeCluster(totalCores[1]-1, type='SOCK')
# cl <- parallel::makeCluster(4, type='PSOCK')
# registerDoParallel(cl)
# clusterEvalQ(cl,  library('magrittr'))
# clusterEvalQ(cl,  library('tidyverse'))
# clusterEvalQ(cl,  library('deSolve'))
# clusterEvalQ(cl, source("housekeeping.R"))
# clusterEvalQ(cl, source("LassaX/DiseaseX_ODEs.R"))



### Burden data set by country with pop size 
# df catchments in lassaX
df_burden = read.csv('LassaX/data_chik/df_burden_with_pop_size_2015.csv')

df_burden$mean_incidence = df_burden$infections_mean / df_burden$total_pop_size
df_burden$max_incidence = df_burden$infections_max / df_burden$total_pop_size

### Probability of spillover in each catchment area
# proportional to total prevalence 
n_spillover = sum(df_burden$infections_mean)
### Add column for the proportion of all spillovers occurring in each catchment
df_burden$p_spillover = df_burden$infections_mean/n_spillover

#write.csv(df_burden, 'LassaX/data_chik/df_burden_with_pop_size_2015_spillover.csv')

# load mobility matrix for country codes with mobility data 
mat_mob_p = read.csv("LassaX/data_chik/mat_mob_prob.csv")
all_codes = colnames(mat_mob_p)
rownames(mat_mob_p) = all_codes

# update burden df because no mobility information available for 
# five regions (see mobility_provessing.R)
# French Guiana, French Polynesia, New Caledonia, Puerto Rico, South Sudan
df_burden = df_burden[df_burden$code %in% all_codes,]

# shape parameters to be sampled from 
shape_params_inc_per_capita = read.csv("LassaX/data_chik/shape_params_inc_per_capita.csv")

### initial conditions
list_initial_conditions = get(load("LassaX/data_chik/inputs_list_initial_conditions.RData"))





#####################
#  INITIAL PARAMS  # 
####################


n_sim = 10
output_set = 2 # 1 = long, 2 = brief

source('LassaX/utils.R')


################
### SIM LOOP ###
################

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = n_simulations, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar


### For each set of initial conditions
# .packages = c('magrittr', 'tidyverse', 'deSolve')
# foreach(simulation_i = icount(n_simulations), .combine = f(n_simulations)) %dopar% {
for(simulation_i in first_sim:n_simulations){
    setTxtProgressBar(pb, simulation_i)
    
    qounter = 0
    list_diseaseX_i = list()
    
    # select initialConditions for model run i
    df_initialConditions_i = list_initial_conditions[[simulation_i]]
    vec_catchments_i = levels(factor(df_initialConditions_i$code))
    
    # for catchment_j in initialConditions_i, begin simulation
    for(catchment_j in vec_catchments_i){
        
        # print(paste0("simulating lassa-X transmission in ", catchment_j))
        
        # extract population size of catchment_j
        df_initialConditions_j = dplyr::filter(df_initialConditions_i, code == catchment_j)
        popSize_j = first(df_initialConditions_j$total_pop_size)
        outbreak_timing = df_initialConditions_j$timing[1]
        
        # MEAN OR MAX INCIDENCE?
        #amplitude = df_burden$$mean_incidence[df_burden$code==catchment_j]
        # unofrm sample from max / min / mean 
        amplitude = sample(
            df_burden[df_burden$code=='VEN',
                      c('infections_mean', 'infections_min','infections_max')], 1)
        amplitude = unlist(unname(amplitude))
        
        # sample district from Ebola data based on population size
        # if-else popThreshold1 - else btw thresh1 and 2 - else large
        # for-loop to select Rt curve corresponding to Ebola catchment that 
        # matches population bin from focal Lassa catchment
        
        ### For each catchment, loop through vaccine scenarios  
        for(vacc_scenario_k in 1:nrow(df_initialConditions_j)){
            
            df_initialConditions_k = df_initialConditions_j[vacc_scenario_k,]
            
            infect0_k = df_initialConditions_k$initial_size
            timing_k = df_initialConditions_k$timing
            country_k = df_initialConditions_k$country
            code_k = df_initialConditions_k$code
            N_k = df_initialConditions_k$total_pop_size
            doses_k = df_initialConditions_k$par_doses
            vacc_timing_k = df_initialConditions_k$vacc_timing
            V0_k = df_initialConditions_k$V0
            Doses0_k = df_initialConditions_k$Doses0
            vacc_strategy_k = df_initialConditions_k$strategy
            vacc_dosing_k = df_initialConditions_k$dosing

            # loop over vaccine efficacies 
            for(par_vacc_eff in vec_vacc_eff){
                
                ### move up to compare the same outbreak at different vec effs 
                # 
                
                # update qounter to index results saved in list 
                qounter = qounter + 1

                # print statement to place ourselves
                # print(paste0("simulating vacc_strategy ", 
                #              vacc_strategy_k, " and dosing ",
                #              vacc_dosing_k, " with VE", par_vacc_eff))
                ### Launch ODEs with corresponding conditions
                # sample outbreak parameters 
                sample_params = sample_n(shape_params_inc_per_capita, 1)
                new_s_l = rnorm(1, sample_params$s_l, sample_params$s_l_SE)
                new_s_r = rnorm(1, sample_params$s_r, sample_params$s_r_SE)
                # negative params not accepted 
                new_s_l = ifelse(new_s_l < 0, abs(new_s_l) - sample_params$s_l, new_s_l)
                new_s_r = ifelse(new_s_r < 0, abs(new_s_r) - sample_params$s_r, new_s_r)
                # <1 not accepted 
                new_s_l = ifelse(new_s_l < 1, 1, new_s_l)
                new_s_r = ifelse(new_s_r < 1, 1, new_s_r)
                peak_time = sample_params$peak_time - sample_params$t_min
                dt = sample_params$t_max - sample_params$t_min
                n_obs = dt*365
                time_d = seq(from=0, to=dt, length.out=n_obs)
                
                
                outbreak_res = shin_curve(xs=time_d, amplitude=amplitude, 
                                          h_transl=peak_time, 
                                          s_l=new_s_l, s_r=new_s_r)
                
                # cases in vaccinated individuals 
                ######## coverage = (n_doses to date / pop size) * (1-wastage)
                # see report for obtaining vaccination scale up etc 
                # p_cases_vac_disease = cases * coverage * (1-vac_eff_disease)
                # # num cases in non vac 
                # cases_non_vac = cases * (1 - coverage) 
                # 
                # outbreak_vac = p_cases_vac_disease + cases_non_vac
                


                # # remove incidence < 1 person 
                # outbreak_res = ifelse(outbreak_res<1/popSize_j, 0, outbreak_res)
                # # rescale to match (discard 0s for outbreaks which start later)
                # start_ind = which(outbreak_res>0)[1] - timing_k
                # outbreak_res = outbreak_res[start_ind:n_obs]
                # time_d = time_d[1:(n_obs-start_ind+1)]
                # cumulative incidence 
                cum_U = sum(outbreak_res)

                df_diseaseX_k = data.frame(
                    country = country_k,
                    code = code_k,
                    timing = timing_k,
                    time_years = time_d,
                    cases_sim = outbreak_res,
                    infect0 = infect0_k,
                    simulation = simulation_i,
                    vacc_alloc = par_parVacStrat,
                    vacc_strategy = vacc_strategy_k,
                    vacc_dosing = vacc_dosing_k,
                    vaccEff = par_vacc_eff,
                    IncCumul_U_final = cum_U,
                    IncCumul_V_final = NA,
                    DosesCumul_final = NA)
                
                params_out = list(outbreak_res = outbreak_res,
                                  xs = time_d, amplitude = amplitude, 
                                  h_transl = peak_time, s_l = new_s_l, s_r = new_s_r,
                                  dt=dt, sample_params = sample_params)
                
                
                if(output_format == "output_long"){
                    # 
                    # list_diseaseX_i[[qounter]] = df_diseaseX_k%>%
                    #     mutate(country = country_k,
                    #            catchment = catchment_j,
                    #            time_adj = time + timing_k,
                    #            infect0 = infect0_k,
                    #            simulation = simulation_i,
                    #            vaccEff = par_vacc_eff)
                    list_diseaseX_i[[qounter]] = list(health_econ = df_diseaseX_k,
                                                     params = params_out)

                    
                }else{if(output_format == "output_brief"){
                    
                    # list_diseaseX_i[[qounter]] = df_diseaseX_k%>%
                    #     mutate(country = country_k,
                    #            catchment = catchment_j,
                    #            time_adj = time + timing_k,
                    #            infect0 = infect0_k,
                    #            simulation = simulation_i,
                    #            vaccEff = par_vacc_eff)%>%
                    #     group_by(country, catchment, infect0, vacc_alloc, 
                    #              vacc_strategy, vacc_dosing, simulation, vaccEff) %>%
                    #     summarise(IncCumul_U_final = max(IncCumul_U),
                    #               IncCumul_V_final = max(IncCumul_V),
                    #               DosesCumul_final = max(DosesCumul))
                    list_diseaseX_i[[qounter]] = list(health_econ = df_diseaseX_k, 
                                                      params = params_out)
                    
                }else{stop("wrong data output format specified")}}
            }
        }
    }
    
    save(list_diseaseX_i, file = paste0("LassaX/chik_res/list_diseaseX_i_outputSet_", 
                                        output_set, "_simulation_", simulation_i,".RData"))
    
    qounter = 0
    list_diseaseX_i = list()
}

close(pb)

# stopCluster(cl)



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

