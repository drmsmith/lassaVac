library('magrittr')
library('tidyverse')
library('deSolve')


source("housekeeping.R")
source("LassaX/DiseaseX_ODEs.R")

###############
# parallelise #
###############

library('doParallel')
library('foreach')
library('progress')

# Progress combine function
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


totalCores = detectCores()
# cl <- makeCluster(totalCores[1]-1, type='SOCK')
cl <- parallel::makeCluster(4, type='PSOCK')
registerDoParallel(cl)
clusterEvalQ(cl,  library('magrittr'))
clusterEvalQ(cl,  library('tidyverse'))
clusterEvalQ(cl,  library('deSolve'))
clusterEvalQ(cl, source("housekeeping.R"))
clusterEvalQ(cl, source("LassaX/DiseaseX_ODEs.R"))



################
### ODE LOOP ###
################

### For each set of initial conditions
# .packages = c('magrittr', 'tidyverse', 'deSolve')
foreach(simulation_i = icount(n_simulations), .combine = f(n_simulations)) %dopar% {
# for(simulation_i in first_sim:n_simulations){
  
  qounter = 0
  list_diseaseX_i = list()
  
  # select initialConditions for model run i
  df_initialConditions_i = list_initial_conditions[[simulation_i]]
  vec_catchments_i = levels(factor(df_initialConditions_i$GID_1))
  
  # for catchment_j in initialConditions_i, begin simulation
  for(catchment_j in vec_catchments_i){
    
    # print(paste0("simulating lassa-X transmission in ", catchment_j))
    
    # extract population size of catchment_j
    df_initialConditions_j = df_initialConditions_i %>% 
      dplyr::filter(GID_1 == catchment_j)
    popSize_j = first(df_initialConditions_j$Population_raster)
    
    # sample district from Ebola data based on population size
    if(popSize_j < popThreshold1){
      
      # small pop size
      df_catchments_ebola_j = df_catchments_ebola_Rt_curves %>%
        dplyr::filter(Population_bin == 1)
      
      rand_j = sample(1:nrow(df_catchments_ebola_j), 1)
      
      catchment_Rt_j = df_catchments_ebola_j$District[rand_j]
      
    }else{
      if(popSize_j >= popThreshold1 & popSize_j < popThreshold2){
        
        # intermediate pop size
        df_catchments_ebola_j = df_catchments_ebola_Rt_curves %>% 
          dplyr::filter(Population_bin == 2)
        
        rand_j = sample(1:nrow(df_catchments_ebola_j), 1)
        
        catchment_Rt_j = df_catchments_ebola_j$District[rand_j]
        
      }else{
        
        # large pop size
        df_catchments_ebola_j = df_catchments_ebola_Rt_curves %>% 
          dplyr::filter(Population_bin == 3)
        
        rand_j = sample(1:nrow(df_catchments_ebola_j), 1)
        
        catchment_Rt_j = df_catchments_ebola_j$District[rand_j]
        
      }
    }
    
    # select Rt curve corresponding to Ebola catchment that 
    # matches population bin from focal Lassa catchment
    for(index_Rt_catchment in 1:length(list_Rt_ebola_i)){
      # which entry in list_Rt_ebola_i corresponds with the chosen catchment
      if(list_Rt_ebola_i[[index_Rt_catchment]][["district"]] == catchment_Rt_j){ 
        index_list_Rt_ebola_i = index_Rt_catchment}
    }
    
    m_Rt_curve_j = list_Rt_ebola_i[[index_list_Rt_ebola_i]][["m_Rt"]]
    Rt_j = m_Rt_curve_j[sample(1:nrow(m_Rt_curve_j), 1),5:ncol(m_Rt_curve_j)]
    f_interpolate_Rt = approxfun(Rt_j, rule = 2)
    
    
    
    ### For each catchment, loop through vaccine scenarios  
    for(vacc_scenario_k in 1:nrow(df_initialConditions_j)){
      
      df_initialConditions_k = df_initialConditions_j[vacc_scenario_k,]
      
      infect0_k = df_initialConditions_k$initial_size
      timing_k = df_initialConditions_k$timing
      country_k = df_initialConditions_k$COUNTRY
      N_k = df_initialConditions_k$Population_raster
      doses_k = df_initialConditions_k$par_doses
      vacc_timing_k = df_initialConditions_k$vacc_timing
      V0_k = df_initialConditions_k$V0
      Doses0_k = df_initialConditions_k$Doses0
      vacc_strategy_k = df_initialConditions_k$strategy
      vacc_dosing_k = df_initialConditions_k$dosing
      
      for(par_vacc_eff in vec_vacc_eff){
        
        # update qounter to index results saved in list 
        qounter = qounter + 1
        
        # print statement to place ourselves
        # print(paste0("simulating vacc_strategy ", vacc_strategy_k, " and dosing ", vacc_dosing_k, " with VE", par_vacc_eff))
        
        
        ### Launch ODEs with corresponding conditions
        df_diseaseX_k = f_simulate_ODEs(time = seq(0,n_duration_j,1),
                                        states_init = c(S = N_k*(1-par_propImmun) - V0_k*(1-wastage) - infect0_k, 
                                                        E = infect0_k/2, 
                                                        I = infect0_k/2, 
                                                        R=N_k*par_propImmun, 
                                                        V = V0_k*(1-wastage),
                                                        VE = 0,
                                                        VI = 0,
                                                        VR = 0,
                                                        IncCumul_U = infect0_k,
                                                        IncCumul_V = 0,
                                                        DosesCumul = Doses0_k),
                                        ODEs = f_ODEs_SEIR_Rt,
                                        parameters = c(alpha = par_alpha, 
                                                       gamma = par_gamma,
                                                       vacc_eff = par_vacc_eff,
                                                       parVacStrat = par_parVacStrat, 
                                                       t_vacc_start = vacc_timing_k, 
                                                       doses = doses_k),
                                        method_simu = "bdf")%>%
          mutate(vacc_alloc = par_parVacStrat,
                 vacc_strategy = vacc_strategy_k,
                 vacc_dosing = vacc_dosing_k)
        
        if(output_format == "output_long"){
          
          list_diseaseX_i[[qounter]] = df_diseaseX_k%>%
            mutate(country = country_k,
                   catchment = catchment_j,
                   time_adj = time + timing_k,
                   infect0 = infect0_k,
                   simulation = simulation_i,
                   vaccEff = par_vacc_eff)
          
        }else{if(output_format == "output_brief"){
          
          list_diseaseX_i[[qounter]] = df_diseaseX_k%>%
            mutate(country = country_k,
                   catchment = catchment_j,
                   time_adj = time + timing_k,
                   infect0 = infect0_k,
                   simulation = simulation_i,
                   vaccEff = par_vacc_eff)%>%
            group_by(country, catchment, infect0, vacc_alloc, 
                     vacc_strategy, vacc_dosing, simulation, vaccEff) %>%
            summarise(IncCumul_U_final = max(IncCumul_U),
                      IncCumul_V_final = max(IncCumul_V),
                      DosesCumul_final = max(DosesCumul))
          
          
        }else{stop("wrong data output format specified")}}
      }
    }
  }
  
  save(list_diseaseX_i, file = paste0("res/list_diseaseX_i_outputSet_", output_set, "_simulation_", simulation_i,".Rdata"))
  
  qounter = 0
  list_diseaseX_i = list()
}

stopCluster(cl)


# test = do.call(rbind, list_diseaseX_i)
# 
# test2 = test%>%filter(vacc_strategy == "none")
# sum(test2$IncCumul_U_final)
# 
# ggplot(test_df%>%filter(country == "Nigeria"), aes(x = time_adj, y = E+I+VE+VI, colour = factor(vacc_strategy), linetype = factor(vacc_dosing)))+
#   geom_line()+
#   theme_light()+
#   facet_wrap(facets = vars(catchment), scales = "free_y")

