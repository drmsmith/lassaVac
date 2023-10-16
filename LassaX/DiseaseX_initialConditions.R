################################
### MODEL INITIAL CONDITIONS ###
################################

df_district_names = read.csv("LassaX/data_chik/df_burden_with_pop_size_2015.csv")


# for any given m_spread as simulated through gravity model, function to establish corresponding initial conditions for ODEs

# there are seven initial conditions with this model 
# no vac, 3x vac rates, 2x vac efficiencies 



f_initialConditions = function(m_spread,
                               n_inits = 50,
                               n_subs = 5,
                               vec_par_doses_percent_annual = c(0.025,0.2,0.4)){
  
  ### Based on gravity spread, determine timing of outbreaks and vaccination, and doses of vaccine
  m_spread_conditions = matrix(NA, nrow = nrow(m_spread), ncol = 3)
  rownames(m_spread_conditions) = rownames(m_spread)
  colnames(m_spread_conditions) = c("code", "initial_size", "timing")
  
  ### assume initial sizes upon outbreak detection are exponentially decreasing from n = n_inits at outbreak onset to n = n_subs at 1 year
  rate_decline_inits = -log(n_subs/n_inits)/365
  
  ### Determine initial outbreak size and timing for each district's outbreak
  for(catchment_i in rownames(m_spread)){
    
    m_spread_i = m_spread[catchment_i,]
    
    timing_i = first(which(m_spread_i == 1))
    
    # determine expected initial size based on timing of outbreak
    initial_size_mean = n_inits*exp(-rate_decline_inits*timing_i)
    
    # draw initial size from Poisson distribution (replace with 1 if 0)
    if(is.na(initial_size_mean)){
      initial_size = NA
    }else{
      initial_size = rpois(1, initial_size_mean)
      if(initial_size == 0){initial_size = 1}
    }
    
    
    # update matrix with these values
    m_spread_conditions[catchment_i, "code"] = catchment_i
    m_spread_conditions[catchment_i, "initial_size"] = initial_size
    m_spread_conditions[catchment_i, "timing"] = timing_i
  }
  
  # create dataframe binding final updated m_spread_conditions with population size of each
  # select only the districts that have any transmission
  df_spread_conditions = as.data.frame(m_spread_conditions)%>%
    left_join(df_district_names %>% 
                dplyr::select(code, country, total_pop_size), by = "code") %>%
    filter(!is.na(initial_size)) %>%
    mutate(initial_size = as.numeric(initial_size),
           timing = as.numeric(timing))
  
  # ### create dataframe combining all initial conditions
  # df_runawayOutbreaks_catchments_doses = data.frame()
  
  # ### no vaccine
  # df_runawayOutbreaks_catchments_noVaccine = df_spread_conditions%>%
  #   mutate(dosing = 0,
  #          par_doses = 0,
  #          strategy = "none",
  #          vacc_timing = Inf,
  #          V0 = 0,
  #          Doses0 = 0)
  # 
  # ### with vaccine: base scenarios
  # for(par_doses_percent_i in vec_par_doses_percent_annual){
  #   
  #   df_runawayOutbreaks_catchments_doses_i = df_spread_conditions%>%
  #     mutate(dosing = par_doses_percent_i,
  #            par_doses = total_pop_size*par_doses_percent_i/365)
  #   
  #   df_runawayOutbreaks_catchments_doses = rbind(df_runawayOutbreaks_catchments_doses,
  #                                                df_runawayOutbreaks_catchments_doses_i)
  # }
  # 
  # ### with vaccine: update timing and initial conditions
  # # 100d delay to vaccination
  # df_runawayOutbreaks_catchments_Vaccine100d = df_runawayOutbreaks_catchments_doses%>%
  #   mutate(strategy = "same_everywhere_100d",
  #          vacc_timing = case_when(100 < timing ~ 0,
  #                                  T ~ 100 - timing),
  #          V0 = case_when(100 > timing ~ 0,
  #                         T ~ par_doses * (timing - 100)),
  #          Doses0 = case_when(100 > timing ~ 0,
  #                             T ~ par_doses * (timing - 100)))
  # 
  # # 160d delay to vaccination
  # df_runawayOutbreaks_catchments_Vaccine160d = df_runawayOutbreaks_catchments_doses%>%
  #   mutate(strategy = "same_everywhere_160d",
  #          vacc_timing = case_when((100+60) < timing ~ 0,
  #                                  T ~ (100+60) - timing),
  #          V0 = case_when((100+60) > timing ~ 0,
  #                         T ~ par_doses * (timing - (100+60))),
  #          Doses0 = case_when((100+60) > timing ~ 0,
  #                             T ~ par_doses * (timing - (100+60))))
  # 
  # ### Combine all 3
  # df_runawayOutbreaks_catchmentsFinal = rbind(rbind(df_runawayOutbreaks_catchments_noVaccine,
  #                                                   df_runawayOutbreaks_catchments_Vaccine100d),
  #                                             df_runawayOutbreaks_catchments_Vaccine160d)
  

  # return(df_runawayOutbreaks_catchmentsFinal)
  return(df_spread_conditions)
}

############
### TEST ###
############

# m_spread1 = f_gravity_model()
# which(m_spread1[,365*2]==0)
# m_spread2 = f_gravity_model()
# 
# df_initialConditions1 = f_initialConditions(m_spread1)
# df_initialConditions2 = f_initialConditions(m_spread2)


########################################################################
### GENERATE INITIAL CONDITIONS FROM SPREAD MATRICES AND SAVE OUTPUT ###
########################################################################

### Load gravity spread list needed to inform initial conditions
# list_gravity_spread = get(load("LassaX/data_chik/inputs_list_gravity_spread.RData"))
list_gravity_spread = get(load("LassaX/data_chik/inpts_ls_spread_updtd.RData"))

### Determine initial conditions for each element of list_gravity_spread
list_initial_conditions = list()
for(gravity_spread_i in 1:length(list_gravity_spread)) {
  print(
    paste0(
      "calculating initial conditions for gravity model run ",
      gravity_spread_i,
      " of ",
      length(list_gravity_spread)
    )
  )
  
  list_initial_conditions[[gravity_spread_i]] = f_initialConditions(list_gravity_spread[[gravity_spread_i]])
  
}

### Save initial conditions
# save(list_initial_conditions, file = "LassaX/data_chik/inputs_list_initial_conditions.RData")
save(list_initial_conditions, file = "LassaX/data_chik/inputs_ls_initial_conditions_updt.RData")

## compare to lassa (called ol_list (which is gone))
old_list = get(load(file = "LassaX/data/inputs_list_gravity_spread.RData"))
load(file = "LassaX/data/inputs_list_initial_conditions.RData")

# check different values and their relative occurrence 
map(list_initial_conditions, function(.x) .x$Doses0) %>% unlist %>% unique %>% sort
list_initial_conditions[[3]]


# or old_list
spread_sums = map(list_gravity_spread, function(.x) {
  spread_sums = apply(.x, 1, function(row) sum(row!=0)) 
  spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% print 
} )

map(list_gravity_spread, function(.x) {
  spread_sums = apply(.x, 1, function(row) sum(row!=0)) 
  spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% length %>% print 
} ) %>% unlist %>% table

countries_spread = spread_sums %>% unlist %>% names(.) %>% table %>% sort(decreasing = T) 
length(countries_spread)


plot(countries_spread, ylim=c(0,30), srt=270, xlab='')
text(countries_spread+3, labels=names(countries_spread), srt=75, col='red')

spread_sums = apply(old_list[[2]], 1, function(row) sum(row!=0)) 
spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% print 
spread_sums[spread_sums!=0] %>% length

list_initial_conditions[[1]] 