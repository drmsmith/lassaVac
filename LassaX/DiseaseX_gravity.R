library('tidyverse')
library('geosphere')

#####################
### GRAVITY MODEL ###
#####################

df_burden = read.csv('LassaX/data_chik/df_burden_with_pop_size_2015.csv')
### Probability of spillover in each catchment area
n_spillover = sum(df_burden$infections_mean)
### Add column for the proportion of all spillovers occurring in each catchment
df_burden$p_spillover = df_burden$infections_mean/n_spillover

### Function for gravity model (daily probabiltiy of spread over assumed duration of spread)
f_gravity_model = function(duration_spread = 365*2, doPrint = F){
  
  # Matrix of spread across catchments (to be updated in loop with gravity model)
  
  m_spread = matrix(0, nrow = nrow(df_burden), ncol = duration_spread)
  colnames(m_spread) = 1:duration_spread
  rownames(m_spread) = df_burden$code
  
  # identify first catchment, add to vector of infected catchments, update spread matrix
  catchment0 = sample(df_burden$code, size = 1, 
                      prob = df_burden$p_spillover)
  
  vec_catchments_infected = catchment0
  
  # fill rest of row with 1 (assumption is that catchments do not return to susceptible)
  m_spread[catchment0,] = 1
  
  # go through outbreak and update matrices day-by-day
  for(date_i in 1:duration_spread){
    if(doPrint == T){print(paste0("evaluating spread on day ", date_i))}
    
    # for each catchment currently infected
    for(catchment_infect_j in vec_catchments_infected){
      #print(paste0("on day ", date_i, ", evaluating potential spread from ", catchment_infect_j))
      
      # evaluate probability of spread to all catchments not currently infected
      for(catchment_suscept_k in df_catchments_lassa$GID_1[!df_catchments_lassa$GID_1 %in%
                                                           vec_catchments_infected]){
        
        # gravity term --> MVT MATRIX 
        # p moving a->b
        # vaccine code (no vaccination for now -- keep )
        infect_i_j = rbinom(1, 1, m_gravity[catchment_infect_j, catchment_suscept_k])
        
        # if infected, update infected catchments and corresponding m_spread matrix
        if(infect_i_j == 1){
          #print(paste0("on day ", date_i, " there was spread from ", catchment_infect_j, " to ", catchment_suscept_k))
          
          vec_catchments_infected = append(vec_catchments_infected, catchment_suscept_k)
          
          m_spread[catchment_suscept_k, date_i:ncol(m_spread)] = 1
        }
      }
    }
  }
  
  return(m_spread)
}


### TEST
# m_spread = f_gravity_model(doPrint = T)
# which(m_spread[,1] == 1)
# which(m_spread[,365] == 1)
# which(m_spread[,365*2] == 1)
# which(m_spread[,365*2] == 0)


###################################
### RUN N TIMES AND SAVE OUTPUT ###
###################################

n_spread_matrices = 100

list_gravity_spread = list()

for(gravity_i in 1:n_spread_matrices){
  
  print(paste0("on stochastic gravity run ", gravity_i, " of ", n_spread_matrices))
  
  m_spread = f_gravity_model()
  
  list_gravity_spread[[gravity_i]] = m_spread
  
}

# save(list_gravity_spread, file = "LassaX/data/inputs_list_gravity_spread.RData")


