library(tidyverse)
library(geosphere)

#####################
### GRAVITY MODEL ###
#####################

### GEOSPATIAL DATA
# Admin1 longitude and latitude entered manually into original "catchments" CSV ("data/incidence_ADM_1_PB_2_JT1.csv")
# data from https://www.geonames.org/NG/administrative-division-nigeria.html

df_catchments_lassa = read.csv("LassaX/data/catchments_zoonosis_lat_lon.csv",
                               stringsAsFactors = F)

### Probability of spillover in each catchment area
n_spillover = sum(df_catchments_lassa$no_reversion_incidence_raster)

### Add column for the proportion of all spillovers occurring in each catchment
df_catchments_lassa$p_spillover = df_catchments_lassa$no_reversion_incidence_raster/n_spillover


### Matrices of n x n catchments describing:
### Euclidean distances between all catchments
### Gravity terms, accounting for border crossing 

m_euclidean = matrix(NA, nrow = nrow(df_catchments_lassa), ncol = nrow(df_catchments_lassa))
colnames(m_euclidean) = df_catchments_lassa$GID_1
rownames(m_euclidean) = df_catchments_lassa$GID_1

m_gravity = m_euclidean

# gravity parameters direct from Kramer et al, Royal Society Open Science:
# https://royalsocietypublishing.org/doi/full/10.1098/rsos.160294

beta0 = 5.166
beta1 = 157.1
beta2 = 0.189
beta3 = 0.507

### Loop across all catchments
for(catchment_i in df_catchments_lassa$GID_1){
  print(paste0("on catchment ", catchment_i))
  
  df_catchments_lassa_i = df_catchments_lassa%>%dplyr::filter(GID_1 == catchment_i)
  LAT_i = df_catchments_lassa_i$LAT
  LON_i = df_catchments_lassa_i$LON
  
  for(catchment_j in df_catchments_lassa$GID_1){
    
    df_catchments_lassa_j = df_catchments_lassa%>%dplyr::filter(GID_1 == catchment_j)
    LAT_j = df_catchments_lassa_j$LAT
    LON_j = df_catchments_lassa_j$LON
    
    # # Spherical distance in metres
    # dist_i_j = distm(c(LON_i, LAT_i), c(LON_j, LAT_j), fun = distHaversine)
    
    # Euclidean distance
    dist_i_j = sqrt((LON_i - LON_j)^2 + (LAT_i - LAT_j)^2)
    
    # Population product
    pi = df_catchments_lassa$Population_raster[which(df_catchments_lassa$GID_1 == catchment_i)]
    pj = df_catchments_lassa$Population_raster[which(df_catchments_lassa$GID_1 == catchment_j)]
    pi_pj =  pi * pj 
    
    # Gravity term (depends whether districts in same country)
    if(substr(catchment_i, 1, 3) == substr(catchment_j, 1, 3)){
      
      gravity_ij = 1/(1+(exp(beta0 + beta1*dist_i_j/(pi_pj**beta2))))
      
    }else{
      
      gravity_ij = beta3*1/(1+(exp(beta0 + beta1*dist_i_j/(pi_pj**beta2))))
      
    }
    
    ### Fill matrices 
    m_euclidean[catchment_i, catchment_j] = dist_i_j
    m_gravity[catchment_i, catchment_j] = gravity_ij
  }
}

# write.csv(m_euclidean, "data/mat_euclidean.csv")
# write.csv(m_gravity, "data/mat_gravity.csv")

m_euclidean = read.csv("LassaX/data/mat_euclidean.csv", row.names = 1)

m_gravity = read.csv("LassaX/data/mat_gravity.csv", row.names = 1)

### Function for gravity model (daily probabiltiy of spread over assumed duration of spread)
f_gravity_model = function(duration_spread = 365*2, doPrint = F){
  
  # Matrix of spread across catchments (to be updated in loop with gravity model)

  m_spread = matrix(0, nrow = nrow(df_catchments_lassa), ncol = duration_spread)
  colnames(m_spread) = 1:duration_spread
  rownames(m_spread) = df_catchments_lassa$GID_1
  
  # identify first catchment, add to vector of infected catchments, update spread matrix
  catchment0 = sample(df_catchments_lassa$GID_1, size = 1, 
                      prob = df_catchments_lassa$p_spillover)
  
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
        
        # gravity term
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


