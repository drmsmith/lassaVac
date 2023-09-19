library(tidyverse)
library(deSolve)
library(ggsci)
library(scales)
library(cowplot)

fix_accents = F

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## Dataframe: catchment names, fix accents
df_district_names = read.csv("LassaX/data/catchments_zoonosis_lat_lon.csv")

if(fix_accents == T){
  df_district_names$NAME_1 <- gsub("\xe9", "é", df_district_names$NAME_1)
  df_district_names$NAME_1 <- gsub("\xfa", "ú", df_district_names$NAME_1)
  df_district_names$NAME_1 <- gsub("\xe8", "è", df_district_names$NAME_1)
  df_district_names$NAME_1 <- gsub("\xe1", "á", df_district_names$NAME_1)
  df_district_names$NAME_1 <- gsub("\xf4", "ô", df_district_names$NAME_1)
  df_district_names$NAME_1 <- gsub("Federal Capital Territory", "FCT", df_district_names$NAME_1)
}


vec_GID_0 = levels(factor(df_district_names$GID_0))
vec_GID_1 = levels(factor(df_district_names$GID_1))

df_country_names = df_district_names%>%
  group_by(GID_0, COUNTRY)%>%
  summarise(Population_raster = sum(Population_raster))%>%
  mutate(country = COUNTRY)

### colour palettes

cols_vacc_delay = c('#e41a1c','#ff7f00','#377eb8')

cols_vacc_efficacy = c('#1b9e77', '#7570b3', '#ff7f00')

cols_outbreak_detection = c("#e41a1c")



# function to estimate beta parameters
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


### function to estimate mean and SD from quartiles
### from Wan et al. 2014
# https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-135#Tab2
# This paper presents 3 scenarios (numerical quantities provided)
# Here consider scenario 3: IQR, median and N provided

# this approach requires a table of values corresponding to eta, which helps approximate sampel sd from IQR
# df_eta = read.csv("J:/methods_learning/distributions_from_quartiles_eta_values.csv")

# f_eta = function(n){
#   
#   if(n>50){eta = 1.34898}else{eta = as.numeric(c(df_eta%>%filter(Q==n))[2])}
#   
#   return(eta)
# }

# X = mean; S = standard deviation; q1 = 1st quartile; q3 = 3rd quartile; m = median; n = sample size

# function to derive mean and SD from quartiles and n
f_dist_from_quartiles = function(q1, m, q3, n){
  
  # approximate mean (per Wan et al.)
  X = (q1 + m + q3)/3
  
  # approximate sd (per Wan et al.)
  S = ((q3 - q1)/(f_eta(n)))
  
  # determine shape and scale parameters from X and S
  shape = (X/S)^2
  scale = S^2/X
  
  result = c('X' = X, 'S' = S, 'shape' = shape, 'scale' = scale)
  return(result)
}



# incubation period
par_alpha_shape = 11.1191707
par_alpha_rate = 1.084107
par_alpha = 1/(par_alpha_shape/par_alpha_rate) # duration of non-infectious exposed period

# infectious period
par_gamma_shape = 1.862467
par_gamma_rate = 0.164666
par_gamma = 1/(par_gamma_shape/par_gamma_rate) # duration of infectious period


###########################################
### Determine set of initial conditions ###
###########################################

### Define output set
# output_set = 1
output_set = 2


### Set 1 ###
# Long output, 1 simulation
# vaccine 50%, 70% or 90% effective against infection
# no immunity at outset (propImmun = 0)
# allocate vac to recovereds and susceptibles (parVacStrat = 2)
# 10% wastage of vaccine doses
# 2 year time horizon for each outbreak in each district
if(output_set == 1){
  output_format = "output_long"
  first_sim = 1
  n_simulations = 1 # a single simulation
  vec_vacc_eff = c(0.5, 0.7,0.9) # effective rate of vaccination
  par_propImmun = 0 # proportion immune upon simulation onset
  par_parVacStrat = 2 # vaccine allocation strategy (see ODEs)
  wastage = 0.1
  n_duration_j = 365*2 # duration of simulation within each district
}

### Set 2 ###
# Brief output, 100 simulations
# vaccine 90% effective against infection
# no immunity at outset (propImmun = 0)
# allocate vac to recovereds and susceptibles (parVacStrat = 2)
# 10% wastage of vaccine doses
# 2 year time horizon for each outbreak in each district
if(output_set == 2){
  output_format = "output_brief"
  first_sim = 1
  n_simulations = 5#100 # a single simulation
  vec_vacc_eff = c(0, 0.5, 0.7,0.9) # effective rate of vaccination
  par_propImmun = 0 # proportion immune upon simulation onset
  par_parVacStrat = 2 # vaccine allocation strategy (see ODEs)
  wastage = 0.1
  n_duration_j = 365*2 # duration of simulation within each district
}




#################
### load data ###
#################

### Ebola catchments
df_catchments_ebola = read.csv("LassaX/data/inputs_df_catchments_ebola.csv")

### Ebola Rt curves
list_Rt_ebola_i = loadRData("LassaX/data/inputs_list_Rt_curves.Rdata")

### Lassa catchments
df_catchments_lassa = read.csv("LassaX/data/catchments_zoonosis_lat_lon.csv",
                               stringsAsFactors = F)

### Lassa-X initial conditions
list_initial_conditions = loadRData("LassaX/data/inputs_list_initial_conditions.Rdata")

##################
### DRAWING RT ###
##################

### bin Ebola catchments into 3 groups based on population size
vec_catchments_ebola_Rt = c()
for(index_i in 1:length(list_Rt_ebola_i)){
  
  catchment_ebola_i = list_Rt_ebola_i[[index_i]][["district"]]
  population_i = list_Rt_ebola_i[[index_i]][["district"]]
  
  if(!catchment_ebola_i %in% vec_catchments_ebola_Rt){
    vec_catchments_ebola_Rt = c(vec_catchments_ebola_Rt, catchment_ebola_i)
  }
}

### bin Ebola catchments into 3 groups based on population size
df_catchments_ebola_Rt_curves = df_catchments_ebola%>%
  filter(District %in% vec_catchments_ebola_Rt)%>%
  mutate(Population = as.numeric(Population))%>%
  arrange(Population)%>%
  #mutate(row_number = row_number())%>%
  mutate(Population_bin = case_when(row_number() < 47/3 ~ 1,
                                    row_number() >= 47/3 & row_number() < 47*2/3 ~ 2,
                                    T ~ 3))

### bin Lassa catchments into 3 groups based on population size
df_catchments_lassa_binned = df_catchments_lassa%>%
  mutate(Population_raster = as.numeric(Population_raster))%>%
  arrange(Population_raster)%>%
  #mutate(row_number = row_number())%>%
  mutate(Population_bin = case_when(row_number() < nrow(df_catchments_lassa)/3 ~ 1,
                                    row_number() >= nrow(df_catchments_lassa)/3 & row_number() < nrow(df_catchments_lassa)*2/3 ~ 2,
                                    T ~ 3))

### What are the population size thresholds for these three bins?
popThreshold1 = 816000 
popThreshold2 = 2200000


