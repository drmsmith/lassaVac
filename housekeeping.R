library(tidyverse)
library(deSolve)
library(ggsci)
library(scales)
library(cowplot)

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
