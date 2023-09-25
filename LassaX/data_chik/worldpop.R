library('tidyverse')
library('countrycode')
library('raster')
library('terra')

################################################
# select countries whose population to extract #
################################################

# burden estimate data from Henrik 
df_burden_estimates = read.csv('LassaX/data_chik/df_countries_burden_estimates_cepi.csv')

# keep only countries where transmission is estimated to occur
df_burden_est_sub = df_burden_estimates %>% filter(classification != 'NoTransmission') %>%
    dplyr::select(., country, who_region, classification, infections_mean, 
                  infections_min, infections_max)

# United_States_of_America name isn't mapped onto USA by countrycode()
df_burden_est_sub$country = ifelse(df_burden_est_sub$country=='United_States_of_America',
                                   'USA', df_burden_est_sub$country)

# map country names onto country codes for query  
ccodes = countrycode(df_burden_est_sub$country,
                     origin = 'country.name', destination = 'iso3c') %>% 
    data.frame(country=df_burden_est_sub$country, code=.)

# adding country code to burden estimate data frame 
df_burden_est_sub = left_join(ccodes, df_burden_est_sub, by='country')



#################################################################
# download worldpop un-adjusted data from 2016 (1km aggregated) #
#################################################################
source('LassaX/data_chik/utils.R')
downlds = map(df_burden_est_sub$code, 
              function(.x) {
                  downld_worldpop_UNadj1km(country=.x, year='2015',
                                           local_filepath='LassaX/data_chik/worldpop_2015/')
                  }, .progress = T) %>% unlist 


########## 2016 ################################################
# codes_errors = c('BRA-brazil', 'COG-rep.congo', 'USA')
# links_errors = c(
#     'https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2016/BRA/bra_ppp_2016_1km_Aggregated_UNadj.tif',
#     'https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2016/COG/cog_ppp_2016_1km_Aggregated_UNadj.tif', 
#     'https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2016/USA/usa_ppp_2016_1km_Aggregated_UNadj.tif'
# )
# 
# downld_worldpop_UNadj1km(country = 'USA', local_filepath = 'LassaX')



###############################################################
# sum across raster to get country-wide population estimates #
###############################################################
# again remember to change year / dir name 

# raster package works better than terra
tif_files = list.files('LassaX/data_chik/worldpop_2015/', '.tif') #%>%
#     str_remove_all(tif_fies, '_ppp_2016_UNadj.tif') %>% toupper
tif_paths = tif_files %>% paste('LassaX/data_chik/worldpop_2015/', ., sep='')

r_sums = map(tif_paths, 
              function(.x) {
                  r <- raster::raster(.x)
                  rs <- raster::cellStats(r, "sum") # change year 
                  code = str_remove_all(.x, '2015.tif') # _ppp_2016_UNadj
                  code = str_remove_all(code, 'LassaX/data_chik/worldpop_2015/')
                  code = toupper(code)
                  return(list(code, rs))
                  },
              .progress = T) %>% unlist

# convert to df with appropriate colnames 
worldpops = matrix(r_sums, ncol = 2, byrow = T) %>% as.data.frame %>%
    rename(code=V1, total_pop_size=V2) 

# add pop sizes to estim burden data 
complete_data = right_join(df_burden_est_sub, worldpops, by = 'code')
complete_data$total_pop_size = as.double(complete_data$total_pop_size)

# get prevalence 
#complete_data$prevalence = complete_data$infections_mean / complete_data$total_pop_size


# complete_data$p_spillover %>% plot
# complete_data$p_spillover[complete_data$p_spillover<0.005]

# remember to change year 
write.csv(complete_data, row.names = F, 
          file='LassaX/data_chik/df_burden_with_pop_size_2015.csv')
