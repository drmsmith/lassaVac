###################################
#### UN-adjusted worldpop data ####
###################################

# download 2015, 1km aggregated 
# UN-adjusted worldpop data 
source('preprocessing/UN_worldpop/download_worldpop.R')
# sum over country rasters to obtain population size 
# add to df_burden_estimates_cepi 
source('preprocessing/UN_worldpop/process_worldpop.R')



#######################
#### Mobility data ####
#######################

# reformat and subset raw mobility data 
source('preprocessing/mobility_and_p_spillover/mobility_processing.R')
# calculate probability of spread
source('preprocessing/mobility_and_p_spillover/p_spillover.R')
# calculate probability of moving between two countries 
source('preprocessing/mobility_and_p_spillover/mat_mob_prob_fudge.R')



########################
#### PAHO case data ####
########################

# reformat PAHO monthly new cases report data 
#### will throw a warning about Saint Martin - ignore 
source('preprocessing/PAHO_curve_fitting/paho_data_long_to_wide.R')

#########################################################
####################### IMPORTANT #######################
# at this stage ensure that a copy of                   #
# PAHO_case_data_2014-2017/df_PAHO_long_subset.csv      #
# with an additional column `outbreak` indicating       #
# outbreak start-end dates (s and e respectively)       #
# exists and is pointed at appropriately                #
# in the file `process_outbreaks.R`                     #
#########################################################

# subset designated outbreaks in preparation for fitting 
source('preprocessing/PAHO_curve_fitting/process_outbreaks.R')
# fit hyperbolic curve using nls()
#### will throw some errors when fitting fails 
#### but results will still be saved for successful fits  
source('preprocessing/PAHO_curve_fitting/fit_hyperbolic_shapes.R')


######################
# DATA FILES CREATED # 
######################

# finally, the `./data` dir should contain the following files: 
#### these files are essential for the simulations to run! ####
# │
# └────data
#        df_burden_with_pop_size_2015_spillover.csv
#        df_mat_mob_prob_fudge.csv
#        df_shape_params_PAHO_cases_adj.csv


# although not exact, for reference, 
# the `./preprocessing/data` dir should be similar to the following:  
# │
# │   df_burden_with_pop_size_2015.csv
# │   df_countries_burden_estimates_cepi.csv
# │   KCMD_EUI_GMP_Estimated_trips.csv
# │   mat_mobility_2015.csv
# │   PAHO_data_raw_wide.csv
# │   
# ├───2015_UNadj_worlpop_data
# │       ....tif
# │       ....tif
# │       
# └───PAHO_case_data_2014-2017
#         df_PAHO_long_full.csv
#         df_PAHO_long_subset.csv
#         df_PAHO_long_subset_outbreaks.csv
#         df_PAHO_long_subset_outbreaks_with_zeros.csv
#         df_PAHO_outbreaks_manual_adj.csv
#         ls_PAHO_long_subset.RData
#         ls_PAHO_outbreaks_manual_adj.RData
        