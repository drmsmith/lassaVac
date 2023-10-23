#######################################
# READ IN DATA FED INTO SIMULATIONS #
#####################################

### ESTIMATED BURDEN DATA SET BY COUNTRY WITH POP SIZE  
# df catchments in lassaX
df_burden = read.csv('chikX/data/df_burden_with_pop_size_2015_spillover.csv')

# LOAD MOBILITY MATRIX FOR COUNTRY CODES WITH MOBILITY DATA
# currently using a fudge factor one
mat_mob_p = read.csv('chikX/data/mat_mob_prob_fudge.csv')
# ENSURE THAT THE ROWS ARE NAMED, 
# AS THESE NAMES ARE USED BY THE FUNCTIONS SOURCED BELOW  
all_codes = colnames(mat_mob_p)
rownames(mat_mob_p) = all_codes

# SHAPE PARAMETERS TO BE SAMPLED FROM
curve_shape_params = read.csv("chikX/data/shape_params_PAHO_cases_adj.csv")


# utility funcs 
source('chikX/model/utils.R')
# modelling spread
source('chikX/model/CHIK-X_spread.R')
# setting initial conditions
source('chikX/model/CHIK-X_init_conditions.R')
# modelling daily infections / sampling curve shapes
source('chikX/model/CHIK-X_sim.R')



n_simulations = 100

# RUN SPREAD MODEL TO SIMULATE
# THE ORIGIN OF CHIK-X AND
# ITS POTNETIAL SPREAD TO NEW LOCATIONS
# runtime: takes about 3 min to run 100 sims
list_gravity_spread = f_gravity_model_run(
    .df_catchments = df_burden,
    .n_spread_matrices = n_simulations,
    .save_res = TRUE,
    .dest_dir = 'chikX/data'
    )


# RUN FUNCTION TO DETERMINE INITIAL CONDITIONS
# FOR EACH SIMULATION RUN
# runtime: pretty quick
list_initial_conditions = f_initialConditions_run(
    list_gravity_spread = list_gravity_spread,
    .save_res = TRUE,
    .dest_dir = 'chikX/data'
    )


# ALTERNATIVELY, LOAD PRE-RUN SPREAD AND INITIAL CONDITIONS 
# list_gravity_spread     = get(load('chikX/data/inputs_ls_spread.RData'))
# list_initial_conditions = get(load('chikX/data/inputs_ls_initial_conditions.RData'))


# FINALLY, RUN SIMULATION
# runtime: also pretty quick ~20 sec 
f_sim_run(n_sim = n_simulations, .dest_dir = 'chikX/res')


# unidentified error 
# ! extremely bad integrand behaviour

           