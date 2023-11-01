#######################################
# READ IN DATA FED INTO SIMULATIONS #
#####################################

### ESTIMATED BURDEN DATA SET BY COUNTRY WITH POP SIZE
# df catchments in lassaX
# df contains info on country, code, estimtd infections (mean/min/max)
# p spillover = n_infections / total
# p establishment and peak size sampled from estimtd infections (mean/min/max)
df_burden = read.csv('data/df_burden_with_pop_size_2015_spillover.csv')

# LOAD MOBILITY MATRIX FOR COUNTRY CODES WITH MOBILITY DATA
# currently using a fudge factor 5
# probability of moving between any two locations
mat_mob_p = read.csv('data/df_mat_mob_prob_fudge.csv')
# ENSURE THAT THE ROWS ARE NAMED,
# AS THESE NAMES ARE USED BY THE FUNCTIONS SOURCED BELOW
all_codes = colnames(mat_mob_p)
rownames(mat_mob_p) = all_codes

# SHAPE PARAMETERS TO BE SAMPLED FROM
# based on PAHO outbreak data
# sampling parameters from curve fits to simulate outbreaks
curve_shape_params = read.csv("data/df_shape_params_PAHO_cases_adj.csv")


# utility funcs
source('model/utils.R')
# modelling spread
source('model/CHIK-X_spread.R')
# setting initial conditions
source('model/CHIK-X_init_conditions.R')
# modelling daily infections / sampling curve shapes
source('model/CHIK-X_sim.R')


# SOME OTHER PARAMETERS TO BE SET

set.seed(231023)
n_simulations = 100

# RUN SPREAD MODEL TO SIMULATE
# THE ORIGIN OF CHIK-X AND
# ITS POTNETIAL SPREAD TO NEW LOCATIONS
# runtime: takes about 3 min to run 100 sims
list_gravity_spread = f_gravity_model_run(
    # returns a list of matrices where rows = countries and cols = days
    .df_catchments = df_burden,  # p spillover and establishment
    .mat_mob = mat_mob_p,       # matrix of pairwise probability of movement
    .duration_spread = 365*2,   # duration of spread simulation in days
    .n_spread_matrices = n_simulations,
    .save_res = TRUE,           # will save a file named inputs_ls_spread.RData
    .dest_dir = 'data'          # in specified destination directory
    )


# RUN FUNCTION TO DETERMINE INITIAL CONDITIONS
# FOR EACH SIMULATION RUN
# runtime: pretty quick
list_initial_conditions = f_initialConditions_run(
    list_gravity_spread = list_gravity_spread,
    .save_res = TRUE,  # will save a file named inputs_ls_initial_conditions.RData
    .dest_dir = 'data' # in specified destination directory
    )


# ALTERNATIVELY, LOAD PRE-RUN SPREAD AND INITIAL CONDITIONS
# list_gravity_spread     = get(load('data/inputs_ls_spread.RData'))
# list_initial_conditions = get(load('data/inputs_ls_initial_conditions.RData'))


# FINALLY, RUN SIMULATION
# runtime: also pretty quick ~20 sec
if (!dir.exists('./res/individual_simulations')) {dir.create('./res/individual_simulations', recursive = T)}
f_sim_run(n_sim = n_simulations, .dest_dir = 'res/individual_simulations')


# unidentified error
# ! extremely bad integrand behaviour