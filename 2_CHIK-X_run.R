#####################################
# READ IN DATA FED INTO SIMULATIONS #
#####################################

# ### ESTIMATED BURDEN DATA SET BY COUNTRY WITH POP SIZE
# # df catchments in lassaX
# # df contains info on country, code, estimtd infections (mean/min/max)
# # p spillover = n_infections / total
# # p establishment and peak size sampled from estimtd infections (mean/min/max)
# df_burden = read.csv('data/df_burden_with_pop_size_2015_spillover.csv')

# # LOAD MOBILITY MATRIX FOR COUNTRY CODES WITH MOBILITY DATA
# # currently using a fudge factor 5
# # probability of moving between any two locations
# mat_mob_p = read.csv('data/df_mat_mob_prob_fudge.csv')
# # ENSURE THAT THE ROWS ARE NAMED,
# # AS THESE NAMES ARE USED BY THE FUNCTIONS SOURCED BELOW
# all_codes = colnames(mat_mob_p)
# rownames(mat_mob_p) = all_codes

# # OUTBREAK CURVE SHAPES TO BE SAMPLED FROM
# # based on PAHO outbreak data
# # sampling parameters from curve fits to simulate outbreaks
# curve_shape_params = read.csv("data/df_shape_params_PAHO_cases_adj.csv")


# utility funcs
source('model/utils.R')
# modelling daily infections / sampling curve shapes
source('model/CHIK-X_sim.R')


# define simulation parameters
set.seed(31124)
# unlink(res_dir)
##### THESE RESULTS HAVE 
##### OUTBREAK SIZE ~ 0.2-0.6 * POP SIZE 
# res_dir = './res/imp_mod_2023_suit_incidence'     # save results in this dir 
# res_dir = './res/slow_spread'     # save results in this dir 
### fact_f and fact_k   100 sims wth 0.05 and 10 
### fact_f and fact_k   100 sims with 0.1 and 12 

##### NOW OUTBREAK SIZE = ANNUAL INCIDENCE 
# unlink(res_dir, recursive = T)
res_dir = 'res/tmp_outbreak_refactor'
# tmp_outbreak_refactor replicates unfactored code 

n_simulations = 100                      # number of simulations
duration_spread <- 365*2                # simulation time span
infectiousness_duration <- 7    # infected individual can infect days

# these factors transform 
# pop-weighted suitability
# f * suit ^ k 
fact_f = 1
fact_k = 1 # 0.01 this effectively makes p_outbreak=1 
## 0.1 and 10



simulation_hyperparameters = list(
    res_dir = res_dir, 
    n_simulations = n_simulations, 
    duration_spread = duration_spread, 
    infectiousness_duration = infectiousness_duration,
    fact_f = fact_f,
    fact_k = fact_k
)


# suitability (instead of estd. infections)
# df_suit_means_pop_wght_pop_size_who_p_spillover
    # suitability info for 220 countries -- 2020 data 
df_burden <- read.csv("data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv")


# mobility data (daily trips between src and dest)
# mobility info for 188 countries
mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
all_codes <- colnames(mat_mob_daily_trips)
rownames(mat_mob_daily_trips) <- all_codes

# ensure mobility data and suitability data are matched 
# since there are fewer countries on the mobility data set 
# 182 vs 188 (suit vs mobility)
# final number of countries is 178 
# 6 countries have mobility but not suitability data
# 4 countries have suitability but no mobility data 
df_burden <- filter(df_burden, code %in% all_codes) %>% drop_na()



# paho case data
df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
paho_codes <- df_paho_outbreak_sizes$code



# FINALLY, RUN SIMULATION
f_run_sim(sim_hyperparams=simulation_hyperparameters, parallel = FALSE)
