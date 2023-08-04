

###################
### SET DISEASE ###
###################

# necessary to run econ inputs file
whichDisease = "lassa"

# number of runs per simulation
#n_runs = 1
n_runs = 10

########################
### LOAD ECON INPUTS ###
########################
fix_accents = F
source("J:/projects/lassa_vaccination/inputs_econOutcomes.R")
source("J:/projects/lassa_vaccination/lassa/lassa_econOutcomesFunc.R")

#########################
### FILE ORGANIZATION ###
#########################

### set folder with simulation outputs
folder_simulationOutputs = "J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/"

### set folder for econ outputs
folder_econOutputs = "J:/projects/lassa_vaccination/lassa/outcomes/"

##################################
### LOAD LASSA SIMULATION DATA ###
##################################

### Load final simulation outputs
df_simulationOutputsFinal_Deltas = loadRData(paste0(folder_simulationOutputs, "df_LassaOutputsFinal_Deltas.Rdata"))

### Ensure outcomes are numeric
cols_outcomes_numeric = c("IncCumul_U_final", "IncCumul_V_final", "IncCumul_U_noVacc", "IncCumul_V_noVacc")
df_simulationOutputsFinal_Deltas[cols_outcomes_numeric] <- sapply(df_simulationOutputsFinal_Deltas[cols_outcomes_numeric], as.numeric)


#####################################
### PREP HEALTH ECON MODEL INPUTS ###
#####################################

### number of simulations included in simulationOutputs
vec_simulations = levels(factor(df_simulationOutputsFinal_Deltas$simulation))

### number of years over which model run
vec_years = levels(factor(df_simulationOutputsFinal_Deltas$realyear))

### vaccination scenarios
vec_scenarios = levels(factor(df_simulationOutputsFinal_Deltas$scenario))

### vaccine efficacy: 
# to be entered manually as consider scenarios both with impact against infection and disease
vec_vaccEff = c("0_70", "0_90", "70_70", "90_90") # infection_disease

### number of runs
vec_runs = 1:n_runs


#####################
### TEST FUNCTION ###
#####################
### single core

# test = f_lassa_econOutcomes(vec_GID_0 = "MLI",
#                             vec_simulations = 1,
#                             vec_years = c(1,10),
#                             vec_runs = c(1,2,3),
#                             vec_prob_hosp = c(0.015,0.006),
#                             vec_scenarios = c(1, 5),
#                             vec_vaccEff = c("70_70"))

#######################
### LAUNCH FUNCTION ###
#######################
### multi core


### SET CORE DISPATCHING
library(furrr)
plan("multisession") # assigns jobs to processors
availableCores() #just to see how many
options(future.globals.onReference = "error")

### SET SEED
set.seed(12072023)

### LAUNCH
list_m_outputs = future_map(1:10, ~ f_lassa_econOutcomes(vec_GID_0 = vec_GID_0,
                                                         vec_simulations = .x,
                                                         vec_years = vec_years, 
                                                         vec_runs = vec_runs,
                                                         vec_prob_hosp = vec_prob_hosp,
                                                         vec_scenarios = vec_scenarios,
                                                         vec_vaccEff = vec_vaccEff),
                            .options = furrr_options(seed=T))

### SAVE OUTPUTS
save(list_m_outputs,
     file = paste0(folder_econOutputs, "list_lassa_econOutputs.Rdata"))
