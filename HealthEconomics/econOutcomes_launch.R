######################
### Which disease? ###
######################
# Toggle disease depending on which is being simulated
# This code works on simulated transmission outputs for both Lassa and Lassa-X

# whichDisease = "Lassa"
whichDisease = "LassaX"

whichAnalysis = "main"
# whichAnalysis = "USA"

########################
### LOAD ECON INPUTS ###
########################
fix_accents = F
source("HealthEconomics/econOutcomes_inputs.R")
source("HealthEconomics/econOutcomes_func.R")

df_params_loop = df_params_montecarlo

##########################################
### FILE ORGANIZATION AND DATA LOADING ###
##########################################

### Set up filepaths and load data ###

### LASSA
if(whichDisease == "Lassa"){
  ### set folder with simulation outputs
  folder_simulationOutputs = "HealthEconomics/simulationInputs_lassa/"
  
  ### set folder for econ outputs
  folder_econOutputs = "HealthEconomics/econOutputs_lassa/"
  
  ### Load final LASV simulation outputs
  df_simulationOutputsFinal_Deltas = loadRData(paste0(folder_simulationOutputs, "df_LassaOutputsFinal_Deltas.Rdata"))%>%
    mutate(vaccEff = as.numeric(vaccEff))
}

### LASSA-X
if(whichDisease == "LassaX"){
  ### set folder with simulation outputs
  folder_simulationOutputs = "HealthEconomics/simulationInputs_lassaX/"
  
  ### set folder for econ outputs
  folder_econOutputs = "HealthEconomics/econOutputs_lassaX/"
  
  ### Load final simulation outputs
  df_simulationOutputsFinal_Deltas = read.csv(paste0(folder_simulationOutputs, "df_simulationOutputsFinal_Deltas.csv"))%>%
    mutate(vaccEff = as.numeric(vaccEff)*100)
}

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
if(whichDisease == "Lassa"){
  vec_scenarios = levels(factor(df_simulationOutputsFinal_Deltas$scenario))
}

if(whichDisease == "LassaX"){
  vec_scenarios = levels(factor(df_simulationOutputsFinal_Deltas$scenario))
}


### vaccine efficacy
# consider scenarios with impact against only disease or both infection and disease
vec_vaccEff = c("0_70", "0_90", "70_70", "90_90") # format: infection_disease

### probability of sequelae
vec_prob_seq = c(8/47, 13/21)

### number of runs
# (although this could be calculated from df_params within loop, need number of runs as function argument to
# dispatch across parallel processors)
vec_runs = levels(factor(df_params_loop$n_sim_montecarlo))

### severity scaler
# que pour Lassa-X : on multiple/divise le taux d'hospitalisation par 10 

if(whichDisease == "Lassa"){
  vec_severity_scaler = 1
}

if(whichDisease == "LassaX"){
  vec_severity_scaler = c(0.1, 1, 10)
}

### for univariate sensitivity analysis
if(whichAnalysis == "USA"){
  vec_severity_scaler = 1
  
  df_params_loop = df_params_usa
  
  vec_runs = levels(factor(df_params_loop$n_sim_montecarlo))
  
  # take only the median LASV simulation to explore uncertainty resulting from Monte Carlo simulations
  vec_simulations = "50"
}

##########################
### SINGLE CORE LAUNCH ###
##########################
### single core

### TEST (SHOULD WORK FOR LASSA AND LASSA-X):

list_m_outputs = f_econOutcomes(vec_GID_0 = "MLI",
                                vec_simulations = 1,
                                vec_years = c(1),
                                vec_runs = c(1,2,3),
                                vec_prob_seq = c(0.015),
                                vec_scenarios = vec_scenarios[6],
                                vec_vaccEff = c("70_70", "90_90"),
                                df_params = df_params_loop)

###########################
### MULTI-CORE FUNCTION ###
###########################
### Launches full set of health-economic simulations, parallelized across available cores using furrr
library(furrr)

### optional code to reduce parameter input vectors to test on a small batch

testBatch = F
if(testBatch == T){
  vec_prob_seq = 8/47
  vec_severity_scaler = 1
  vec_years = c("1")
  vec_simulations = c("1", "2")
  vec_runs = c("1", "2")
  vec_scenarios = vec_scenarios[c(1,2)]
  vec_GID_0 = c("MLI")
}


### To be distributed through a loop across PROB SEQUELAE, SEVERITY SCALING (Lassa-X only) and OUTBREAK SIMULATIONS to save results au fur et a mesure
for(val_prob_seq_f in vec_prob_seq){
  
  val_prob_seq = val_prob_seq_f
  
  for(severity_scaler_h in vec_severity_scaler){
    
    df_params_loop_h = df_params_loop
    
    # ignore if severity_scaler_h == 1; otherwise update df_params accordingly
    if(severity_scaler_h == 1){filepath_sev_scale = c()}else{
      filepath_sev_scale = paste0("_severityScale_", severity_scaler_h, "_")
      
      df_params_loop_h$prob_hosp =  df_params_loop$prob_hosp*severity_scaler_h
    }
    
    for(outbreaksim_i in vec_simulations){
      
      print(paste0("scaling severity by ", severity_scaler_h, ", on outbreak simulation ", outbreaksim_i, " of ", length(vec_simulations)))
      
      ### SET CORE DISPATCHING
      
      plan("multisession") # assigns jobs to processors
      availableCores() #just to see how many
      options(future.globals.onReference = "error")
      
      ### SET SEED
      set.seed(12072023)
      
      ### LAUNCH
      list_m_outputs = future_map(vec_runs, ~ f_econOutcomes(vec_GID_0 = vec_GID_0,
                                                             vec_simulations = outbreaksim_i,
                                                             vec_years = vec_years, 
                                                             vec_runs = .x,
                                                             vec_prob_seq = val_prob_seq, 
                                                             vec_scenarios = vec_scenarios,
                                                             vec_vaccEff = vec_vaccEff,
                                                             df_params = df_params_loop_h),
                                  .options = furrr_options(seed=T))
      
      ### SAVE OUTPUTS
      save(list_m_outputs,
           file = paste0(folder_econOutputs, whichDisease, "_list_econOutputs_outbreakSimulation_", outbreaksim_i,"_probSeq_", round(val_prob_seq,2), filepath_sev_scale, ".Rdata"))
      
    }
  }
}
