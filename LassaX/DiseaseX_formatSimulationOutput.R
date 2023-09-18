source("J:/projects/lassa_vaccination/lassaX/housekeeping.R")

##################################################
### DATA PREP: Flatten simulation output files ###
##################################################

### Flatten list for each simulation into a matrix, and save as a CSV

# folders and files
wd_simulationOutputFiles = "J:/projects/lassa_vaccination/lassaX/simulations/run2_briefformat/"
wd_simulationOutputFiles_flattened = "J:/projects/lassa_vaccination/lassaX/simulations/run2_briefformat_flattened/"
wd_simulationOutputFiles_final = "J:/projects/lassa_vaccination/lassaX/simulations/run2_briefformat_final/"

# how many simulation files?
files_diseaseX = list.files(wd_simulationOutputFiles)
n_sims = length(files_diseaseX)

# loop through simulation files, flatten to matrix and save
for(file_i in files_diseaseX[1:n_sims]){
  print(file_i)
  
  # load simulation files and flatten to matrix
  m_simulationOutput_i = as.matrix(do.call(rbind, loadRData(paste0(wd_simulationOutputFiles, file_i))))
  
  # write simulation files as CSVs
  write.csv(m_simulationOutput_i, file = paste0(wd_simulationOutputFiles_flattened, gsub(".Rdata", ".csv", file_i)))
}

###############################################################################
### DATA PREP: Combine simulation CSVs into a single list, flatten and save ###
###############################################################################

# how many simulation files?
files_diseaseX_flattened = list.files(wd_simulationOutputFiles_flattened)
n_sims_flattened = length(files_diseaseX_flattened)

# empty df
list_simulationOutputsFlattened = list()

# loop through flattened simulation CSVs, combine as a single matrix
qounter = 0
for(file_flattened_i in files_diseaseX_flattened[1:n_sims_flattened]){
  qounter = qounter + 1
  
  print(file_flattened_i)
  
  # load CSV and combine with previous
  list_simulationOutputsFlattened[[qounter]] = read.csv(paste0(wd_simulationOutputFiles_flattened, file_flattened_i))
}

# Flatten list into a single matrix
m_simulationOutputsFlattened = as.matrix(do.call(rbind, list_simulationOutputsFlattened))

# Export matrix as CSV
write.csv(m_simulationOutputsFlattened, file = paste0(wd_simulationOutputFiles_final, "m_simulationOutputsFlattened.csv"))


#################################
### OUTCOMES: CLEAN DATAFRAME ###
#################################

df_simulationOutputsFinal_partial = m_simulationOutputsFlattened%>%
  as.data.frame()%>%
  mutate(GID_1 = catchment)%>%
  left_join(., df_district_names%>%dplyr::select(GID_0, GID_1, NAME_1), by = "GID_1")%>%
  mutate(vaccEff = case_when(vacc_strategy == "none" & vaccEff == 0.7 ~ "no vaccine",
                             vacc_strategy == "none" & vaccEff == 0.9 ~ "EXCESS",
                             T ~ vaccEff))%>%
  filter(vaccEff != "EXCESS")%>%
  dplyr::select(-X)%>%
  mutate(simulation = as.numeric(simulation))

################################
### OUTCOMES: COMPLETE CASES ###
################################
### for districts with no outbreak, add null incidence

# vector of simulations
vec_simulation = levels(factor(df_simulationOutputsFinal_partial$simulation))

# dataframe of scenarios (vacc strategy, dosing, vaccEff), alongside incidences for null case
df_simulationOutputs_empty = df_simulationOutputsFinal_partial%>%
  filter(catchment == df_simulationOutputsFinal_partial$catchment[1] & simulation == df_simulationOutputsFinal_partial$simulation[1])%>%
  mutate(infect0 = 0,
         IncCumul_U_final = 0,
         IncCumul_V_final = 0,
         DosesCumul_final = Inf)%>%
  dplyr::select(infect0, vacc_alloc, vacc_strategy, vacc_dosing, vaccEff, IncCumul_U_final, IncCumul_V_final, DosesCumul_final)

# dataframe of districts in format needed
df_districts_empty = df_district_names%>%
  mutate(country = COUNTRY,
         catchment = GID_1)%>%
  dplyr::select(country, catchment, GID_0, GID_1, NAME_1)

### Loop through simulations and check which catchments missing
list_districtsMissing = list()
qounter = 0
for(simulation_i in vec_simulation){
  
  print(paste0("adding missing catchments for simulation ", simulation_i, " of ", length(vec_simulation)))
  
  # filter out results for simulation_i
  df_simulationOutputsFinal_i = filter(df_simulationOutputsFinal_partial, simulation == simulation_i)
  
  # which districts had no outbreak in this simulation?
  vec_GID_1_missing = vec_GID_1[which(!vec_GID_1 %in% df_simulationOutputsFinal_i$GID_1)]
  
  # if no districts missing in this simulation, skip to next simulation
  if(length(vec_GID_1_missing) == 0){print("TEST"); next()}
  
  qounter = qounter + 1

  list_districtsMissing[[qounter]] = df_districts_empty%>%
    filter(GID_1 %in% vec_GID_1_missing)%>%
    cross_join(df_simulationOutputs_empty)%>%
    mutate(simulation = simulation_i)
}

# flatten list and combine with main results
df_simulationOutputsFinal = rbind(df_simulationOutputsFinal_partial, 
                                  do.call(rbind, list_districtsMissing))

### Export dataframe as CSV
write.csv(df_simulationOutputsFinal,
          file = paste0(wd_simulationOutputFiles_final, "df_simulationOutputsFinal.csv"),
          row.names = F)

################################
### OUTCOMES: VACCINE DELTAS ###
################################

### Final input into health-economic model: to calculate numbers averted by vaccination, need to join "no vaccine" scenario to all vaccine scenarios
df_simulationOutputsFinal_noVacc = df_simulationOutputsFinal%>%
  filter(vacc_strategy == "none", vaccEff == "0.0")%>%
  mutate(IncCumul_U_noVacc = IncCumul_U_final,
         IncCumul_V_noVacc = IncCumul_V_final)%>%
  dplyr::select(GID_1, NAME_1, simulation, IncCumul_U_noVacc, IncCumul_V_noVacc)

df_simulationOutputsFinal_Deltas_beta = df_simulationOutputsFinal%>%
  left_join(., df_simulationOutputsFinal_noVacc)

### Update formatting! To be consistent with Lassa, we need:
# only scenarios with vaccine
# only VE 0, 70, 90
# only the following columns: country, GID_0, scenario, vaccEff, realyear, simulation, IncCumul_U_final, IncCumul_V_final, IncCumul_U_noVacc, IncCumul_V_noVacc

df_simulationOutputsFinal_Deltas = df_simulationOutputsFinal_Deltas_beta%>%
  filter(vacc_strategy != "none", vaccEff != 0.5)%>%
  mutate(vacc_strategy = gsub('same_everywhere_', '', vacc_strategy),
         scenario = paste0(vacc_strategy, "_", vacc_dosing),
         realyear = 1)%>%
  dplyr::select(-c(infect0, vacc_alloc, DosesCumul_final, catchment, NAME_1, vacc_strategy, vacc_dosing))%>%
  group_by(country, GID_0, scenario, vaccEff, realyear, simulation)%>%
  summarise(IncCumul_U_final = sum(IncCumul_U_final), 
            IncCumul_V_final = sum(IncCumul_V_final), 
            IncCumul_U_noVacc = sum(IncCumul_U_noVacc), 
            IncCumul_V_noVacc = sum(IncCumul_V_noVacc))

### Export dataframe as CSV
write.csv(df_simulationOutputsFinal_Deltas,
          file = paste0(wd_simulationOutputFiles_final, "df_simulationOutputsFinal_Deltas.csv"),
          row.names = F)

