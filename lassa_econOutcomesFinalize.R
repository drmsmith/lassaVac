#########################
### LOAD HOUSEKEEPING ###
#########################

fix_accents = T
source("J:/projects/lassa_Vaccination/lassaX/housekeeping.R")

######################################
### LOAD HEALTH ECON OUTCOMES DATA ###
######################################


# number of runs per simulation
#n_runs = 1
n_runs = 10

if(n_runs == 10){
  # filepaths
  filepath_runs = c("J:/projects/lassa_Vaccination/lassa/outcomes/10runs/")
  filepath_tables = c("J:/projects/lassa_Vaccination/lassa/tables/10runs/")
  # outputs
  list_outputs = loadRData(paste0(filepath_runs, "list_lassa_econOutputs.Rdata"))
}

# flatten list to a single matrix
m_list_outputs = do.call(rbind, list_outputs)

# which columns are metadata?
cols_metadata = c("outcome", "GID_0", "simulation", "realyear", "run", "prob_hosp", "scenario", "vaccEff")

# make dataframe with numeric outcome columns, and add "discounting year" (accounts for no disc in first year)
df_list_outputs = m_list_outputs%>%
  data.frame()%>%
  mutate(year_disc = as.numeric(realyear)-1)

# make outcomes columns numeric
df_list_outputs[,(length(cols_metadata)+1):ncol(m_list_outputs)] <- sapply(df_list_outputs[,(length(cols_metadata)+1):ncol(m_list_outputs)], as.numeric)

########################
### LOAD ECON INPUTS ###
########################

# necessary to run econ inputs file
whichDisease = "lassa"
source("J:/projects/lassa_vaccination/inputs_econOutcomes.R")

###########################################
### CALCULATE OUTCOMES FOR EACH COUNTRY ###
###########################################

# prep empty dataframe
df_lassa_econOutcomes = data.frame()

# loop through countries and calculate outcomes with country-specific values
for(GID_0_i in vec_GID_0){
  print(paste("calculating final econ outcomes for ", GID_0_i))
  
  # econ data: lifetable, VSL, daly estimates, hosp costs and OOP costs
  df_age_distr_i = df_age_distr%>%filter(GID_0 == GID_0_i)
  df_daly_i = df_daly%>%filter(GID_0 == GID_0_i)
  df_VSL_i = df_VSL%>%filter(GID_0 == GID_0_i)
  df_scaling_i = df_scaling%>%filter(GID_0 == GID_0_i)
  # df_choice_i = df_choice%>%filter(GID_0 == GID_0_i) ### NO LONGER USE DF CHOICE
  df_catastrophe_i = df_catastrophe%>%filter(GID_0 == GID_0_i)
  df_GNI_prop_working_i = df_GNI_prop_working%>%filter(GID_0 == GID_0_i)
  df_country_avg_age_i = df_country_avg_age%>%filter(GID_0 == GID_0_i)
  df_age_distr_avg_age_i = df_age_distr_i%>%filter(age_numeric == df_country_avg_age_i$avg_age)
  
  # pull out specific values
  # cost_hosp_daily_i = df_choice_i$Int_Dollar_2021[1] ### OLD per day healthcare cost, now instead use total Lassa treamtent cost
  cost_treatment_i = df_scaling_i$treat_cost_2021IntD
  cost_oop_i = df_scaling_i$countr_specific_OOP_2021IntD
  cost_daly_i = df_daly_i$daly_value
  prop_working_i = df_GNI_prop_working_i$prop_working
  cost_GNI_i = df_GNI_prop_working_i$GNI
  cost_GNI_daily_i = df_GNI_prop_working_i$GNI_daily
  VSL_i = df_VSL_i$Int_Dollar_2021
  
  avg_years_left_to_live_i = df_age_distr_avg_age_i$life_exp_at_age_x
  VSLY_i = VSL_i/avg_years_left_to_live_i
  
  df_lassa_econOutcomes_i = df_list_outputs%>%
    filter(GID_0 == GID_0_i)%>%
    # calculate DALYs for fever (community), hospitalization and death
    mutate(DALY_fever = (N_symptoms-N_hospital)*DALY_fever, # DALYs for those with fever in community (no hospital)
           DALY_hospital = (N_hospital-N_death)*DALY_hospital_survived + N_death*DALY_hospital_died,
           DALY_sequelae = DALY_sequelae_annual*YLS,
           DALY_sequelae_disc = DALY_sequelae_annual*YLS_disc,
           DALY_death = YLL,
           DALY_death_disc = YLL_disc)%>%
    mutate(DALY_total = DALY_fever + DALY_hospital + DALY_sequelae + DALY_death,
           DALY_total_disc = DALY_fever + DALY_hospital + DALY_sequelae_disc + DALY_death_disc,
           Cost_VSL = N_death*VSL_i,
           Cost_VSLY = DALY_death_disc*VSLY_i)%>%
    # calculate DALY cost, hospitalization costs (based on LOS and daily cost in Int_dollar)
    mutate(Cost_DALY_total = DALY_total*cost_daly_i,
           Cost_hosp = N_hospital*cost_treatment_i,
           Cost_OOP = N_hospital*cost_oop_i)%>%
    # calculate productivity losses
    mutate(Cost_prod_fever = (N_W_symptoms-N_W_hospital)*cost_GNI_daily_i*duration_fever,
           Cost_prod_hospital = (N_W_hospital-N_W_death)*cost_GNI_daily_i*(duration_ill_prehospital + hospital_LOS_survived) + N_W_death*cost_GNI_daily_i*(duration_ill_prehospital + hospital_LOS_died),
           Cost_prod_death = cost_GNI_i * YWL,
           Cost_prod_death_disc = cost_GNI_i * YWL_disc,
           Cost_prod_total = Cost_prod_fever + Cost_prod_hospital + Cost_prod_death)%>%
    # calculate total productivity cost and discount all future costs
    mutate(Cost_DALY_total_disc = (DALY_total_disc*cost_daly_i)*(1/(1+discRate)^year_disc),
           Cost_hosp_disc = Cost_hosp*(1/(1+discRate)^year_disc),
           Cost_OOP_disc = Cost_OOP*(1/(1+discRate)^year_disc),
           Cost_prod_total_disc = (Cost_prod_fever + Cost_prod_hospital + Cost_prod_death_disc)*(1/(1+discRate)^year_disc))%>%
    # group societal costs
    mutate(Cost_societal = Cost_OOP + Cost_hosp + Cost_prod_total,
           Cost_societal_disc = Cost_OOP_disc + Cost_hosp_disc + Cost_prod_total_disc)
  
  df_lassa_econOutcomes = rbind(df_lassa_econOutcomes, df_lassa_econOutcomes_i)
    
}


### save final outcomes (plutot en tant que Rdata car csv grand et lent)
save(df_lassa_econOutcomes, file = paste0(filepath_runs, "df_lassa_econOutcomes.Rdata"))


