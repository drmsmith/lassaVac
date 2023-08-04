library(tidyverse)


### run housekeeping file
source("J:/projects/lassa_vaccination/lassaX/housekeeping.R")

################################
#### LOAD DATA AND AGGREGATE ###
################################

#################
### SELECT VE ###
#################

VE_i = "70"

### working directories
wd_Lassa_csv = paste0("J:/projects/lassa_vaccination/lassa/simulations/99_runs/outputs_raw_", VE, "/")

wd_Lassa_clean = "J:/projects/lassa_vaccination/lassa/simulations/99_runs/outputs_clean/"

### selected districts
vec_districts_highlighted = c("NGA.20_1", "GIN.5_1", "MLI.5_1")

### List Lassa files:  VE = 70% ###
vec_files_Lassa_csv = list.files(wd_Lassa_csv)

# empty list and qounter to fill it
list_Lassa_allRegion_daily = list()
list_Lassa_selectDistricts_daily = list()
list_Lassa_byDistrict_annual = list()
list_Lassa_byCountry_annual = list()
list_Lassa_allRegion_annual = list()
list_Lassa_byDistrict_total = list()
list_Lassa_byCountry_total = list()
list_Lassa_allRegion_total = list()
qounter = 0

##########################
### LOOP THROUGH FILES ###
##########################

for(file_lassa_i in vec_files_Lassa_csv){
  
  qounter = qounter+1
  
  print(paste0("on file ", qounter, " of ", length(vec_files_Lassa_csv)))
  
  df_Lassa_csv_i = read.csv(paste0(wd_Lassa_csv, file_lassa_i))
  
  #############
  ### DAILY ###
  #############
  
  #### Daily outcomes: whole region
  df_Lassa_allRegion_daily_i = df_Lassa_csv_i%>%
    group_by(scenario, run, country, catchmentID)%>%
    mutate(time = row_number())%>%
    group_by(scenario, run, time)%>%
    summarise(unpruned = sum(unpruned),
              pruned = sum(pruned))%>%
    mutate(averted = unpruned - pruned)
  
  #### Daily outcomes: select districts
  df_Lassa_selectDistricts_daily_i = df_Lassa_csv_i%>%
    filter(catchmentID %in% vec_districts_highlighted)%>%
    group_by(scenario, run, country, catchmentID)%>%
    mutate(time = row_number())%>%
    mutate(averted_nc = unpruned_nc - pruned_nc)%>%
    ungroup()%>%
    dplyr::select(scenario, run, country, catchmentID, time, unpruned_nc, pruned_nc, averted_nc)
  
  ##############
  ### ANNUAL ###
  ##############
  
  ##### Annual outcomes: by district
  df_Lassa_byDistrict_annual_i = df_Lassa_csv_i%>%
    group_by(scenario, run, realyear, country, catchmentID)%>%
    mutate(time = row_number(),
           unpruned = cumsum(unpruned_nc),
           pruned = cumsum(pruned_nc),
           cum_vacc = cumsum(vaccinated),
           cum_totvacc = cumsum(total_vaccinated),
           cum_totaver_spillover = cumsum(total_averted),
           spillover = sum(spillover_nc),
           h2h = cumsum(h2h_nc))%>%
    filter(time == 365)%>%
    mutate(averted = unpruned - pruned,
           vaccinated_infected = cum_totvacc - cum_totaver_spillover)%>%
    ungroup()%>%
    dplyr::select(scenario, run, realyear, country, catchmentID, unpruned, pruned, averted, 
                  cum_vacc, cum_totvacc, cum_totaver_spillover, vaccinated_infected, spillover, h2h)
  
  ##### Annual outcomes: by country
  df_Lassa_byCountry_annual_i = df_Lassa_byDistrict_annual_i%>%
    group_by(scenario, run, country, realyear)%>%
    summarise(unpruned = sum(unpruned),
              pruned = sum(pruned),
              averted = sum(averted),
              cum_vacc = sum(cum_vacc),
              cum_totvacc = sum(cum_totvacc),
              cum_totaver_spillover = sum(cum_totaver_spillover),
              vaccinated_infected = sum(vaccinated_infected),
              spillover = sum(spillover),
              h2h = sum(h2h))%>%
    mutate(prop_h2h = h2h/(h2h+spillover),
           prop_h2h_test = h2h/(unpruned))
  
  ##### Annual outcomes: for whole region
  df_Lassa_allRegion_annual_i = df_Lassa_byCountry_annual_i%>%
    group_by(scenario, run, realyear)%>%
    summarise(unpruned = sum(unpruned),
              pruned = sum(pruned),
              averted = sum(averted),
              cum_vacc = sum(cum_vacc),
              cum_totvacc = sum(cum_totvacc),
              cum_totaver_spillover = sum(cum_totaver_spillover),
              vaccinated_infected = sum(vaccinated_infected),
              spillover = sum(spillover),
              h2h = sum(h2h))%>%
    mutate(prop_h2h = h2h/(h2h+spillover),
           prop_h2h_test = h2h/unpruned)
  
  #############
  ### TOTAL ###
  #############
  
  ##### Total outcomes: by district
  df_Lassa_byDistrict_total_i = df_Lassa_byDistrict_annual_i%>%
    ungroup()%>%
    group_by(scenario, run, country, catchmentID)%>%
    summarise(unpruned = sum(unpruned),
              pruned = sum(pruned),
              averted = sum(averted),
              cum_vacc = sum(cum_vacc),
              cum_totvacc = sum(cum_totvacc),
              cum_totaver_spillover = sum(cum_totaver_spillover),
              vaccinated_infected = sum(vaccinated_infected),
              spillover = sum(spillover),
              h2h = sum(h2h))%>%
    mutate(prop_h2h = h2h/(h2h+spillover),
           prop_h2h_test = h2h/(unpruned))
  
  ##### Total outcomes: by country
  df_Lassa_byCountry_total_i = df_Lassa_byCountry_annual_i%>%
    ungroup()%>%
    group_by(scenario, run, country)%>%
    summarise(unpruned = sum(unpruned),
              pruned = sum(pruned),
              averted = sum(averted),
              cum_vacc = sum(cum_vacc),
              cum_totvacc = sum(cum_totvacc),
              cum_totaver_spillover = sum(cum_totaver_spillover),
              vaccinated_infected = sum(vaccinated_infected),
              spillover = sum(spillover),
              h2h = sum(h2h))%>%
    mutate(prop_h2h = h2h/(h2h+spillover),
           prop_h2h_test = h2h/(unpruned))
  
  ##### Total outcomes: for whole region
  df_Lassa_allRegion_total_i = df_Lassa_allRegion_annual_i%>%
    group_by(scenario, run)%>%
    summarise(unpruned = sum(unpruned),
              pruned = sum(pruned),
              averted = sum(averted),
              cum_vacc = sum(cum_vacc),
              cum_totvacc = sum(cum_totvacc),
              cum_totaver_spillover = sum(cum_totaver_spillover),
              vaccinated_infected = sum(vaccinated_infected),
              spillover = sum(spillover),
              h2h = sum(h2h))%>%
    mutate(prop_h2h = h2h/(h2h+spillover),
           prop_h2h_test = h2h/unpruned)
  
  ##############################
  ### STORE RESULTS TO LISTS ###
  ##############################
  
  list_Lassa_allRegion_daily[[qounter]] = df_Lassa_allRegion_daily_i
  list_Lassa_selectDistricts_daily[[qounter]] = df_Lassa_selectDistricts_daily_i
  list_Lassa_byDistrict_annual[[qounter]] = df_Lassa_byDistrict_annual_i
  list_Lassa_byCountry_annual[[qounter]] = df_Lassa_byCountry_annual_i
  list_Lassa_allRegion_annual[[qounter]] = df_Lassa_allRegion_annual_i
  list_Lassa_byDistrict_total[[qounter]] = df_Lassa_byDistrict_total_i
  list_Lassa_byCountry_total[[qounter]] = df_Lassa_byCountry_total_i
  list_Lassa_allRegion_total[[qounter]] = df_Lassa_allRegion_total_i
}

####################
### SAVE RESULTS ###
####################

save(list_Lassa_allRegion_daily, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_allRegion_daily.Rdata"))
save(list_Lassa_selectDistricts_daily, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_selectDistricts_daily.Rdata"))
save(list_Lassa_byDistrict_annual, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byDistrict_annual.Rdata"))
save(list_Lassa_byCountry_annual, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byCountry_annual.Rdata"))
save(list_Lassa_allRegion_annual, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_allRegion_annual.Rdata"))
save(list_Lassa_byDistrict_total, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byDistrict_total.Rdata"))
save(list_Lassa_byCountry_total, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byCountry_total.Rdata"))
save(list_Lassa_allRegion_total, file = paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_allRegion_total.Rdata"))


########################################################
### COMBINE RESULTS FOR DIFFERENT VACCINE EFFICACIES ###
########################################################



##############################################
### PREPARE OUTCOMES FOR HEALTH-ECON MODEL ###
##############################################

### Load data

# temp = df_Lassa_byCountry_annual%>%filter(run == "run2", VE == 90, scenario == 1)
# sum(temp$unpruned)

df_LassaOutputsFinal_Deltas = df_Lassa_byCountry_annual%>%
  ungroup()%>%
  left_join(., df_country_names%>%
              dplyr::select(country, GID_0),
            by = "country")%>%
  mutate(scenario = scenario,
         vaccEff = VE,
         realyear = realyear,
         simulation = factor(run, 
                             levels = c(paste0("run",1:10)),
                             labels = 1:10),
         IncCumul_U_final = pruned - vaccinated_infected,
         IncCumul_V_final = vaccinated_infected,
         DosesCumul_final = 0,
         IncCumul_U_noVacc = unpruned,
         IncCumul_V_noVacc = 0)%>%
  dplyr::select(country, GID_0, scenario, vaccEff, realyear, simulation,
                IncCumul_U_final, IncCumul_V_final, IncCumul_U_noVacc, IncCumul_V_noVacc)

save(df_LassaOutputsFinal_Deltas, file = "J:/projects/lassa_vaccination/lassa/simulations/df_LassaOutputsFinal_Deltas.Rdata")
  
# temp = df_LassaOutputsFinal_Deltas%>%filter(simulation == 9, vaccEff == 70, scenario == 7)
# sum(temp$IncCumul_U_noVacc) - sum(temp$IncCumul_U_final) + sum(temp$IncCumul_V_final)
