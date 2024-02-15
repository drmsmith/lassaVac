# Pruning code
# Uses output from new Lassa R code.
# Lassa pruning function includes removal of additional h2h infections
# (to approximate prevention of onward transmissions).
#    N.B. This involves a "while" loop, which will continue indefinitely if R0 > 0.5
#         and "cases" becomes 1.
# Number vaccinated also recorded for assessment of disease prevention.
# Each output file is pruned separately.

rm(list=ls())

library(zoo)
library(tidyverse)
source("Lassa_pruning_functions.R")

#specify vaccine efficacy and R0
VE <- 70
vacc_eff <- VE/100
R0 <- 0.0631

#read in the scenario files to get the coverages
Scenario_1_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_1.csv")
Scenario_2_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_2.csv")
Scenario_3_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_3.csv")
Scenario_4_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_4.csv")
Scenario_5_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_5.csv")
Scenario_6_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_6.csv")
Scenario_7_orig <- read_csv("Scenario_files/OxLiv_lassa_new_scenario_7.csv")

#make NAs 0
Scenario_1_orig[is.na(Scenario_1_orig)] = 0
Scenario_2_orig[is.na(Scenario_2_orig)] = 0
Scenario_3_orig[is.na(Scenario_3_orig)] = 0
Scenario_4_orig[is.na(Scenario_4_orig)] = 0
Scenario_5_orig[is.na(Scenario_5_orig)] = 0
Scenario_6_orig[is.na(Scenario_6_orig)] = 0
Scenario_7_orig[is.na(Scenario_7_orig)] = 0

#combine the scenarios
combined_scenarios <- Scenario_1_orig %>%
    mutate(Scenario = 1) %>%
  bind_rows(Scenario_2_orig %>%
    mutate(Scenario = 2)) %>%
  bind_rows(Scenario_3_orig %>%
    mutate(Scenario = 3)) %>%
  bind_rows(Scenario_4_orig %>%
    mutate(Scenario = 4)) %>%
  bind_rows(Scenario_5_orig %>%
    mutate(Scenario = 5)) %>%
  bind_rows(Scenario_6_orig %>%
    mutate(Scenario = 6)) %>%
  bind_rows(Scenario_7_orig %>%
    mutate(Scenario = 7))

#start output file loop
for (i in (0:98)) {
  message("i = ", i)
  
  #read in results to get the baseline (unpruned)
  model_outputs <- read_csv(paste0("OxLiv_lassa_Routput_R0_0631_reformatted_", i, ".csv"))

  #join the datasets by catchment and scenario
  joined_data <- full_join(model_outputs, combined_scenarios, by = c("identifier" = "GID_1"))

  #add real year to data (the column Year is actually the year where a catchment starts preventive vaccination)
  joined_data$realyear <- ceiling(joined_data$time/365)

  #start scenario loop
  for (scen in (1:7)) {
    message("scen = ", scen)
	
	#filter out one scenario
	catchmentdata_scen <- joined_data %>% filter(.,Scenario == scen)
	
	#start catchment loop
	catchments <- unique(catchmentdata_scen$identifier)
	for (cat in (1:length(catchments))) {
	  message("cat = ", cat)
	
		#filter out one catchment
		catchmentdata <- catchmentdata_scen %>% filter(., .$identifier %in% catchments[cat])

		#use pruning function to apply vaccination
		Lassa_pruning_function(catchmentdata, vacc_eff, R0)
		
		#add cumulative versions of pruned and h2h to Lassa_results
		Lassa_results$pruned <- cumsum(Lassa_results$pruned_nc)
		Lassa_results$h2h <- cumsum(Lassa_results$h2h_nc)
		
		if (cat==1) {
		  Lassa_results_cats <<- Lassa_results
		} else {
		  Lassa_results_cats <<- bind_rows(Lassa_results_cats, Lassa_results)
		}

	} #end catchment loop

	if (scen==1) {
	  Lassa_results_all <<- Lassa_results_cats
	} else {
	  Lassa_results_all <<- bind_rows(Lassa_results_all, Lassa_results_cats)
	}
	
  } #end scenario loop
  
  #save results for all scenarios
  write.csv(Lassa_results_all, file=sprintf("Lassa_results_%s_99_%s_scenarios_all.csv", VE, (i+1)), row.names=FALSE)

} #end output file loop

