#########################
### LOAD HOUSEKEEPING ###
#########################

fix_accents = T
source("J:/projects/lassa_Vaccination/lassaX/housekeeping.R")

#############################
### LOAD DEMOGRAPHIC DATA ###
#############################

df_district_names_national = df_district_names%>%
  group_by(GID_0, COUNTRY)%>%
  summarise(N = sum(Population_raster))

N_region = sum(df_district_names_national$N)

######################################
### LOAD HEALTH ECON OUTCOMES DATA ###
######################################

# howManyRuns = 1
howManyRuns = 10


if(howManyRuns == 10){
  # filepaths
  filepath_runs = c("J:/projects/lassa_Vaccination/lassa/outcomes/10runs/")
  filepath_tables = c("J:/projects/lassa_Vaccination/lassa/tables/10runs/")
  # outputs
  df_outputs = loadRData(paste0(filepath_runs, "df_lassa_econOutcomes.Rdata"))
}


######################
### SUMMARIZE DATA ###
######################

##############################
### LONG FORMAT BY COUNTRY ###
##############################

# outputs
df_outputs_long = df_outputs%>%
  filter(outcome == "baseline")%>%
  dplyr::select(-outcome)%>%
  pivot_longer(-c(GID_0, simulation, realyear, run, prob_hosp, scenario, vaccEff, val_prob_hosp),
               names_to = "outcome",
               values_to = "value")%>%
  mutate(value = as.numeric(value))%>%
  na.omit()%>%
  left_join(., df_district_names_national)

# outputs averted
df_outputs_averted_long = df_outputs%>%
  filter(outcome == "averted")%>%
  dplyr::select(-outcome)%>%
  pivot_longer(-c(GID_0, simulation, realyear, run, prob_hosp, scenario, vaccEff, val_prob_hosp),
               names_to = "outcome",
               values_to = "value")%>%
  mutate(value = as.numeric(value))%>%
  na.omit()%>%
  left_join(., df_district_names_national)


#########################################
### SUMMARIZED (QUANTILES) BY COUNTRY ###
#########################################

### ANNUAL ###
# outputs
df_outputs_annual_summarized = df_outputs_long%>%
  group_by(GID_0, N, scenario, vaccEff, realyear, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N,
         q250_rate = q250/N,
         q750_rate = q750/N,
         q975_rate = q975/N,
         median_rate = median/N,
         mean_rate = mean/N)

save(df_outputs_annual_summarized, file = paste0(filepath_runs, "df_outputs_annual_summarized.Rdata"))



# outputs averted
df_outputs_averted_annual_summarized = df_outputs_averted_long%>%
  group_by(GID_0, N, scenario, vaccEff, realyear, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N,
         q250_rate = q250/N,
         q750_rate = q750/N,
         q975_rate = q975/N,
         median_rate = median/N,
         mean_rate = mean/N)

save(df_outputs_averted_annual_summarized, file = paste0(filepath_runs, "df_outputs_averted_annual_summarized.Rdata"))



### TOTAL ###

# outputs
df_outputs_total_summarized = df_outputs_long%>%
  group_by(GID_0, N, scenario, vaccEff, prob_hosp, outcome, simulation, run)%>%
  summarise(value = sum(value))%>%
  group_by(GID_0, N, scenario, vaccEff, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N,
         q250_rate = q250/N,
         q750_rate = q750/N,
         q975_rate = q975/N,
         median_rate = median/N,
         mean_rate = mean/N)

save(df_outputs_total_summarized, file = paste0(filepath_runs, "df_outputs_total_summarized.Rdata"))


# outputs averted
df_outputs_averted_total_summarized = df_outputs_averted_long%>%
  group_by(GID_0, N, scenario, vaccEff, prob_hosp, outcome, simulation, run)%>%
  summarise(value = sum(value))%>%
  group_by(GID_0, N, scenario, vaccEff, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N,
         q250_rate = q250/N,
         q750_rate = q750/N,
         q975_rate = q975/N,
         median_rate = median/N,
         mean_rate = mean/N)

save(df_outputs_averted_total_summarized, file = paste0(filepath_runs, "df_outputs_averted_total_summarized.Rdata"))


###############################################
### SUMMARIZED (QUANTILES) FOR WHOLE REGION ###
###############################################

### ANNUAL ###

# outputs
df_outputs_annual_summarized_allRegion = df_outputs_long%>%
  group_by(scenario, vaccEff, realyear, prob_hosp, simulation, run, outcome)%>%
  summarise(value = sum(value))%>%
  group_by(scenario, vaccEff, realyear, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N_region,
         q250_rate = q250/N_region,
         q750_rate = q750/N_region,
         q975_rate = q975/N_region,
         median_rate = median/N_region,
         mean_rate = mean/N_region)

save(df_outputs_annual_summarized_allRegion, file = paste0(filepath_runs, "df_outputs_annual_summarized_allRegion.Rdata"))


# outputs averted
df_outputs_averted_annual_summarized_allRegion = df_outputs_averted_long%>%
  group_by(scenario, vaccEff, realyear, prob_hosp, simulation, run, outcome)%>%
  summarise(value = sum(value))%>%
  group_by(scenario, vaccEff, realyear, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N_region,
         q250_rate = q250/N_region,
         q750_rate = q750/N_region,
         q975_rate = q975/N_region,
         median_rate = median/N_region,
         mean_rate = mean/N_region)

save(df_outputs_averted_annual_summarized_allRegion, file = paste0(filepath_runs, "df_outputs_averted_annual_summarized_allRegion.Rdata"))


### TOTAL ###
# outputs
df_outputs_total_summarized_allRegion = df_outputs_long%>%
  group_by(scenario, vaccEff, prob_hosp, simulation, run, outcome)%>%
  summarise(value = sum(value))%>%
  group_by(scenario, vaccEff, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N_region,
         q250_rate = q250/N_region,
         q750_rate = q750/N_region,
         q975_rate = q975/N_region,
         median_rate = median/N_region,
         mean_rate = mean/N_region)

save(df_outputs_total_summarized_allRegion, file = paste0(filepath_runs, "df_outputs_total_summarized_allRegion.Rdata"))


# outputs averted
df_outputs_averted_total_summarized_allRegion = df_outputs_averted_long%>%
  group_by(scenario, vaccEff, prob_hosp, simulation, run, outcome)%>%
  summarise(value = sum(value))%>%
  group_by(scenario, vaccEff, prob_hosp, outcome)%>%
  summarise(q025 = quantile(value, 0.025),
            q250 = quantile(value, 0.25),
            q750 = quantile(value, 0.75),
            q975 = quantile(value, 0.975),
            median = quantile(value, 0.5),
            mean = mean(value))%>%
  mutate(q025_rate = q025/N_region,
         q250_rate = q250/N_region,
         q750_rate = q750/N_region,
         q975_rate = q975/N_region,
         median_rate = median/N_region,
         mean_rate = mean/N_region)

save(df_outputs_averted_total_summarized_allRegion, file = paste0(filepath_runs, "df_outputs_averted_total_summarized_allRegion.Rdata"))

