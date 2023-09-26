
fix_accents = F
source("housekeeping.R")

folder_econ_inputs = c("HealthEconomics/data_econ/")

#########################
### SET DISCOUNT RATE ###
#########################

discRate = 0.03

####################################
### MONTE CARLO PARAMETER INPUTS ###
####################################

### parameter point values ###

df_params_montecarlo = loadRData(paste0(folder_econ_inputs, "params_montecarlo.Rdata"))%>%
  mutate(prob_treat_comm_oop = prob_treat_comm_any - prob_treat_comm_gvt)

### parameter values sensitivity analysis
df_params_usa = loadRData(paste0(folder_econ_inputs, "params_usa.Rdata"))%>%
  mutate(prob_treat_comm_oop = prob_treat_comm_any - prob_treat_comm_gvt)

###################################
### COUNTRY-SPECIFIC PARAMETERS ###
###################################

### age distributions: 
# create numeric age column and calculate years left to work (for productivity loss calculations),
df_age_distr = loadRData(paste0(folder_econ_inputs, "age_life_exp_master.rda"))%>%
  mutate(age_numeric = as.numeric(gsub("A", "", age)))%>%
  mutate(life_exp_total = age_numeric + life_exp_at_age_x)%>%
  mutate(years_lived_past_retirement = case_when((life_exp_total - 65) > 0 ~ life_exp_total - 65,
                                                 T ~ 0))%>%
  mutate(years_left_to_work = case_when(age_numeric > 14 & age_numeric < 65 ~ life_exp_at_age_x - years_lived_past_retirement,
                                        T ~ 0))

### average population age
df_country_avg_age = df_age_distr%>%
  mutate(age_group_pop_size_times_age = age_numeric*age_group_pop_size)%>%
  group_by(GID_0)%>%
  summarise(age_group_pop_size_times_age = sum(age_group_pop_size_times_age))%>%
  left_join(., df_age_distr%>%group_by(GID_0)%>%summarise(country_pop_size = max(country_pop_size)), by = c("GID_0"))%>%
  mutate(avg_age = round(age_group_pop_size_times_age/country_pop_size))

### daly value estimates
df_daly = loadRData(paste0(folder_econ_inputs, "daly_master.rda"))

### hospital treatment costs including the share that are out of pocket expenses
df_scaling = loadRData(paste0(folder_econ_inputs, "scaling_master.rda"))

#### value of statistical life, select only the largest value from each country
df_VSL = loadRData(paste0(folder_econ_inputs, "vsl_master.rda"))%>%
  group_by(GID_0)%>%
  summarise(Int_Dollar_2021 = max(vsl_estimate_2021_IntDoll))

#### probability of catastrophic expenses
df_catastrophe = loadRData(paste0(folder_econ_inputs, "poverty_master.rda"))

### productivity estimates
df_GNI_prop_working = loadRData(paste0(folder_econ_inputs, "productivity_master.Rdata"))

### Costs of outpatient visits
# Taken from estimated national unit cost of outpatient visits, in international dollars 2017
# https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(18)30213-5/fulltext#%20 

### adjusted for inflation using GDP deflator from World Bank Open Data
# https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.DEFL.ZS&country=USA

df_cost_outpatient_visit = read.csv(paste0(folder_econ_inputs, "cost_outpatient.csv"))%>%
  mutate(GDP_deflator_2017_2021 = 113.6/102.9)%>%
  mutate(Cost_USD_2021 = Cost_USD_2017*GDP_deflator_2017_2021)


