#################################
### PARAMETER GENERATION FILE ###
#################################
### This file generates the parameter distributions used as inputs that are
### varied in Monte Carlo simulations in the health-economic model

# set working directory

# source housekeeping file
fix_accents = F
source("housekeeping.R")


#########################################
### NUMBER OF MONTE CARLO SIMULATIONS ###
#########################################
### specification of the number of simulations run
### (and hence size of parameter vectors generated for Monte Carlo model)

n_sims_montecarlo = 100

######################
### EPI PARAMETERS ###
######################

### epi parameters in underlying Lassa model (not plugged into health-econ model)
# from Lerch et al: https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-022-02405-1

### seasonality (beta)
vec_seasonality = rbeta(1000000, 9.532623, 6.438107)
c(mean(vec_seasonality), sd(vec_seasonality), qbeta(c(0.025, 0.975), 9.532623, 6.438107))

### incubation period
vec_incubation = rgamma(1000000, shape = 11.1191707, scale = 0.9224181)
c(mean(vec_incubation), sd(vec_incubation), qgamma(c(0.025, 0.975), shape = 11.1191707, scale = 0.9224181))

### infectious period
vec_infectious = rgamma(1000000, shape = 1.862467, scale = 6.0729)
c(mean(vec_infectious), sd(vec_infectious), qgamma(c(0.025, 0.975), shape = 1.862467, scale = 6.0729))


###############################
### PROBABILITY OF SYMPTOMS ###
###############################

# data from: https://academic.oup.com/jid/article/155/3/437/851511?login=true

data_mccormick = data.frame(village = c("Niahun", "Konia", "Kpandebu", "Tongola"),
                            n_ill = c(4, 1, 3, 1),
                            n_infected = c(12, 10, 15, 11))%>%
  mutate(p_symptoms = n_ill/n_infected)

# meta-analyze proportion from these four villagse

p_symptoms_metaprop = metaprop(event = data_mccormick$n_ill,
                               n = data_mccormick$n_infected,
                               studlab = data_mccormick$village)

### Check model
# summary(p_symptoms_metaprop)
# print(p_symptoms_metaprop, backtransf = F)
# forest(p_symptoms_metaprop)

library(boot)

prop_mean = inv.logit(p_symptoms_metaprop$TE.random)
prop_se = inv.logit(p_symptoms_metaprop$seTE.random)

### Check values
# check <- inv.logit(rnorm(10000, mean=(p_symptoms_metaprop$TE.random),
#                              (p_symptoms_metaprop$seTE.random)))
# 
# quantile(check, probs=c(0.025,0.5,0.975))

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_prob_symptoms = inv.logit(rnorm(n_sims_montecarlo, mean=p_symptoms_metaprop$TE.random,sd = p_symptoms_metaprop$seTE.random))

##################################################
### PROBABILITY OF SEEKING CARE GIVEN SYMPTOMS ###
##################################################

# estimates from: https://link.springer.com/article/10.1186/s12936-015-1048-x

### any treatment
p_treatment_any_mean = 0.5979
p_treatment_any_upper = 0.6512
p_treatment_any_lower = 0.5432
p_treatment_any_sd = ((p_treatment_any_upper-p_treatment_any_mean)/1.96 + (p_treatment_any_mean-p_treatment_any_lower)/1.96)/2

### government treatment
p_treatment_gvt_mean = 0.4891
p_treatment_gvt_upper = 0.5452
p_treatment_gvt_lower = 0.4352
p_treatment_gvt_sd = ((p_treatment_gvt_upper-p_treatment_gvt_mean)/1.96 + (p_treatment_gvt_mean-p_treatment_gvt_lower)/1.96)/2

### Check values
# qnorm(c(0.025, 0.975), p_treatment_any_mean, p_treatment_any_sd)
# qnorm(c(0.025, 0.975), p_treatment_gvt_mean, p_treatment_gvt_sd)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_prob_treat_comm_any = rnorm(n_sims_montecarlo, p_treatment_any_mean, p_treatment_any_sd)
parvec_prob_treat_comm_gvt = rnorm(n_sims_montecarlo, p_treatment_gvt_mean, p_treatment_gvt_sd)

##################################################
### PROBABILITIES OF HOSPITALIZATION AND DEATH ###
##################################################

# data from: https://academic.oup.com/inthealth/advance-article/doi/10.1093/inthealth/ihac076/6840000?login=true

data_simons <- readxl::read_xlsx("HealthEconomics/data_econ/suspected_confirmed_deaths.xlsx")


# CFR was calculated using the number of reported deaths as the numerator and cases as the denominator, 
# weighted by the number of reported cases. 

# NCDC data were limited to Edo and Ondo states between 2018 and 2021 (better surveillance). 
# The expected number of cases was calculated for reported deaths and compared with the number of reported cases. 
# CFR values of 0% and 100% were removed prior to calculating weighted mean CFRs.

# Limiting the Nigerian states contributing data to those with higher surveillance (Edo and Endo)
states <- c("edo","ondo")

# define data to include in analyses (districts and years with good surveillance)
dataend <- 2021
datastart <- 2018 # start of improved surveillance in Nigeria, 

###########
### CFR ###
###########

# clean data and calculate CFR
cfr <- data_simons %>%
  dplyr::select(year, country, region, suspected_cases, confirmed_cases, deaths_among_confirmed) %>%
  drop_na(deaths_among_confirmed) %>%
  group_by(year, country) %>%
  mutate(records = n(),
         suspected_cases = as.numeric(suspected_cases)) %>%
  filter(!(country == "nigeria" & region == "all" & records > 2)) %>%
  ungroup() %>%
  filter(year <= dataend & year >= datastart & region %in% states) %>%
  mutate(cases = case_when(is.na(confirmed_cases) ~ suspected_cases,
                           deaths_among_confirmed > confirmed_cases ~ suspected_cases,
                           TRUE ~ confirmed_cases)) %>%
  rowwise() %>%
  mutate(cfr = case_when(is.nan(deaths_among_confirmed/cases) ~ 0,
                         TRUE ~ deaths_among_confirmed/cases),
         cfr_type = "ncdc_sub_cfr") %>%
  ungroup()

weighted_cfr <- cfr %>%
  filter(cfr != 0 & cfr != 1) %>%
  summarise(total_cases = sum(cases),
            total_deaths = sum(deaths_among_confirmed),
            summary_cfr = wtd.mean(cfr, cases),
            sd_cfr = sqrt(wtd.var(cfr, cases))) %>%
  mutate(cfr_type = "ncdc_sub_cfr") 

cfr_mean = weighted_cfr$summary_cfr
cfr_sd = weighted_cfr$sd_cfr

### Fit to a beta distribution
parsBeta_p_hosp = estBetaParams(cfr_mean, cfr_sd^2)

### Check values
# c(mean(rbeta(10000, parsBeta_p_hosp$alpha, parsBeta_p_hosp$beta)), qbeta(c(0.025,0.975), parsBeta_p_hosp$alpha, parsBeta_p_hosp$beta))

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_prob_death = rbeta(n_sims_montecarlo, parsBeta_p_hosp$alpha, parsBeta_p_hosp$beta)

##############
### n_hosp ###
##############

### UPDATE TO USE TEST MEAN AND TEST SD INSTEAD OF OVER COMPLICATED RE-ETSIMATION APPROACH BELOW
n_hosp_mean = mean(cfr$cases)
n_hosp_sd = sd(cfr$cases)/sqrt(length(cfr$cases))

set.seed(20230831)
vec_n_hosp <- rnorm(n=10000, mean=n_hosp_mean, sd=n_hosp_sd)

##############
### p_hosp ###
##############

### Calculate probability of hospitalisation based on number of hospitalisations (these data) and number of infections (transmission model)

### From raw data [not possible from Github, underlying data too large to be loaded]

# data_n_infect=loadRData("lassa/simulations/99_runs/outputs_clean_00/list_Lassa_00_byDistrict_annual.Rdata")%>%
#   do.call(rbind,.)
# 
# data_n_infect_edo = data_n_infect%>%
#   filter(catchmentID == "NGA.12_1", scenario == 1)
# 
# data_n_infect_ondo = data_n_infect%>%
#   filter(catchmentID == "NGA.29_1", scenario == 1)
# 
# df_LASV_incidence_annual_edo_ondo = rbind(data_n_infect_edo, data_n_infect_ondo)

data_n_infect_districts_high_surveillance = read.csv("HealthEconomics/data_econ/df_LASV_incidence_annual_edo_ondo.csv")


vec_n_infect = sample(c(data_n_infect_districts_high_surveillance$unpruned), 10000, replace = T)

### Check values
# round(quantile(vec_n_infect, probs=c(0.025,0.5,0.975)),4)

### so estimated probability of hospitalization based on numbers of infections and hospitalizations:
vec_prob_hosp_inf <- vec_n_hosp/vec_n_infect
p_hosp_mean = mean(vec_prob_hosp_inf)
p_hosp_sd = sd(vec_prob_hosp_inf)

### Check values
# c(mean(rnorm(10000, p_hosp_mean, p_hosp_sd)), qnorm(c(0.025,0.975), p_hosp_mean, p_hosp_sd))

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_prob_hosp = rnorm(n_sims_montecarlo, p_hosp_mean, p_hosp_sd)

##################
### p_sequelae ###
##################

### This parameter is considered in sensitivity analysis (two distinct point estimates) so no distributional assumptions

### LOW SCENARIO (Ficenec et al. estimate from Sierra Leone case-control study)
p_sequelae_low = 0.17

### HIGH SCENARIO (ENABLE)
p_sequelae_high = 0.63


#################
### DURATIONS ###
#################

#########################
### DURATION OF FEVER ###
#########################

# From: https://www.sciencedirect.com/science/article/pii/S1201971220324905?via%3Dihub

d_fever_mean = 3.53
d_fever_sd = 1.99/sqrt(261)

### Check values
# qnorm(c(0.025,0.975), d_fever_mean, d_fever_sd)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_dur_fever = rnorm(n_sims_montecarlo, d_fever_mean, d_fever_sd)

################################
### DURATION ILL PRE-HOSPITAL###
################################

### these and following duration estimates from the LASCOPE study
# https://www.sciencedirect.com/science/article/pii/S2214109X20305180?via%3Dihub

d_ill_prehosp_q1 = 7
d_ill_prehosp_m = 8
d_ill_prehosp_q3 = 13
d_ill_prehosp_n = 510

vec_d_ill_prehosp = f_dist_from_quartiles(d_ill_prehosp_q1, d_ill_prehosp_m, d_ill_prehosp_q3, d_ill_prehosp_n)
d_ill_prehosp_mean = vec_d_ill_prehosp['X']
d_ill_prehosp_SE = vec_d_ill_prehosp['S']/sqrt(d_ill_prehosp_n)

## Check values
# qnorm(c(0.025,0.975), d_ill_prehosp_mean, d_ill_prehosp_SE)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_dur_ill_prehosp = rnorm(n_sims_montecarlo, d_ill_prehosp_mean, d_ill_prehosp_SE)

############################
### DURATION HOSP (DIED) ###
############################

d_hosp_died_q1 = 1
d_hosp_died_m = 3
d_hosp_died_q3 = 6
d_hosp_died_n = 61

vec_d_hosp_died = f_dist_from_quartiles(d_hosp_died_q1, d_hosp_died_m, d_hosp_died_q3, d_hosp_died_n)
d_hosp_died_mean = vec_d_hosp_died['X']
d_hosp_died_SE = vec_d_hosp_died['S']/sqrt(d_hosp_died_n)

### Check values
# qnorm(c(0.025,0.975), d_hosp_died_mean, d_hosp_died_SE)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_dur_hosp_died = rnorm(n_sims_montecarlo, d_hosp_died_mean, d_hosp_died_SE)

################################
### DURATION HOSP (SURVIVED) ###
################################

d_hosp_survived_q1 = 10
d_hosp_survived_m = 11
d_hosp_survived_q3 = 15
d_hosp_survived_n = 449

vec_d_hosp_survived = f_dist_from_quartiles(d_hosp_survived_q1, d_hosp_survived_m, d_hosp_survived_q3, d_hosp_survived_n)
d_hosp_survived_mean = vec_d_hosp_survived['X']
d_hosp_survived_SE = vec_d_hosp_survived['S']/sqrt(d_hosp_survived_n)

### Check values
# qnorm(c(0.025,0.975), d_hosp_survived_mean, d_hosp_survived_SE)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_dur_hosp_survived = rnorm(n_sims_montecarlo, d_hosp_survived_mean, d_hosp_survived_SE)

#############
### DALYs ###
#############

########################
### Fever disutility ###
########################

daly_fever_mean = 0.051
daly_fever_sd = ((0.051-0.032)/1.96 + (0.074-0.051)/1.96)/2

### Check values
# qnorm(c(0.025,0.975), daly_fever_mean, daly_fever_sd)

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_disutility_fever = rnorm(n_sims_montecarlo, daly_fever_mean, daly_fever_sd)

###########################
### Sequelae disutility ###
###########################

# Proportions of overall hearing loss based on GBD estimates from Nigeria 
# (https://www.thelancet.com/cms/10.1016/S0140-6736(21)00516-X/attachment/a552cf62-9122-4b03-b1af-afce71be7e9e/mmc1.pdf)

prop_hearingloss_mild = 17700/26524
prop_hearingloss_moderate = 4740/26524
prop_hearingloss_moderatesevere = 1880/26524
prop_hearingloss_severe = 1250/26524
prop_hearingloss_profound = 571/26524
prop_hearingloss_complete = 383/26524

### GBD daly weights associated with hearing loss

# mild with ringing
daly_hearingloss_mild_mean = 0.021
daly_hearingloss_mild_lower = 0.012
daly_hearingloss_mild_upper = 0.036
daly_hearingloss_mild_sd = c((daly_hearingloss_mild_mean - daly_hearingloss_mild_lower)/1.96 + (daly_hearingloss_mild_upper - daly_hearingloss_mild_mean)/1.96)/2

# moderate with ringing
daly_hearingloss_moderate_mean = 0.074
daly_hearingloss_moderate_lower = 0.048
daly_hearingloss_moderate_upper = 0.107
daly_hearingloss_moderate_sd = c((daly_hearingloss_moderate_mean - daly_hearingloss_moderate_lower)/1.96 + (daly_hearingloss_moderate_upper - daly_hearingloss_moderate_mean)/1.96)/2

# moderately severe with ringing
daly_hearingloss_moderatesevere_mean = 0.167
daly_hearingloss_moderatesevere_lower = 0.114
daly_hearingloss_moderatesevere_upper = 0.231
daly_hearingloss_moderatesevere_sd = c((daly_hearingloss_moderatesevere_mean - daly_hearingloss_moderatesevere_lower)/1.96 + (daly_hearingloss_moderatesevere_upper - daly_hearingloss_moderatesevere_mean)/1.96)/2

# severe with ringing
daly_hearingloss_severe_mean = 0.261
daly_hearingloss_severe_lower = 0.174
daly_hearingloss_severe_upper = 0.361
daly_hearingloss_severe_sd = c((daly_hearingloss_severe_mean - daly_hearingloss_severe_lower)/1.96 + (daly_hearingloss_severe_upper - daly_hearingloss_severe_mean)/1.96)/2

# profound with ringing
daly_hearingloss_profound_mean = 0.277
daly_hearingloss_profound_lower = 0.182
daly_hearingloss_profound_upper = 0.388
daly_hearingloss_profound_sd =  c((daly_hearingloss_profound_mean - daly_hearingloss_profound_lower)/1.96 + (daly_hearingloss_profound_upper - daly_hearingloss_profound_mean)/1.96)/2

# complete with ringing
daly_hearingloss_complete_mean = 0.316
daly_hearingloss_complete_lower = 0.211
daly_hearingloss_complete_upper = 0.436
daly_hearingloss_complete_sd =  c((daly_hearingloss_complete_mean - daly_hearingloss_complete_lower)/1.96 + (daly_hearingloss_complete_upper - daly_hearingloss_complete_mean)/1.96)/2

###

n_samples = 100000

set.seed(20230906)
vec_daly_hearingloss_mild = rnorm(n_samples, daly_hearingloss_mild_mean, daly_hearingloss_mild_sd)
vec_daly_hearingloss_moderate = rnorm(n_samples, daly_hearingloss_moderate_mean, daly_hearingloss_moderate_sd)
vec_daly_hearingloss_moderatesevere = rnorm(n_samples, daly_hearingloss_moderatesevere_mean, daly_hearingloss_moderatesevere_sd)
vec_daly_hearingloss_severe = rnorm(n_samples, daly_hearingloss_severe_mean, daly_hearingloss_severe_sd)
vec_daly_hearingloss_profound = rnorm(n_samples, daly_hearingloss_profound_mean, daly_hearingloss_profound_sd)
vec_daly_hearingloss_complete = rnorm(n_samples, daly_hearingloss_complete_mean, daly_hearingloss_complete_sd)

vec_hearingloss_final = sample(c(vec_daly_hearingloss_mild,
                                 vec_daly_hearingloss_moderate,
                                 vec_daly_hearingloss_moderatesevere,
                                 vec_daly_hearingloss_severe,
                                 vec_daly_hearingloss_profound,
                                 vec_daly_hearingloss_complete),
                               size = n_samples,
                               replace = T,
                               prob = c(rep(prop_hearingloss_mild, n_samples), 
                                        rep(prop_hearingloss_moderate, n_samples),
                                        rep(prop_hearingloss_moderatesevere, n_samples),
                                        rep(prop_hearingloss_severe, n_samples),
                                        rep(prop_hearingloss_profound, n_samples),
                                        rep(prop_hearingloss_complete, n_samples)))

### check values
# c(mean(vec_hearingloss_final), quantile(vec_hearingloss_final, c(0.025, 0.975)))
# hist(vec_hearingloss_final)


### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_disutility_sequelae = sample(vec_hearingloss_final, 100)

##################################
### Hospitalization disutility ###
##################################

### Load Excel file with estimated number of patients with each symptom, and extract rows with associated disability
# NB: hearing loss excluded as considered as chronic sequelae
df_daly_hospital_raw = read.csv("HealthEconomics/data_econ/DALYs_weighting_v3_summarized.csv")%>%
  mutate(baseline_p = baseline_n/baseline_N,
         post_p = post_n/post_N)

vec_daly_overall_max = c()

n_samples = 100000

set.seed(20230908)
for(x in 1:n_samples){
  if(x%%1000==0){print(x)}
  
  ### BASELINE
  # which symptoms does each individual have at baseline
  vec_symptoms_baseline = rbinom(n = nrow(df_daly_hospital_raw), 
                                 size = 1,
                                 prob = df_daly_hospital_raw$baseline_p)

  # max baseline disutility; if no symptoms, take as zero
  if(sum(vec_symptoms_baseline) == 0){
    daly_baseline_max = 0
  }else{
    daly_baseline_max = max(df_daly_hospital_raw$disutility[which(vec_symptoms_baseline==1)])
  }
  
  
  ### POST BASELINE
  # which symptoms does each individual have post-baseline
  vec_symptoms_post = rbinom(n = nrow(df_daly_hospital_raw), 
                             size = 1,
                             prob = df_daly_hospital_raw$post_p)
  
  # max post-baseline disutility; if no symptoms, take as zero
  if(sum(vec_symptoms_baseline) == 0){
    daly_post_max = 0
  }else{
    daly_post_max = max(df_daly_hospital_raw$disutility[which(vec_symptoms_post==1)])
  }
  
  ### OVERALL ACROSS POST AND BASELINE
  daly_overall_max = (daly_baseline_max + daly_post_max)/2
  
  ### SAVE OUTPUTS TO VECTOR
  vec_daly_overall_max[x] = daly_overall_max
}

df_dalys_hospitalization = data.frame(timepoint = "overall", method = "max", DALY = vec_daly_overall_max)

df_dalys_hospitalization%>%group_by(timepoint, method)%>%
  summarise(lower = quantile(DALY, 0.025),
            mean = mean(DALY),
            upper = quantile(DALY, 0.975))

### PARAMETER VECTOR FOR MODEL INPUT
set.seed(20230908)
parvec_disutility_hospital = sample(vec_daly_overall_max, 100)


#################################
### FINAL PARAMETER DATAFRAME ###
#################################

df_params_montecarlo = data.frame(n_sim_montecarlo = 1:n_sims_montecarlo,
                                  prob_symptoms = parvec_prob_symptoms,
                                  prob_treat_comm_any = parvec_prob_treat_comm_any,
                                  prob_treat_comm_gvt = parvec_prob_treat_comm_gvt,
                                  prob_hosp = parvec_prob_hosp,
                                  prob_death = parvec_prob_death,
                                  dur_fever = parvec_dur_fever,
                                  dur_ill_prehosp = parvec_dur_ill_prehosp,
                                  dur_hosp_survived = parvec_dur_hosp_survived,
                                  dur_hosp_died = parvec_dur_hosp_died,
                                  disutility_fever = parvec_disutility_fever,
                                  disutility_hospital = parvec_disutility_hospital,
                                  disutility_sequelae = parvec_disutility_sequelae)

### Save dat
# save(df_params_montecarlo, file = "HealthEconomics/data_econ/params_montecarlo.Rdata")

#################################################
### UNIVARIATE SENSITIVITY ANALYSIS DATAFRAME ###
#################################################

df_params_montecarlo = loadRData("HealthEconomics/data_econ/params_montecarlo.Rdata")

vec_usa_prob_symptoms = c(mean(df_params_montecarlo$prob_symptoms), min(df_params_montecarlo$prob_symptoms), max(df_params_montecarlo$prob_symptoms), rep(mean(df_params_montecarlo$prob_symptoms), 22))
vec_usa_prob_treat_comm_any = c(rep(mean(df_params_montecarlo$prob_treat_comm_any), 3), min(df_params_montecarlo$prob_treat_comm_any), max(df_params_montecarlo$prob_treat_comm_any), rep(mean(df_params_montecarlo$prob_treat_comm_any), 20))
vec_usa_prob_treat_comm_gvt = c(rep(mean(df_params_montecarlo$prob_treat_comm_gvt), 5), min(df_params_montecarlo$prob_treat_comm_gvt), max(df_params_montecarlo$prob_treat_comm_gvt), rep(mean(df_params_montecarlo$prob_treat_comm_gvt), 18))
vec_usa_prob_hosp = c(rep(mean(df_params_montecarlo$prob_hosp), 7), min(df_params_montecarlo$prob_hosp), max(df_params_montecarlo$prob_hosp), rep(mean(df_params_montecarlo$prob_hosp), 16))
vec_usa_prob_death = c(rep(mean(df_params_montecarlo$prob_death), 9), min(df_params_montecarlo$prob_death), max(df_params_montecarlo$prob_death), rep(mean(df_params_montecarlo$prob_death), 14))
vec_usa_dur_fever = c(rep(mean(df_params_montecarlo$dur_fever), 11), min(df_params_montecarlo$dur_fever), max(df_params_montecarlo$dur_fever), rep(mean(df_params_montecarlo$dur_fever), 12))
vec_usa_dur_ill_prehosp = c(rep(mean(df_params_montecarlo$dur_ill_prehosp), 13), min(df_params_montecarlo$dur_ill_prehosp), max(df_params_montecarlo$dur_ill_prehosp), rep(mean(df_params_montecarlo$dur_ill_prehosp), 10))
vec_usa_dur_hosp_survived = c(rep(mean(df_params_montecarlo$dur_hosp_survived), 15), min(df_params_montecarlo$dur_hosp_survived), max(df_params_montecarlo$dur_hosp_survived), rep(mean(df_params_montecarlo$dur_hosp_survived), 8))
vec_usa_dur_hosp_died = c(rep(mean(df_params_montecarlo$dur_hosp_died), 17), min(df_params_montecarlo$dur_hosp_died), max(df_params_montecarlo$dur_hosp_died), rep(mean(df_params_montecarlo$dur_hosp_died), 6))
vec_usa_disutility_fever = c(rep(mean(df_params_montecarlo$disutility_fever), 19), min(df_params_montecarlo$disutility_fever), max(df_params_montecarlo$disutility_fever), rep(mean(df_params_montecarlo$disutility_fever), 4))
vec_usa_disutility_hospital = c(rep(mean(df_params_montecarlo$disutility_hospital), 21), min(df_params_montecarlo$disutility_hospital), max(df_params_montecarlo$disutility_hospital), rep(mean(df_params_montecarlo$disutility_hospital), 2))
vec_usa_disutility_sequelae = c(rep(mean(df_params_montecarlo$disutility_sequelae), 23), min(df_params_montecarlo$disutility_sequelae), max(df_params_montecarlo$disutility_sequelae))

df_params_usa = data.frame(n_sim_montecarlo = 1:25,
                                  prob_symptoms = vec_usa_prob_symptoms,
                                  prob_treat_comm_any = vec_usa_prob_treat_comm_any,
                                  prob_treat_comm_gvt = vec_usa_prob_treat_comm_gvt,
                                  prob_hosp = vec_usa_prob_hosp,
                                  prob_death = vec_usa_prob_death,
                                  dur_fever = vec_usa_dur_fever,
                                  dur_ill_prehosp = vec_usa_dur_ill_prehosp,
                                  dur_hosp_survived = vec_usa_dur_hosp_survived,
                                  dur_hosp_died = vec_usa_dur_hosp_died,
                                  disutility_fever = vec_usa_disutility_fever,
                                  disutility_hospital = vec_usa_disutility_hospital,
                                  disutility_sequelae = vec_usa_disutility_sequelae)

### Save dat
# save(df_params_usa, file = "HealthEconomics/data_econ/params_usa.Rdata")
