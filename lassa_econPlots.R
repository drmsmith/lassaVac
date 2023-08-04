#########################
### LOAD HOUSEKEEPING ###
#########################

fix_accents = T
source("J:/projects/lassa_Vaccination/lassaX/housekeeping.R")

setwd("J:/projects/lassa_vaccination/lassa")

df_country_names = df_district_names%>%
  dplyr::select(GID_0, COUNTRY)%>%
  unique()

# howManyRuns = 1
howManyRuns = 10

if(howManyRuns == 1){
  # filepaths
  filepath_runs = c("J:/projects/lassa_Vaccination/lassa/outcomes/1run/")
}

if(howManyRuns == 10){
  # filepaths
  filepath_runs = c("J:/projects/lassa_Vaccination/lassa/outcomes/10runs/")
}

#################
### LOAD DATA ###
#################

library(cowplot)

cols_vacc_strategy = c("#998ec3", "#f1a340")

# cols_scenarios = c('#1b9e77',
#                       '#fdcc8a','#fc8d59','#d7301f',
#                       "darkred",
#                       '#9e9ac8','#6a51a3')

cols_scenarios = pal_lancet()(7)

vec_vaccEff_levels = c("0_70", "0_90", "70_70","90_90")
vec_vaccEff_labels = c("0%/70% VE", "0%/90% VE", "70%/70% VE", "90%/90% VE")

options(scipen = 10000000)

df_outputs_total_summarized = loadRData(paste0(filepath_runs, "df_outputs_total_summarized.Rdata"))%>%
  left_join(., df_country_names, by = "GID_0")
df_outputs_total_summarized_allRegion = loadRData(paste0(filepath_runs, "df_outputs_total_summarized_allRegion.Rdata"))

df_outputs_averted_total_summarized = loadRData(paste0(filepath_runs, "df_outputs_averted_total_summarized.Rdata"))%>%
  mutate(vaccEff = factor(vaccEff, levels = vec_vaccEff_levels, labels = vec_vaccEff_labels),
         scenario = factor(scenario))%>%
  left_join(., df_country_names, by = "GID_0")
df_outputs_averted_total_summarized_allRegion = loadRData(paste0(filepath_runs, "df_outputs_averted_total_summarized_allRegion.Rdata"))%>%
  mutate(vaccEff = factor(vaccEff, levels = vec_vaccEff_levels, labels = vec_vaccEff_labels),
         scenario = factor(scenario))

#########################
### EXPLORATORY PLOTS ###
#########################

#####################
### PLOT RAW DATA ###
#####################

### No vaccine, high prob_hosp: DALYs per country for each simulation
head(m_outputs)

cols_heterogeneity = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd')

p_simulation_heterogeneity = df_outputs%>%
  filter(scenario == 1,
         prob_hosp == 0.015,
         GID_0 %in% c("NGA", "BEN", "MRT"),
         vaccEff == "70_70")%>%
  left_join(., df_country_names, by = "GID_0")%>%
  ggplot(., aes(x = factor(simulation), y = DALY_total_disc, colour = factor(simulation)))+
  geom_jitter(height = 0, alpha = 0.6)+
  theme_light()+
  facet_grid(rows = vars(COUNTRY), scales = "free")+
  theme(legend.position = "none")+
  xlab("Outbreak simulation")+ylab("Annual DALYs (discounted)")+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = rep(cols_heterogeneity, 10))
p_simulation_heterogeneity

ggsave(p_simulation_heterogeneity, filename = "p_lassa_simulation_heterogeneity.png",
       width = 20, height = 12,
       units = "cm", bg="white")
ggsave(p_simulation_heterogeneity, filename = "p_lassa_simulation_heterogeneity.pdf",
       width = 20, height = 12,
       units = "cm", bg="white")



### Breakdown of DALYs: fever, hospital, sequelae, death (discounted vs undiscounted)

### Breakdown of productivity costs: fever, hospital, death (discounted vs undiscounted)


#########################################
### LASSA-X BURDEN PLOTS (NO VACCINE) ###
#########################################



### absolute incidence, cumulative
p_cases_cumul_by_country = df_outputs_total_summarized%>%
  filter(prob_hosp == 0.015,
         outcome == "N_cases")%>%
  ungroup()%>%
  ggplot(aes(y = median, ymin = q025, ymax = q975, x = COUNTRY, fill = COUNTRY))+
  geom_bar(stat = "identity", colour = "black")+
  theme_bw()+
  ylab(paste0("Infections"))+xlab("")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_errorbar(width = 0.2)+
  scale_y_continuous(label = comma)
p_cases_cumul_by_country


### incidence rate, cumulative
p_cases_incidence_by_country = df_outputs_total_summarized%>%
  filter(prob_hosp == 0.015,
         outcome == "N_cases")%>%
  ungroup()%>%
  ggplot(aes(y = median_rate*100000, ymin = q025_rate*100000, ymax = q975_rate*100000, x = COUNTRY, fill = COUNTRY))+
  geom_bar(stat = "identity", colour = "black")+
  theme_bw()+
  ylab(paste0("Infections (/100,000 population)"))+xlab("")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_errorbar(width = 0.2)
p_cases_incidence_by_country

### incidence rate (mortality)
p_cases_incidence_death_by_country = df_outputs_total_summarized%>%
  filter(prob_hosp == 0.015,
         outcome == "N_death")%>%
  ungroup()%>%
  ggplot(aes(y = median_rate*100000, ymin = q025_rate*100000, ymax = q975_rate*100000, x = COUNTRY, fill = COUNTRY))+
  geom_bar(stat = "identity", colour = "black")+
  theme_bw()+
  ylab(paste0("Deaths (/100,000 population)"))+xlab("")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_errorbar(width = 0.2)
p_cases_incidence_death_by_country

### incidence rate, cumulative (alternative: mean and 95%)
p_cases_incidence_by_country_alternative = df_outputs_total_summarized%>%
  filter(prob_hosp == 0.015,
         outcome == "N_cases")%>%
  ungroup()%>%
  ggplot(aes(y = mean_rate*100000, ymin = q025_rate*100000, ymax = q975_rate*100000, x = COUNTRY, fill = COUNTRY))+
  geom_bar(stat = "identity", colour = "black")+
  theme_bw()+
  ylab(paste0("Infections (/100,000 population)"))+xlab("")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_errorbar(width = 0.2)
p_cases_incidence_by_country_alternative


### DALYs by type of DALY
df_outputs_total_dalys_prep = df_outputs_total_summarized_allRegion%>%
  filter(prob_hosp == 0.015, 
         outcome %in% c("DALY_death", "DALY_death_disc", "DALY_fever", "DALY_hospital", "DALY_sequelae", "DALY_sequelae_disc"))%>%
  mutate(DALY_undisc = case_when(outcome %in% c("DALY_death", "DALY_fever", "DALY_hospital", "DALY_sequelae") ~ 1,
                                 T ~ 0),
         DALY_disc = case_when(outcome %in% c("DALY_death_disc", "DALY_fever", "DALY_hospital", "DALY_sequelae_disc") ~ 1,
                               T ~ 0))%>%
  dplyr::select(outcome, mean, q025, q975, DALY_undisc, DALY_disc)

df_outputs_total_dalys = rbind(df_outputs_total_dalys_prep%>%
                                 filter(DALY_undisc == 1)%>%
                                 mutate(whichDALY = "Undiscounted",
                                        Outcome = factor(outcome,
                                                         levels = c("DALY_fever", "DALY_hospital", "DALY_sequelae", "DALY_death"),
                                                         labels = c("Fever", "Hospitalisation", "Sequelae", "Death"))),
                               df_outputs_total_dalys_prep%>%
                                 filter(DALY_disc == 1)%>%
                                 mutate(whichDALY = "Discounted",
                                        Outcome = factor(outcome,
                                                         levels = c("DALY_fever", "DALY_hospital", "DALY_sequelae_disc", "DALY_death_disc"),
                                                         labels = c("Fever", "Hospitalisation", "Sequelae", "Death"))))

p_DALY_breakdown = ggplot(df_outputs_total_dalys, 
       aes(y = Outcome, x = mean, xmin = q025, xmax = q975, fill = Outcome))+
  geom_bar(position = "stack", stat = "identity", colour = "black", linewidth = 0.25)+
  geom_errorbar(width = 0.2, linewidth = 0.25)+
  theme_light()+
  scale_x_continuous(labels = comma)+
  xlab("Cumulative DALYs")+
  ylab("")+
  facet_grid(rows = vars(whichDALY))+
  theme(legend.position = "none")
p_DALY_breakdown

ggsave(p_DALY_breakdown, filename = "p_lassa_DALYbreakdown.png",
       width = 20, height = 6,
       units = "cm", bg="white")
ggsave(p_DALY_breakdown, filename = "p_lassa_DALYbreakdown.pdf",
       width = 20, height = 6,
       units = "cm", bg="white")

######################
### VACCINE IMPACT ###
######################

p_vaccImpact_allRegions_N_cases = df_outputs_averted_total_summarized_allRegion%>%
  filter(prob_hosp == 0.0006,
         outcome == "N_cases")%>%
  ungroup()%>%
  ggplot(aes(y = median, ymin = q025, ymax = q975, x = scenario, fill = scenario))+
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  theme_bw()+
  ylab(paste0("Infections averted"))+xlab("Scenario")+
  theme(legend.position = "none")+
  geom_errorbar(width = 0.2, position =position_dodge(.9))+
  facet_grid(cols = vars(vaccEff))+
  #scale_y_continuous(labels = function(x) gsub("000$", "k", x))+
  scale_y_continuous(labels = comma)+
  scale_fill_manual("Vaccination scenario", values = cols_scenarios)


p_vaccImpact_allRegions_N_deaths = df_outputs_averted_total_summarized_allRegion%>%
  filter(prob_hosp == 0.015,
         outcome == "N_death")%>%
  ungroup()%>%
  ggplot(aes(y = mean, ymin = q025, ymax = q975, x = scenario, fill = scenario))+
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  theme_bw()+
  ylab(paste0("Deaths averted"))+xlab("Scenario")+
  theme(legend.position = "none")+
  geom_errorbar(width = 0.2, position =position_dodge(.9))+
  facet_grid(cols = vars(vaccEff))+
  #scale_y_continuous(labels = function(x) gsub("000$", "k", x))+
  scale_y_continuous(labels = comma)+
  scale_fill_manual("Vaccination delay", values = cols_scenarios)

p_vaccImpact_allRegions_DALYs_total_disc = df_outputs_averted_total_summarized_allRegion%>%
  filter(prob_hosp == 0.015,
         outcome == "DALY_total_disc")%>%
  ungroup()%>%
  ggplot(aes(y = mean, ymin = q250, ymax = q750, x = scenario, fill = scenario))+
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  theme_bw()+
  ylab(paste0("DALYs averted"))+xlab("Scenario")+
  theme(legend.position = c(0.15,0.8))+
  geom_errorbar(width = 0.2, position =position_dodge(.9))+
  facet_grid(cols = vars(vaccEff))+
  #scale_y_continuous(labels = function(x) gsub("$", "M", x))+
  scale_y_continuous(labels = comma)+
  scale_fill_manual("Vaccination delay", values = cols_scenarios)

p_vaccImpact_allRegions_Cost_prod_total_disc = df_outputs_averted_total_summarized_allRegion%>%
  filter(prob_hosp == 0.015,
         outcome == "Cost_prod_total_disc")%>%
  ungroup()%>%
  ggplot(aes(y = mean/1000000, ymin = q250/1000000, ymax = q750/1000000, x = scenario, fill = scenario))+
  geom_bar(stat = "identity", position = position_dodge(), colour = "black")+
  theme_bw()+
  ylab(paste0("Productivity costs averted (USD $ 2021)"))+xlab("Scenario")+
  theme(legend.position = c(0.15,0.8))+
  geom_errorbar(width = 0.2, position =position_dodge(.9))+
  facet_grid(cols = vars(vaccEff))+
  #scale_y_continuous(labels = function(x) gsub("$", "M", x))+
  scale_y_continuous(labels = comma)+
  scale_fill_manual("Vaccination delay", values = cols_scenarios)



plot_grid(p_vaccImpact_allRegions_N_deaths,
          p_vaccImpact_allRegions_DALYs_total_disc+theme(legend.position = "none"),
          p_vaccImpact_allRegions_Cost_prod_total_disc+theme(legend.position = "none"),
          nrow = 3,
          align = "hv", axis = "tblr")
