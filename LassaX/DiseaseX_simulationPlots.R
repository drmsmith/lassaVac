library(tidyverse)

source("J:/projects/lassa_vaccination/lassaX/housekeeping.R")

#################
### DATA PREP ###
#################

####################################################
### OUTPUT 1: INDIVIDUAL LONG FORMAT SIMULATIONS ###
####################################################
### 1 simulation, long format, 70% and 90% VE

## List: simulation output
list_diseaseX_output1 = loadRData("J:/projects/lassa_vaccination/lassaX/simulations/run1_longformat/list_diseaseX_i_outputSet_1_simulation_1.Rdata")

### BY DISTRICT ###
## Dataframe: collapse simulation output into a dataframe and 
## merge with population sizes from initial conditions to plot prevalence as a function of pop size
df_diseaseX_output1 = do.call(rbind, list_diseaseX_output1)%>%
  mutate(GID_1 = catchment,
         prevalence = E+I+VE+VI,
         prevalence_proportion = (E+I+VE+VI)/(S+E+I+R+V+VE+VI+VR),
         vacc_strategy = factor(vacc_strategy, 
                                levels = c("none", "same_everywhere_160d", "same_everywhere_100d"),
                                labels = c("no vaccine", "160d to first dose", "100d to first dose")),
         vacc_dose = factor(vacc_dosing, 
                            levels = c(0, 0.025, 0.2,0.4),
                            labels = c("none", "2.5%/yr", "20%/yr", "40%/yr")),
         vaccEff = factor(vaccEff, 
                          levels = c(0.5, 0.7, 0.9),
                          labels = c("50%", "70%", "90%"))
  )%>%
  left_join(., df_district_names%>%dplyr::select(GID_1, NAME_1), by = "GID_1")%>%
  mutate(vaccEff = case_when(vacc_strategy == "no vaccine" & vaccEff == "70%" ~ "no vaccine",
                             vacc_strategy == "no vaccine" & vaccEff == "90%" ~ "EXCESS",
                             T ~ vaccEff))%>%
  filter(vaccEff != "EXCESS")

### BY COUNTRY ###
df_diseaseX_output1_national = df_diseaseX_output1%>%
  group_by(time_adj, country, vacc_strategy, vacc_dosing, vacc_dose, vaccEff)%>%
  summarise(S = sum(S),
            E = sum(E),
            I = sum(I),
            R = sum(R),
            V = sum(V),
            VE = sum(VE),
            VI = sum(VI),
            VR = sum(VR),
            IncCumul_U = sum(IncCumul_U),
            IncCumul_V = sum(IncCumul_V),
            DosesCumul = sum(DosesCumul),
            prevalence = sum(prevalence),
            prevalence_proportion = (sum(E) + sum(I) + sum(VE) + sum(VI))/(sum(S) + sum(E) + sum(I) + sum(R) + sum(V) + sum(VE) + sum(VI) + sum(VR)))

### Numbers of interest
# cumulative incidence across all outbreaks
temp = df_diseaseX_output1%>%
  dplyr::filter(time == max(df_diseaseX_output1$time),
                vacc_strategy == "no vaccine")
sum(temp$IncCumul_U)


######################################
### PLOT ALL DISTRICTS, NO VACCINE ###
######################################

### Prevalence (total infections)
p_diseaseX_prevalence_noVacc_allDistricts = df_diseaseX_output1%>%
  filter(vacc_strategy == "no vaccine")%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = country, group = catchment))+
  geom_line(linewidth = 0.4)+
  theme_bw()+
  facet_wrap(facets = vars(country), scales = "free_y")+
  xlab("Time (days since outbreak detected detected)")+
  ylab("Number of infected individuals")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)
p_diseaseX_prevalence_noVacc_allDistricts

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_noVacc_allDistricts, filename = "p_diseaseX_prevalence_noVacc_allDistricts.png",
         width = 30, height = 15,
         units = "cm", bg="white")
}


### Prevalence (proportion infected)
p_diseaseX_prevalenceProportion_noVacc_allDistricts = df_diseaseX_output1%>%
  filter(vacc_strategy == "no vaccine")%>%
  ggplot(aes(x = time_adj, y = prevalence_proportion*100, colour = country, group = catchment))+
  geom_line(linewidth = 0.4)+
  theme_bw()+
  facet_wrap(facets = vars(country), scales = "free_y")+
  xlab("Time (days since outbreak detected detected)")+
  ylab("Infection prevalence (% of population)")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)
p_diseaseX_prevalenceProportion_noVacc_allDistricts

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalenceProportion_noVacc_allDistricts, filename = "p_diseaseX_prevalenceProportion_noVacc_allDistricts.png",
         width = 30, height = 15,
         units = "cm", bg="white")
}


### Cumulative incidence
p_diseaseX_cumulIncidence_noVacc_allDistricts = df_diseaseX_output1%>%
  filter(vacc_strategy == "no vaccine")%>%
  ggplot(aes(x = time_adj, y = IncCumul_U + IncCumul_V, colour = country, group = catchment))+
  geom_line(linewidth = 0.4)+
  theme_bw()+
  facet_wrap(facets = vars(country), scales = "free_y")+
  xlab("Time (days since outbreak detected detected)")+
  ylab("Cumulative incidence")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme(legend.position = "none")+
  scale_y_continuous(label = comma)
p_diseaseX_cumulIncidence_noVacc_allDistricts

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulIncidence_noVacc_allDistricts, filename = "p_diseaseX_cumulIncidence_noVacc_allDistricts.png",
         width = 30, height = 15,
         units = "cm", bg="white")
}


#####################################
### VACCINE IMPACT: ALL COUNTRIES ###
#####################################

### VE = 70% ###

# Prevalence
options(scipen = 1000000)
p_diseaseX_prevalence_vaccDosing_VE70_allCountries = df_diseaseX_output1_national%>%
  filter(vaccEff %in% c("no vaccine","70%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(country), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70_allCountries

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70_allCountries, filename = "p_diseaseX_prevalence_vaccDosing_VE70_allCountries.png",
         width = 40, height = 18,
         units = "cm", bg="white")
}


### VE: 70% vs 90% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70v90_allCountries = df_diseaseX_output1_national%>%
  filter(vacc_strategy == "160d to first dose" & vacc_dose == "20%/yr" | vacc_strategy == "no vaccine")%>%
  filter(vaccEff %in% c("no vaccine","70%", "90%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vaccEff))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(country), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccine efficacy", values = cols_vacc_efficacy)+
  geom_vline(xintercept = 160, linetype = 1, colour = "black", linewidth = 2, alpha = 0.2)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70v90_allCountries

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70v90_allCountries, filename = "p_diseaseX_prevalence_vaccDosing_VE70v90_allCountries.png",
         width = 40, height = 18,
         units = "cm", bg="white")
}


#################################################
### VACCINE IMPACT: HIGHLY IMPACTED COUNTRIES ###
#################################################

### VE = 70% ###

vec_countries_high_impact = c("Benin", "Burkina Faso", "Cote d'Ivoire", "Niger", "Ghana", "Nigeria")

# Prevalence
options(scipen = 1000000)
p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries = df_diseaseX_output1_national%>%
  filter(vaccEff %in% c("no vaccine","70%"), country %in% vec_countries_high_impact)%>%
  ggplot(aes(x = time_adj/365, y = prevalence, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(country), scales = "free_y")+
  theme_light()+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  xlab("Time (years since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1,3,4,2))+
  scale_y_continuous(label = comma)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"))+
  geom_point(data.frame(x = 0, y = 57, country = "Niger",
                        vacc_strategy = "no vaccine", vacc_dose = "none", vaccEff = "no vaccine"), 
             mapping = aes(x = x, y = y),
             show.legend = F, shape = 21, colour = "#e41a1c", fill = "#e41a1c", size = 1.3)
p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries, filename = "p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries.png",
         width = 40, height = 18,
         units = "cm", bg="white")
}

### VE = 70% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries = df_diseaseX_output1_national%>%
  filter(vaccEff %in% c("no vaccine","70%"), country %in% vec_countries_high_impact)%>%
  ggplot(aes(x = time_adj/365, y = prevalence, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.2)+
  facet_wrap(facets = vars(country), scales = "free_y")+
  theme_light()+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  xlab("Time (years since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1,3,4,2))+
  scale_y_continuous(label = comma)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"))+
  geom_point(data.frame(x = 0, y = 57, country = "Niger",
                        vacc_strategy = "no vaccine", vacc_dose = "none", vaccEff = "no vaccine"), 
             mapping = aes(x = x, y = y),
             show.legend = F, shape = 21, colour = "#e41a1c", fill = "#e41a1c", size = 1.3)
p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries, filename = "p_diseaseX_prevalence_vaccDosing_VE70_highImpactCountries.png",
         width = 40, height = 18,
         units = "cm", bg="white")
}




### VE: 70% vs 90% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70v90_highImpactCountries = df_diseaseX_output1_national%>%
  filter(vacc_strategy == "160d to first dose" & vacc_dose == "20%/yr" | vacc_strategy == "no vaccine", country %in% vec_countries_high_impact)%>%
  filter(vaccEff %in% c("no vaccine","70%", "90%"))%>%
  ggplot(aes(x = time_adj/365, y = prevalence, colour = vaccEff))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(country), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (years since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccine efficacy", values = cols_vacc_efficacy)+
  geom_vline(xintercept = 160/365, linetype = 1, colour = "black", linewidth = 2, alpha = 0.2)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70v90_highImpactCountries

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70v90_highImpactCountries, filename = "p_diseaseX_prevalence_vaccDosing_VE70v90_highImpactCountries.png",
         width = 40, height = 18,
         units = "cm", bg="white")
}


###############################
### VACCINE IMPACT: NIGERIA ###
###############################

### VE = 70% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70_Nigeria = df_diseaseX_output1%>%
  filter(country == "Nigeria", vaccEff %in% c("no vaccine","70%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Nigeria",
                      time == 0),
             mapping = aes(x = time_adj, y = prevalence),
             colour = cols_outbreak_detection)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70_Nigeria

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70_Nigeria, filename = "p_diseaseX_prevalence_vaccDosing_VE70_Nigeria.png",
         width = 40, height = 25,
         units = "cm", bg="white")
}


# Cumulative incidence
p_diseaseX_cumulIncidence_vaccDosing_VE70_Nigeria = df_diseaseX_output1%>%
  filter(country == "Nigeria", vaccEff %in% c("no vaccine","70%"))%>%
  ggplot(aes(x = time_adj, y = IncCumul_U + IncCumul_V, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Cumulative infection incidence")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Nigeria",
                      time == 0),
             mapping = aes(x = time_adj, y = IncCumul_U + IncCumul_V),
             colour = cols_outbreak_detection)+
  scale_y_continuous(label = comma)
p_diseaseX_cumulIncidence_vaccDosing_VE70_Nigeria

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulIncidence_vaccDosing_VE70_Nigeria, filename = "p_diseaseX_cumulIncidence_vaccDosing_VE70_Nigeria.png",
         width = 40, height = 25,
         units = "cm", bg="white")
}


# Cumulative doses
p_diseaseX_cumulDoses_vaccDosing_VE70_Nigeria = df_diseaseX_output1%>%
  filter(country == "Nigeria", vaccEff %in% c("70%"))%>%
  ggplot(aes(x = time_adj, y = DosesCumul/1000000, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1))+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Cumulative vaccine doses administered (million)")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay[2:3])+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_vline(xintercept = 100, colour= cols_vacc_delay[3])+
  geom_vline(xintercept = 160, colour = cols_vacc_delay[2])+
  scale_y_continuous(label = comma)
p_diseaseX_cumulDoses_vaccDosing_VE70_Nigeria

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulDoses_vaccDosing_VE70_Nigeria, filename = "p_diseaseX_cumulDoses_vaccDosing_VE70_Nigeria.png",
         width = 40, height = 25,
         units = "cm", bg="white")
}


### VE: 70% vs 90% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70v90_Nigeria = df_diseaseX_output1%>%
  filter(country == "Nigeria", 
         vacc_strategy == "160d to first dose" & vacc_dose == "20%/yr" | vacc_strategy == "no vaccine")%>%
  filter(vaccEff %in% c("no vaccine","70%", "90%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vaccEff))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccine efficacy", values = cols_vacc_efficacy)+
  #scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Nigeria",
                      time == 0),
             mapping = aes(x = time_adj, y = prevalence),
             colour = cols_outbreak_detection)+
  geom_vline(xintercept = 160, linetype = 1, colour = "black", linewidth = 2, alpha = 0.4)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70v90_Nigeria

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70v90_Nigeria, filename = "p_diseaseX_prevalence_vaccDosing_VE70v90_Nigeria.png",
         width = 40, height = 25,
         units = "cm", bg="white")
}


#####################################
### VACCINE IMPACT: COTE D'IVOIRE ###
#####################################

### VE = 70% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70_CotedIvoire = df_diseaseX_output1%>%
  filter(country == "Cote d'Ivoire", vaccEff %in% c("no vaccine","70%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  xlim(0,800)+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Cote d'Ivoire",
                      time == 0),
             mapping = aes(x = time_adj, y = prevalence),
             colour = cols_outbreak_detection)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70_CotedIvoire

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70_CotedIvoire, filename = "p_diseaseX_prevalence_vaccDosing_VE70_CotedIvoire.png",
         width = 30, height = 20,
         units = "cm", bg="white")
}


# Cumulative incidence
p_diseaseX_cumulIncidence_vaccDosing_VE70_CotedIvoire = df_diseaseX_output1%>%
  filter(country == "Cote d'Ivoire", vaccEff %in% c("no vaccine","70%"))%>%
  ggplot(aes(x = time_adj, y = IncCumul_U + IncCumul_V, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Cumulative infection incidence")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  xlim(0,800)+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Cote d'Ivoire",
                      time == 0),
             mapping = aes(x = time_adj, y = IncCumul_U + IncCumul_V),
             colour = cols_outbreak_detection)+
  scale_y_continuous(label = comma)
p_diseaseX_cumulIncidence_vaccDosing_VE70_CotedIvoire

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulIncidence_vaccDosing_VE70_CotedIvoire, filename = "p_diseaseX_cumulIncidence_vaccDosing_VE70_CotedIvoire.png",
         width = 30, height = 20,
         units = "cm", bg="white")
}


# Cumulative doses
p_diseaseX_cumulDoses_vaccDosing_VE70_CotedIvoire = df_diseaseX_output1%>%
  filter(country == "Cote d'Ivoire", vaccEff == "70%")%>%
  ggplot(aes(x = time_adj, y = DosesCumul/1000000, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1))+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Cumulative vaccine doses administered (million)")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay[2:3])+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_vline(xintercept = 100, colour= cols_vacc_delay[3])+
  geom_vline(xintercept = 160, colour = cols_vacc_delay[2])+
  scale_y_continuous(label = comma)
p_diseaseX_cumulDoses_vaccDosing_VE70_CotedIvoire

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulDoses_vaccDosing_VE70_CotedIvoire, filename = "p_diseaseX_cumulDoses_vaccDosing_VE70_CotedIvoire.png",
         width = 30, height = 20,
         units = "cm", bg="white")
}


### VE: 70% vs 90% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70v90_CotedIvoire = df_diseaseX_output1%>%
  filter(country == "Cote d'Ivoire", 
         vacc_strategy == "160d to first dose" & vacc_dose == "20%/yr" | vacc_strategy == "no vaccine")%>%
  filter(vaccEff %in% c("no vaccine","70%", "90%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vaccEff))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("vaccine efficacy", values = cols_vacc_efficacy)+
  #scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Cote d'Ivoire",
                      time == 0),
             mapping = aes(x = time_adj, y = prevalence),
             colour = cols_outbreak_detection)+
  geom_vline(xintercept = 160, linetype = 1, colour = "black", linewidth = 2, alpha = 0.4)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70v90_CotedIvoire

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70v90_CotedIvoire, filename = "p_diseaseX_prevalence_vaccDosing_VE70v90_CotedIvoire.png",
         width = 40, height = 25,
         units = "cm", bg="white")
}


#####################################
### VACCINE IMPACT: NIGER ###
#####################################

### VE = 70% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70_Niger = df_diseaseX_output1%>%
  filter(country == "Niger", vaccEff %in% c("no vaccine","70%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  xlim(0,800)+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Niger",
                      time == 0),
             mapping = aes(x = time_adj, y = prevalence),
             colour = cols_outbreak_detection)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70_Niger

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70_Niger, filename = "p_diseaseX_prevalence_vaccDosing_VE70_Niger.png",
         width = 25, height = 12,
         units = "cm", bg="white")
}


# Cumulative incidence
p_diseaseX_cumulIncidence_vaccDosing_VE70_Niger = df_diseaseX_output1%>%
  filter(country == "Niger", vaccEff %in% c("no vaccine", "70%"))%>%
  ggplot(aes(x = time_adj, y = IncCumul_U + IncCumul_V, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Cumulative infection incidence")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay)+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  xlim(0,800)+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Niger",
                      time == 0),
             mapping = aes(x = time_adj, y = IncCumul_U + IncCumul_V),
             colour = cols_outbreak_detection)+
  scale_y_continuous(label = comma)
p_diseaseX_cumulIncidence_vaccDosing_VE70_Niger

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulIncidence_vaccDosing_VE70_Niger, filename = "p_diseaseX_cumulIncidence_vaccDosing_VE70_Niger.png",
         width = 25, height = 12,
         units = "cm", bg="white")
}


# Cumulative doses
p_diseaseX_cumulDoses_vaccDosing_VE70_Niger = df_diseaseX_output1%>%
  filter(country == "Niger", vaccEff == "70%")%>%
  ggplot(aes(x = time_adj, y = DosesCumul/1000000, colour = vacc_strategy, linetype = vacc_dose))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1))+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Cumulative vaccine doses administered (million)")+
  scale_colour_manual("Vaccination delay", values = cols_vacc_delay[2:3])+
  scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_vline(xintercept = 100, colour= cols_vacc_delay[3])+
  geom_vline(xintercept = 160, colour = cols_vacc_delay[2])+
  scale_y_continuous(label = comma)
p_diseaseX_cumulDoses_vaccDosing_VE70_Niger

if(qSavePlot == T){
  ggsave(p_diseaseX_cumulDoses_vaccDosing_VE70_Niger, filename = "p_diseaseX_cumulDoses_vaccDosing_VE70_Niger.png",
         width = 25, height = 12,
         units = "cm", bg="white")
}


### VE: 70% vs 90% ###

# Prevalence
p_diseaseX_prevalence_vaccDosing_VE70v90_Niger = df_diseaseX_output1%>%
  filter(country == "Niger", 
         vacc_strategy == "160d to first dose" & vacc_dose == "20%/yr" | vacc_strategy == "no vaccine")%>%
  filter(vaccEff %in% c("no vaccine","70%", "90%"))%>%
  ggplot(aes(x = time_adj, y = prevalence, colour = vaccEff))+
  geom_line(linewidth = 0.4)+
  facet_wrap(facets = vars(NAME_1), scales = "free_y")+
  theme_minimal()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  xlab("Time (days since outbreak detected)")+
  ylab("Number of infected individuals")+
  scale_colour_manual("Vaccine efficacy", values = cols_vacc_efficacy)+
  #scale_linetype_manual("Population vaccinated", values = c(1:4))+
  geom_point(data = df_diseaseX_output1%>%
               filter(country == "Niger",
                      time == 0),
             mapping = aes(x = time_adj, y = prevalence),
             colour = cols_outbreak_detection)+
  geom_vline(xintercept = 160, linetype = 1, colour = "black", linewidth = 2, alpha = 0.4)+
  scale_y_continuous(label = comma)
p_diseaseX_prevalence_vaccDosing_VE70v90_Niger

if(qSavePlot == T){
  ggsave(p_diseaseX_prevalence_vaccDosing_VE70v90_Niger, filename = "p_diseaseX_prevalence_vaccDosing_VE70v90_Niger.png",
         width = 25, height = 12,
         units = "cm", bg="white")
}


