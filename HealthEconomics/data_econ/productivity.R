library(tidyverse)

folder_econ_inputs = c("HealthEconomics/data_econ/")

# PPP-adjusted Gross National Income per capita
df_GNI = read.csv(paste0(folder_econ_inputs, "GNI_PPPadj.csv"))%>%
  filter(GID_0 %in% vec_GID_0)%>%
  mutate(GNI_daily = GNI/365)%>%
  mutate(ref_area.label = case_when(country == "C?te d'Ivoire" ~ "C\x93te d'Ivoire",
                                    country == "Gambia, The" ~ "Gambia",
                                    T ~ country))


# participation in labour force
df_participation_labour_force = read.csv(paste0(folder_econ_inputs, "labour_force.csv"))%>%
  filter(ref_area.label %in% df_GNI$ref_area.label,
         sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-64")


df_prop_working = data.frame()
for(ref_area_i in levels(factor(df_participation_labour_force$ref_area.label))){
  
  df_participation_labour_force_i = filter(df_participation_labour_force,
                                           ref_area.label == ref_area_i)%>%
    dplyr::select(ref_area.label,
                  time,
                  obs_value)%>%
    mutate(prop_working = obs_value/100)
  
  max_time = max(df_participation_labour_force_i$time)
  
  df_prop_working = rbind(df_prop_working, df_participation_labour_force_i%>%
                            filter(time == max_time)%>%
                            dplyr::select(ref_area.label, prop_working))
  
}

# Combined GNI and prop working
df_GNI_prop_working = left_join(df_GNI, df_prop_working, by = c("ref_area.label"))

# save(df_GNI_prop_working, file = "J:/projects/lassa_vaccination/parameters_scenarios/data_econ/productivity_master.Rdata")