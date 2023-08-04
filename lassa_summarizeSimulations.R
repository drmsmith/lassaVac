fix_accents = F
source("J:/projects/lassa_vaccination/lassaX/housekeeping.R")

#################
### SELECT VE ###
#################

VE_i = "70"

### working directories
wd_Lassa_csv = paste0("J:/projects/lassa_vaccination/lassa/simulations/99_runs/outputs_raw_", VE_i, "/")

wd_Lassa_clean = "J:/projects/lassa_vaccination/lassa/simulations/99_runs/outputs_clean/"

#################
### LOAD DATA ###
#################

list_Lassa_70_allRegion_daily = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_allRegion_daily.Rdata"))
list_Lassa_70_selectDistricts_daily = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_selectDistricts_daily.Rdata"))
list_Lassa_70_byDistrict_annual = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byDistrict_annual.Rdata"))
list_Lassa_70_byCountry_annual = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byCountry_annual.Rdata"))
list_Lassa_70_allRegion_annual = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_allRegion_annual.Rdata"))
list_Lassa_70_byDistrict_total = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byDistrict_total.Rdata"))
list_Lassa_70_byCountry_total = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_byCountry_total.Rdata"))
list_Lassa_70_allRegion_total = loadRData(paste0(wd_Lassa_clean, " list_Lassa_", VE_i,"_allRegion_total.Rdata"))

df_Lassa_70_allRegion_daily = do.call(rbind, list_Lassa_70_allRegion_daily)%>%
  mutate(VE = 70)
df_Lassa_70_selectDistricts_daily = do.call(rbind, list_Lassa_70_selectDistricts_daily)%>%
  mutate(VE = 70)
df_Lassa_70_byDistrict_annual = do.call(rbind, list_Lassa_70_byDistrict_annual)%>%
  mutate(VE = 70)
df_Lassa_70_byCountry_annual = do.call(rbind, list_Lassa_70_byCountry_annual)%>%
  mutate(VE = 70)
df_Lassa_70_allRegion_annual = do.call(rbind, list_Lassa_70_allRegion_annual)%>%
  mutate(VE = 70)
df_Lassa_70_byDistrict_total = do.call(rbind, list_Lassa_70_byDistrict_total)%>%
  mutate(VE = 70)
df_Lassa_70_byCountry_total = do.call(rbind, list_Lassa_70_byCountry_total)%>%
  mutate(VE = 70)
df_Lassa_70_allRegion_total = do.call(rbind, list_Lassa_70_allRegion_total)%>%
  mutate(VE = 70)

#####################################
### SUMMARIZE OUTPUTS: QUANTILES ###
#####################################

df_Lassa_70_allRegion_daily_quantiles = df_Lassa_70_allRegion_daily%>%
  pivot_longer(-c(scenario, run, time, VE), 
               names_to = "outcome",
               values_to = "value")%>%
  group_by(scenario, time, VE, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q025 = quantile(value,0.025),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75),
            q975 = quantile(value,0.975))

ggplot(df_Lassa_70_allRegion_daily_quantiles%>%filter(outcome == "averted"), 
       aes(x = time, y = mean, ymin = q025, ymax = q975, colour = factor(scenario), fill = factor(scenario)))+
  geom_line()+
  geom_ribbon(alpha = 0.2, colour = NA)
  

df_Lassa_csv_70_byDistrict_daily_quantiles_selectDistricts = df_Lassa_csv_70_byDistrict_daily%>%
  filter(catchmentID %in% vec_districts_highlighted)%>%
  pivot_longer(-c(scenario, run, country, catchmentID, time), 
               names_to = "outcome",
               values_to = "value")%>%
  filter(outcome == "averted_nc")%>%
  group_by(scenario, country, catchmentID, time, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q025 = quantile(value,0.025),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75),
            q975 = quantile(value,0.975),)%>%
  mutate(GID_1 = catchmentID,
         catchmentID_label = factor(catchmentID,
                                    levels = c("MLI.5_1", "NGA.20_1", "GIN.5_1"),
                                    labels = c("Koulikoro, Mali (non-endemic district, non-endemic country)",
                                               "Kano, Nigeria (non-endemic district, endemic country)",
                                               "Kindia, Guinea (endemic district, endemic country)")))%>%
  left_join(., df_district_names)

save(df_Lassa_csv_90_byDistrict_daily_quantiles_selectDistricts,
     file = "J:/projects/lassa_vaccination/lassa/simulations/df_Lassa_csv_90_byDistrict_daily_quantiles_selectDistricts.Rdata")


### Total outcomes, quantiles
df_Lassa_byDistrict_total_quantiles = df_Lassa_byDistrict_total%>%
  pivot_longer(-c(scenario, run, country, catchmentID, VE), 
               names_to = "outcome",
               values_to = "value")%>%
  group_by(scenario, country, catchmentID, outcome, VE)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75))

save(df_Lassa_byDistrict_total_quantiles,
     file = "J:/projects/lassa_vaccination/lassa/simulations/df_Lassa_byDistrict_total_quantiles.Rdata")

### Total outcomes, quantiles
df_Lassa_byCountry_total_quantiles = df_Lassa_byCountry_total%>%
  left_join(., df_country_names%>%dplyr::select(country, Population_raster), by = "country")%>%
  pivot_longer(-c(scenario, run, country, GID_0, Population_raster, VE), 
               names_to = "outcome",
               values_to = "value")%>%
  group_by(scenario, country, GID_0, outcome, VE)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75),
            mean_rate = mean(value)/max(Population_raster),
            median_rate = median(value)/max(Population_raster),
            min_rate = min(value)/max(Population_raster),
            max_rate = max(value)/max(Population_raster),
            q250_rate = quantile(value,0.25)/max(Population_raster),
            q750_rate = quantile(value,0.75)/max(Population_raster))

save(df_Lassa_byCountry_total_quantiles,
     file = "J:/projects/lassa_vaccination/lassa/simulations/df_Lassa_byCountry_total_quantiles.Rdata")

df_Lassa_allRegion_daily_quantiles = df_Lassa_allRegion_daily%>%
  pivot_longer(-c(scenario, VE, run, time), 
               names_to = "outcome",
               values_to = "value")%>%
  group_by(scenario, VE, time, outcome)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75))

save(df_Lassa_allRegion_daily_quantiles,
     file = "J:/projects/lassa_vaccination/lassa/simulations/df_Lassa_allRegion_daily_quantiles.Rdata")



### Annual outcomes, quantiles
df_Lassa_allRegion_annual_quantiles = df_Lassa_allRegion_annual%>%
  pivot_longer(-c(scenario, run, VE), 
               names_to = "outcome",
               values_to = "value")%>%
  group_by(scenario, outcome, VE)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75))

save(df_Lassa_allRegion_annual_quantiles,
     file = "J:/projects/lassa_vaccination/lassa/simulations/df_Lassa_allRegion_total_quantiles.Rdata")

### Total outcomes, quantiles
df_Lassa_allRegion_total_quantiles = df_Lassa_allRegion_total%>%
  pivot_longer(-c(scenario, run, VE), 
               names_to = "outcome",
               values_to = "value")%>%
  group_by(scenario, outcome, VE)%>%
  summarise(mean = mean(value),
            median = median(value),
            min = min(value),
            max = max(value),
            q250 = quantile(value,0.25),
            q750 = quantile(value,0.75))

save(df_Lassa_allRegion_total_quantiles,
     file = "J:/projects/lassa_vaccination/lassa/simulations/df_Lassa_allRegion_total_quantiles.Rdata")
