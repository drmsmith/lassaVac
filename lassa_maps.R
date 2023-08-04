library(sf)
library(dplyr)
library(sp)
library(raster)
library(readr)
library(readxl)
library(maps)
library(scales)
library(ggplot2)

################
### GEO DATA ###
################

setwd("J:/projects/lassa_vaccination/lassa/")

# Read in GADM data
nga <- st_read("data/GADM/gadm41_NGA_shp/gadm41_NGA_1.shp")
ner <- st_read("data/GADM/gadm41_NER_shp/gadm41_NER_1.shp")
mli <- st_read("data/GADM/gadm41_MLI_shp/gadm41_MLI_1.shp")
ben <- st_read("data/GADM/gadm41_BEN_shp/gadm41_BEN_1.shp")
tgo <- st_read("data/GADM/gadm41_TGO_shp/gadm41_TGO_1.shp")
gha <- st_read("data/GADM/gadm41_GHA_shp/gadm41_GHA_1.shp")
civ <- st_read("data/GADM/gadm41_CIV_shp/gadm41_CIV_1.shp") %>%
  mutate(COUNTRY = "Cote d'Ivoire")
sle <- st_read("data/GADM/gadm41_SLE_shp/gadm41_SLE_1.shp")
lbr <- st_read("data/GADM/gadm41_LBR_shp/gadm41_LBR_1.shp")
gin <- st_read("data/GADM/gadm41_GIN_shp/gadm41_GIN_1.shp")
gnb <- st_read("data/GADM/gadm41_GNB_shp/gadm41_GNB_1.shp")
gmb <- st_read("data/GADM/gadm41_GMB_shp/gadm41_GMB_1.shp")
sen <- st_read("data/GADM/gadm41_SEN_shp/gadm41_SEN_1.shp")
mrt <- st_read("data/GADM/gadm41_MRT_shp/gadm41_MRT_1.shp")
bfa <- st_read("data/GADM/gadm41_BFA_shp/gadm41_BFA_1.shp")

# Create one GADM file
sfg_list <- dplyr::bind_rows(list(nga, ner, mli, ben, tgo, gha, civ, sle, gin, lbr, gnb, gmb, sen, mrt, bfa))

# Shape files for all of africa
africa = st_read("data/GADM/africa_shp/Africa_Boundaries.shp")


##################
### Lassa data ###
##################

### set folder with simulation outputs
folder_simulationOutputs = "J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/"

### Load final simulation outputs (quantiles)
df_Lassa_byDistrict_cumulTotals_quantiles = read.csv(paste0(folder_simulationOutputs, "df_Lassa_byDistrict_cumulTotals_quantiles.csv"))%>%
  mutate(GID_1 = catchmentID)

### Load final simulation outputs and summarize outcomes controlling for population size
df_Lassa_byDistrict_cumulTotals_quantiles_rate = df_Lassa_byDistrict_cumulTotals_quantiles%>%
  left_join(., df_district_names%>%dplyr::select(GID_1, Population_raster), by = "GID_1")%>%
  mutate(mean = (mean/Population_raster)*100000,
         median = (median/Population_raster)*100000,
         min = (min/Population_raster)*100000,
         max = (max/Population_raster)*100000,
         q250 = (q250/Population_raster)*100000,
         q750 = (q750/Population_raster)*100000)



##################
### MERGE DATA ###
##################

sfg_list_infections_averted = sfg_list%>%
  left_join(., df_Lassa_byDistrict_cumulTotals_quantiles%>%
              filter(outcome == "averted"), 
            by = c("GID_1"), multiple = "all")

sfg_list_infections_averted_rate = sfg_list%>%
  left_join(., df_Lassa_byDistrict_cumulTotals_quantiles_rate%>%
              filter(outcome == "averted"), 
            by = c("GID_1"), multiple = "all")


###################
### RENDER MAPS ###
###################

cols_infections_averted = c("white",'#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

### Mean number of infections averted ###

# using Africa shape file to explicitly map neighbouring countries outside W Africa region

## s1
sfg_list_infections_averted%>%
  filter(VE == 90, scenario == 1)%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = mean)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradient("Infections\naverted", low = 'white', high = '#084594', label = comma) +
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")

## s3
sfg_list_infections_averted%>%
  filter(VE == 90, scenario == 3)%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = mean)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradient("Infections\naverted", low = 'white', high = '#084594', label = comma) +
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")

## s5
sfg_list_infections_averted%>%
  filter(VE == 90, scenario == 5)%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = mean)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradientn("Infections\naverted", colours = cols_infections_averted, label = comma)+
  #scale_fill_gradient("Infections\naverted", low = 'white', high = '#084594', label = comma) +
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")

## s7
sfg_list_infections_averted%>%
  filter(VE == 90, scenario == 7)%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = mean)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  #scale_fill_gradientn("Thousand Lassa-X infections", colours = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')))+
  scale_fill_gradient("Infections\naverted", low = '#deebf7', high = '#084594', label = comma) +
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")

## scenarios as facets
p_sf_cases_averted_mean = sfg_list_infections_averted%>%
  filter(VE == 90, scenario %in% c(1,3, 5, 7))%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = mean)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradientn("Infections\naverted", colours = cols_infections_averted, label = comma)+
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")+
  facet_wrap(facets = vars(scenario))
p_sf_cases_averted_mean

if(qSavePlot == T){
  ggsave(p_sf_cases_averted_mean, file = "J:/projects/lassa_vaccination/lassa/plots/sf_cases_averted_mean.png",
         width = 30, height = 25, unit = "cm", bg = "white")
  ggsave(p_sf_cases_averted_mean, file = "J:/projects/lassa_vaccination/lassa/plots/sf_cases_mean.pdf",
         width = 30, height = 25, unit = "cm")
}

p_sf_cases_averted_mean_rate = sfg_list_infections_averted_rate%>%
  filter(VE == 90, scenario %in% c(1,3, 5, 7))%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = mean)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradientn("Infections\naverted\n(/100,000)", colours = cols_infections_averted, label = comma)+
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")+
  facet_wrap(facets = vars(scenario))
p_sf_cases_averted_mean_rate

if(qSavePlot == T){
  ggsave(p_sf_cases_averted_mean_rate, file = "J:/projects/lassa_vaccination/lassa/plots/sf_cases_averted_mean_rate.png",
         width = 30, height = 25, unit = "cm", bg = "white")
  ggsave(p_sf_cases_averted_mean_rate, file = "J:/projects/lassa_vaccination/lassa/plots/sf_cases_averted_mean_rate.pdf",
         width = 30, height = 25, unit = "cm")
}


### Mean incidence of infections ###
p_sf_cases_incidence = sfg_list_infections_incidence%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = IncCumul_U_noVacc_incidence)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  theme_void()+
  scale_fill_gradient("Infections\n(/100,000 population)", low = '#fee5d9', high = '#99000d', label = comma) +
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")
p_sf_cases_incidence

if(qSavePlot == T){
  ggsave(p_sf_cases_incidence, file = "J:/projects/lassa_vaccination/lassaX/plots/sf_cases_incidence.png",
         width = 18, height = 15, unit = "cm", bg = "white")
  ggsave(p_sf_cases_incidence, file = "J:/projects/lassa_vaccination/lassaX/plots/sf_cases_incidence.pdf",
         width = 18, height = 15, unit = "cm", bg = "white")
}


### Proportion of simulations with outbreaks
p_sf_outbreaks = sfg_list_outbreaks%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = hasOutbreak/100)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradient("Proportion of\nsimulations with\nLassa-X outbreak", low = '#fee5d9', high = '#99000d') + xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")
p_sf_outbreaks

if(qSavePlot == T){
  ggsave(p_sf_outbreaks, file = "J:/projects/lassa_vaccination/lassaX/plots/sf_cases_outbreaks.png",
         width = 18, height = 15, unit = "cm", bg = "white")
  ggsave(p_sf_outbreaks, file = "J:/projects/lassa_vaccination/lassaX/plots/sf_cases_outbreaks.pdf",
         width = 18, height = 15, unit = "cm", bg = "white")
}


p_sf_outbreaks_heat = sfg_list_outbreaks%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = hasOutbreak)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradientn("Proportion of simulations with Lassa-X outbreak", 
                       colours = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')))+
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")
p_sf_outbreaks_heat



sfg_list_outbreaks%>%
  group_by(GID_1)%>%
  ggplot() +
  geom_sf(data = africa, fill = "grey90", colour = NA)+
  geom_sf(aes(fill = hasOutbreak)) +
  geom_sf(data = africa, fill = NA, colour = "black")+
  #geom_sf_text(data = africa, aes(label = NAME_0), colour ="black", size = 3)+
  theme_void()+
  scale_fill_gradientn("Proportion of simulations with Lassa-X outbreak", 
                       colours = rev(c('black',
                                       '#67001f',
                                       rep('#a50026',3),
                                       rep('#d73027',5),
                                       rep('#f46d43', 10),
                                       rep('#fdae61', 10),
                                       rep('#fee090', 10),
                                       rep('#ffffbf', 10),
                                       rep('#e0f3f8', 10),
                                       rep('#abd9e9', 10),
                                       rep('#74add1', 10),
                                       rep('#4575b4', 10),
                                       rep('#313695', 10))))+
  xlim(-20, 15)+ylim(0,30)+
  theme(legend.position = "right",
        legend.direction = "vertical")
