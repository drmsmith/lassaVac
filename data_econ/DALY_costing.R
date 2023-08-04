#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1. Background ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Author: Patrick Fahr
# Project: CEPI Economic Impact Assessment Lassa Fever
# Theme: DALY monetisation
# Date: 24/03/2023


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2. Packages and working directory ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd("/Users/patrickfher/Desktop/Lassa Data")

options(scipen=999)
options(digits = 3)
library(dplyr)
library(tidyr)
library(readxl)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3. World Pop Africa Data ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
WorldPop_Africa <- read_excel("Population_Africa_WorldPopADM1.xlsx")
  nrow(WorldPop_Africa)
  names(WorldPop_Africa)
  length(unique(WorldPop_Africa$GID_0))

WorldPop_Africa <- WorldPop_Africa[!duplicated(WorldPop_Africa$GID_0), ]
  

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4. GDP per Capita (2021 current US$) and creation of master linkage data ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# GDP per capita (current US$) data from World Bank
  gdp_pc_2021 <- read_excel("GDP_pC_for_R.xlsx")

# Joining
  daly_master <- left_join(WorldPop_Africa, gdp_pc_2021, by = "GID_0")

# Checking missing  
  daly_master %>%
    filter(is.na(GDP_pC_2021_currentUS)) %>%
    filter(! duplicated(GID_0))
  # 6 countries without estimates
  
# For WB missing data: 
  daly_master <- daly_master %>%
    mutate(GDP_pC_2021_currentUS = ifelse(GID_0 == "ERI", 614.26, GDP_pC_2021_currentUS))
  
names(daly_master)
daly_master$NAME_1 <- NULL
daly_master$Population <- NULL
daly_master$NAME_0.y <- NULL
daly_master$GID_1 <- NULL

# Rename the Region variable to country
  daly_master <- daly_master %>% 
    rename(NAME_0 = NAME_0.x)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4. DALY value estimates as % of GDP per capit and linkage to master DF ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# DALY value as percentage of GDP per capita based on Method 4 of:
  # Ochalek, Jessica, James Lomas, and Karl Claxton.
  # "Estimating health opportunity costs in low-income and middle-income countries:
  # a novel approach and evidence from cross-country data." BMJ global health 3.6 (2018): e000964.
  
daly_estimates <- read_excel("DALY_for_R.xlsx")

daly_master <- left_join(daly_master, daly_estimates, by = "NAME_0")

# For missing data:
  median_perc <- median(daly_master$perc_of_GDPpc, na.rm = TRUE)
  # Countries:
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Angola", median_perc, perc_of_GDPpc))
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Central African Republic", median_perc, perc_of_GDPpc))
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Côte d'Ivoire", 0.19, perc_of_GDPpc)) # Value from Ochalke et al.
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Republic of the Congo", 0.15, perc_of_GDPpc)) # Value from Ochalke et al.
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Cabo Verde", 0.84, perc_of_GDPpc)) # Value from Ochalke et al.
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Djibouti", median_perc, perc_of_GDPpc))
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Western Sahara", median_perc, perc_of_GDPpc))
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Gambia", 0.69, perc_of_GDPpc)) # Value from Ochalke et al.
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Equatorial Guinea", median_perc, perc_of_GDPpc)) 
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Liberia", median_perc, perc_of_GDPpc)) 
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "Libya", median_perc, perc_of_GDPpc)) 
  daly_master <- daly_master %>% mutate(perc_of_GDPpc = ifelse(NAME_0 == "South Sudan", 0.16, perc_of_GDPpc)) # Value from Ochalke et al.
  
daly_master$US_2015 <- NULL  

daly_master <- daly_master %>%
  mutate(daly_value = GDP_pC_2021_currentUS * perc_of_GDPpc)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 6. END SAVE ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
save(daly_master, file='daly_master.rda') 
