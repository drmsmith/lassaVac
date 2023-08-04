#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1. Background ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Author: Patrick Fahr
# Project: CEPI Economic Impact Assessment Lassa Fever
# Theme: VSL monetisation
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
# 4. VSL data ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# GDP per capita (current US$) data from World Bank
  vsl <- read_excel("VSL_for_R.xlsx")

# Joining
  vsl_master <- left_join(WorldPop_Africa, vsl, by = "NAME_0")

# Checking missing  
  vsl_master %>%
    filter(is.na(vsl_estimate_2021_IntDoll)) %>%
    filter(! duplicated(GID_0))
  # 10 countries without estimates

# For WB missing data: 
  vsl_master <- vsl_master %>% mutate(vsl_estimate_2021_IntDoll = ifelse(GID_0 == "COD", 22729.16417, vsl_estimate_2021_IntDoll))
  vsl_master <- vsl_master %>% mutate(vsl_estimate_2021_IntDoll = ifelse(GID_0 == "COG", 302547.4287, vsl_estimate_2021_IntDoll))
  vsl_master <- vsl_master %>% mutate(vsl_estimate_2021_IntDoll = ifelse(GID_0 == "EGY", 1418246.156, vsl_estimate_2021_IntDoll))
  vsl_master <- vsl_master %>% mutate(vsl_estimate_2021_IntDoll = ifelse(GID_0 == "GMB", 64287.78445, vsl_estimate_2021_IntDoll))
  vsl_master <- vsl_master %>% mutate(vsl_estimate_2021_IntDoll = ifelse(GID_0 == "SWZ", 966510.1256, vsl_estimate_2021_IntDoll))

  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 5. END SAVE ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
save(vsl_master, file='vsl_master.rda') 
  

