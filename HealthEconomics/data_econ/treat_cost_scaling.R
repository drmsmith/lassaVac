#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1 Background ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Author: Patrick Fahr
# Project: CEPI Economic Impact Assessment Lassa Fever
# Theme: Scaling Hospital Costs based on the study from Nigeria
# Date: 27/07/2023


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2 Packages and working directory ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
folder_econ_inputs = c("HealthEconomics/data_econ/") 

options(scipen=999)
options(digits = 5)
library(readxl)
library(dplyr)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3 Necessary data sets ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# World Pop Data on Africa developed by Paul
  WorldPop_Africa <- read_excel(paste0(folder_econ_inputs, "WordPop_Africa_short.xlsx"))
  names(WorldPop_Africa)
  nrow(WorldPop_Africa)
  WorldPop_Africa <- WorldPop_Africa %>% distinct(GID_0, .keep_all = TRUE)
  length(unique(WorldPop_Africa$GID_0))

  
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# 4 Method explainer on cost scaling #### 
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#  
  
# Background
  # Study: Medical cost of lassa fever treatment in Irrua specialist teaching hospital, Nigeria
  # Asogun et al., 2016
  # Cost estimates provided in local currency 2016 values
  # Average total Lassa treatment costs: N 205,559
  # Average total Lassa out-of-pocket costs: N 86,803

# Method for Cost Scaling to get country-specific cost-estimates
  # Method based on tradable resources (see method 3: Turner et al, 2019; VALUE HEALTH. 2019; 22(9):1026???1032)
  # 1. Take original value expressed in local currency
  # --> Avg Lassa treatment cost: N 205,559
  # 2. Convert to I$ with the exchange rate at the time of the costing (2016)
  # Note: WB data used (file name: PPP_conv_factor.xlsx)
  PPP_conv_factor <- read_excel(paste0(folder_econ_inputs, "PPP_conv_factor.xlsx"))
  names(PPP_conv_factor)
  PPP_conv_factor %>% filter(GID_0 == "NGA")
  # --> PPP I$ conversion factor 2016 for NGA: 105.373917
  # --> 205559 / 105.373917 = 1950.758 I$
  # 3. Inflate using the US$ inflation rate (divide GDP deflator)
  # Note: WB data used (file name: GDP_deflator.xlsx)
  GDP_deflator <- read_excel(paste0(folder_econ_inputs, "GDP_deflator.xlsx"))
  names(GDP_deflator)
  GDP_deflator %>% filter(GID_0 == "USA")
  # --> GDP_2021 / GDP_2016 = 113.568894993064 / 101.002235480218 = 1.12442
  # --> 1950.758 I$ * 1.12442 InflRate = 2193.471 $I
  treat_cost_2021IntD <- 2193.471
  
# Conversion for the out of pocket payments
  # 1) N 86,803 / 105.373917 = 823.7617
  # 2) 823.7617 I$ * 1.12442 InflRate = 926.2541 I$
  oop_cost_2021IntD <- 926.2541
  
# WB data on healthcare expenditures in Nigeria:
  # 1) Information on current per capita healthcare expenditure
    PcHCexpend <- read_excel(paste0(folder_econ_inputs, "Current expend HC IntPPP.xlsx"))
    names(PcHCexpend)
    PcHCexpend %>% filter(GID_0 == "NGA")
    # Per Capita Healthcare Expenditure I$ PPP in 2016: 190.9473114 I$
  # 2) Information on share of OOP expenditure
    OOP_proportion <- read_excel(paste0(folder_econ_inputs, "OOP expend proportion.xlsx"))  
    names(OOP_proportion)
    OOP_proportion %>% filter(GID_0 == "NGA")
    # Proportion of per Capita Healthcare Exp paid OOP I$ PPP in 2019: 70.52402496 (latest available estimate)
  # 3) OOP expenditure adjustment factor:
    # 1) What is the proportion of OOP paid in Nigeria study?
      # 926.2541 I$ / 2193.471 $I = 0.4222778 (42.2%)
    # 2) Develop an adjustment factor using the study OOP proportion and the WB OOP proportion
      # 42.2% / 70.5% = 0.5985816
      adjustment_factor <- 0.5985816
 
# Example: Translating Nigeria Study costs to Benin setting:
  # Benin % of total HC expenditure paid OOP in 2019:
    OOP_proportion %>% filter(GID_0 == "BEN")
    # = 47.04356766
  # Calc: Total direct Nigeria study costs in I$ * Benin OOP% * OOP adjustment factor
  # 2193.471 * (0.47 * 0.5985816) = = 617 I$
  
    
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# 5 Scaling of costs to otgher countries #### 
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-# 
  
# Share of OOP payments of total current healthcare expenditure WB data
  names(OOP_proportion)

# Only keep latest estimate (2019)
  OOP_proportion <- OOP_proportion %>% select(GID_0, OOP_2019)
    
# Join OOP data with the World Pop data
  scaling_master <- left_join(WorldPop_Africa, OOP_proportion, by = "GID_0")
  
# Checking countries without OOP proportion estimate
  scaling_master %>% filter(is.na(OOP_2019))
  # 6 countries without cost estimates
  
# Manually substituting OOP% values if data value can be found
  scaling_master <- scaling_master %>% mutate(OOP_2019 = ifelse(NAME_0 == "Libya", 36.66828156, OOP_2019)) # 2011 value
 
# Substituting remaining missing with median value
  median_oop <- median(scaling_master$OOP_2019, na.rm = TRUE)
  scaling_master <- scaling_master %>% mutate(OOP_2019 = ifelse(is.na(OOP_2019), median_oop, OOP_2019))
  
# Adding estimated values from section (4)
  scaling_master <- scaling_master %>%
    mutate(treat_cost_2021IntD = treat_cost_2021IntD) %>%
    mutate(adjustment_factor = adjustment_factor)
  
# Estimating country-specific OOP expenditures
  scaling_master <- scaling_master %>%
    mutate(countr_specific_OOP_2021IntD = treat_cost_2021IntD * (OOP_2019/100) * adjustment_factor)
  
  
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
# 6 Saving data set #### 
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-# 
# save(scaling_master, file='scaling_master.Rda')
  
