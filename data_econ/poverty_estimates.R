#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1. Background ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Author: Patrick Fahr
# Project: CEPI Economic Impact Assessment Lassa Fever
# Theme: Poverty estimation
# Date: 16/04/2023

# Note: https://pip.worldbank.org/#messages
  # Based on year 2018 and 2017 PPP $


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2. Packages and working directory ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd("/Users/patrickfher/Desktop/Lassa Data")

options(scipen=999)
options(digits = 3)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)

WorldPop_Africa <- read_excel("Population_Africa_WorldPopADM1.xlsx")


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3. Scenario data - Example for single district/catchment ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Reducing size of data frame and calculating total population size of country
  poverty_master <- WorldPop_Africa %>% 
    group_by(NAME_0) %>%
    mutate(total_pop_size = sum(Population)) %>%
    distinct(NAME_0, .keep_all = TRUE) %>%
    select(GID_0, NAME_0, total_pop_size)

# Defining number of people living below 2.15 US$ per day in 2017 PPP (World Bank data)
  poverty_master <- poverty_master %>% 
    mutate(n_below_2_15USD_pD_2017PPP = NA) %>%
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Nigeria", 61230000, n_below_2_15USD_pD_2017PPP)) %>%        # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Côte d'Ivoire", 2920000, n_below_2_15USD_pD_2017PPP)) %>%   # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Mauritania", 250000, n_below_2_15USD_pD_2017PPP)) %>%       # 2014 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Niger", 11430000, n_below_2_15USD_pD_2017PPP)) %>%          # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Ghana", 7450000, n_below_2_15USD_pD_2017PPP)) %>%           # 2016 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Gambia", 440000, n_below_2_15USD_pD_2017PPP)) %>%           # 2020 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Benin", 2380000, n_below_2_15USD_pD_2017PPP)) %>%           # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Mali", 2950000, n_below_2_15USD_pD_2017PPP)) %>%            # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Guinea", 1740000, n_below_2_15USD_pD_2017PPP)) %>%          # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Guinea-Bissau", 420000, n_below_2_15USD_pD_2017PPP)) %>%    # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Liberia", 1300000, n_below_2_15USD_pD_2017PPP)) %>%         # 2016 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Burkina Faso", 6230000, n_below_2_15USD_pD_2017PPP)) %>%    # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Togo", 2260000, n_below_2_15USD_pD_2017PPP)) %>%            # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Senegal", 1450000, n_below_2_15USD_pD_2017PPP)) %>%         # 2018 value
    mutate(n_below_2_15USD_pD_2017PPP = ifelse(NAME_0 == "Sierra Leone", 2050000, n_below_2_15USD_pD_2017PPP))        # 2018 value

# Subsetting data set to contries of interest
  poverty_master <- subset(poverty_master, !is.na(poverty_master$n_below_2_15USD_pD_2017PPP))

# Calculating percentage of total population living below 2.15 US$ per day  
  poverty_master <- poverty_master %>% mutate(percentage_below_2_15USD = (n_below_2_15USD_pD_2017PPP * 100) / total_pop_size)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4. Importing the scaled cost ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# These are the scaled Out Of Pocket HC expenditures based in a study from Nigeria
  load("scaling_master.rda")

# Linking the OOP cost data to the master data file  
  poverty_master <- poverty_master %>%
    left_join(scaling_master, by = c("GID_0", "NAME_0"))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 5. Estimate catastrophic health expenditures ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# This is country-specific

# Example Nigeria:
  # OOP is $926 in 2021 PPP$
  # Catastrophic OOP HC expenditure defined as >10% if income
  # This means an assumed income of 10 * $926 = $9260
  # This equals $25.4 per day (9260 / 365)
  # Now we need to look up number of people living below $25.4 a day
  # That number is 198,180,000

# Estimating country-specific dat rate
  poverty_master <- poverty_master %>%
    mutate(country_specific_oop_dayrate = countr_specific_OOP_2021IntD * 10 / 365)

# Estimating the number of individuals that are below the per day threshold associated with an income equal to 10% * 10 of the OOP costs
poverty_master <- poverty_master %>% 
  mutate(n_below_catastrophic_threshold = NA) %>%
  # Nigeria: 25.37
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Nigeria", 198380000, n_below_catastrophic_threshold)) %>%       # 2018 value
  # Côte d'Ivoire: 13.42
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Côte d'Ivoire", 24410000, n_below_catastrophic_threshold)) %>%  # 2018 value
  # Mauritania: 16.19
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Mauritania", 3700000, n_below_catastrophic_threshold)) %>%      # 2014 value
  # Niger: 16.58
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Niger", 22360000, n_below_catastrophic_threshold)) %>%          # 2018 value
  # Ghana: 13.03
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Ghana", 28440000, n_below_catastrophic_threshold)) %>%          # 2016 value
  # Gambia: 8.34
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Gambia", 2230000, n_below_catastrophic_threshold)) %>%           # 2016 value
  # Benin: 16.92
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Benin", 11640000, n_below_catastrophic_threshold)) %>%          # 2018 value
  # Mali: 11.28
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Mali", 18250000, n_below_catastrophic_threshold)) %>%           # 2018 value
  # Guinea: 21.30
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Guinea", 12540000, n_below_catastrophic_threshold)) %>%         # 2018 value
  # Guinea-Bissau: 23.44
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Guinea-Bissau", 1920000, n_below_catastrophic_threshold)) %>%   # 2018 value
  # Liberia: 19.57
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Liberia", 4690000, n_below_catastrophic_threshold)) %>%         # 2016 value
  # Burkina Faso: 12.48
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Burkina Faso", 18040000, n_below_catastrophic_threshold)) %>%   # 2018 value
  # Togo: 23.82
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Togo", 7990000, n_below_catastrophic_threshold)) %>%            # 2018 value
  # Senegal: 18.34
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Senegal", 15210000, n_below_catastrophic_threshold)) %>%        # 2018 value
  # Sierra Leone: 19.85
  mutate(n_below_catastrophic_threshold = ifelse(NAME_0 == "Sierra Leone", 7800000, n_below_catastrophic_threshold))        # 2018 value

# Calculating percentage of total population living below the pre-specified threshold of US$ per day  
  poverty_master <- poverty_master %>% mutate(percentage_below_catastrophic_threshold = (n_below_catastrophic_threshold * 100) / total_pop_size)
  table(poverty_master$percentage_below_catastrophic_threshold)
  # Note: some countries have a % > 100
  # Check: the population estimates do not add up
  # Solution: source WB population estimates based on same year as threshold value
  poverty_master <- poverty_master %>% 
    mutate(total_pop_size = ifelse(NAME_0 == "Nigeria", 198387623, total_pop_size)) %>%        # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Côte d'Ivoire", 25493988, total_pop_size)) %>%   # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Mauritania", 3843174, total_pop_size)) %>%       # 2014 value
    mutate(total_pop_size = ifelse(NAME_0 == "Niger", 22577058, total_pop_size)) %>%          # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Ghana", 29554303, total_pop_size)) %>%           # 2016 value
    mutate(total_pop_size = ifelse(NAME_0 == "Gambia", 2573995, total_pop_size)) %>%           # 2020 value
    mutate(total_pop_size = ifelse(NAME_0 == "Benin", 11940683, total_pop_size)) %>%           # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Mali", 19934298, total_pop_size)) %>%            # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Guinea", 12554864, total_pop_size)) %>%          # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Guinea-Bissau", 1924955, total_pop_size)) %>%    # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Liberia", 4706097, total_pop_size)) %>%         # 2016 value
    mutate(total_pop_size = ifelse(NAME_0 == "Burkina Faso", 20392723, total_pop_size)) %>%    # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Togo", 8046679, total_pop_size)) %>%            # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Senegal", 15574909, total_pop_size)) %>%         # 2018 value
    mutate(total_pop_size = ifelse(NAME_0 == "Sierra Leone", 7861281, total_pop_size))        # 2018 value
  
# Re-Calculating percentage of total population living below 2.15 US$ / $ threshold value per day  
  poverty_master <- poverty_master %>% mutate(percentage_below_2_15USD = (n_below_2_15USD_pD_2017PPP * 100) / total_pop_size)
  poverty_master <- poverty_master %>% mutate(percentage_below_catastrophic_threshold = (n_below_catastrophic_threshold * 100) / total_pop_size)

# Creating variable that indicates % of population at risk of catastrophic health expenditures when Lassa hits
  poverty_master <- poverty_master %>% mutate(prop_at_risk_catastrp_HCexp = percentage_below_catastrophic_threshold)

  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 6. Estimate risk of impoverishment due to health expenditures ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# These are the people above the 2.15 US$ poverty threshold and the the catastrophic expenditure threshold
  poverty_master <- poverty_master %>% mutate(prop_at_risk_impoverishing = percentage_below_catastrophic_threshold - percentage_below_2_15USD)

names(poverty_master)
poverty_master <- poverty_master[, c("GID_0", "NAME_0", "prop_at_risk_catastrp_HCexp", "prop_at_risk_impoverishing")]
save(poverty_master, file='poverty_master.rda')  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
