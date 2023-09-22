#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1 Background ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Author: Patrick Fahr
# Project: CEPI Economic Impact Assessment Lassa Fever
# Theme: WHO CHOICE Cost Estimation
# Date: 16/03/2023


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2 Packages and working directory ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

setwd("HealthEconomics/data_econ")

options(scipen=999)
library(readxl)
library(dplyr)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3 WHO CHOICE Data including economic indicators for calculation ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

who_choice <- read_excel("WHO_Choice_for_R.xlsx")
names(who_choice)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4 Updating 2010 cost to 2021 ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Needed:
  # 2010 CHOICE International $ estimates
  # Purchasing Power Parity Conversion factors for 2010 and 2021
  # GDP deflator: linked series (ratio of GDP current local currency and constant local currency) for 2010 and 2021

# Calculating GDP ratio
  who_choice <- who_choice %>% mutate(GDP_ratio = GDP_2021 / GDP_2010)

# Calculating 2010 Local Currency given 2010 US$ and using 2010 PPP
  who_choice <- who_choice %>% mutate(LCU_2010 = CHOICE_pred_2010_Int_Dollars * PPP_2010)

# Calculating 2021 Local Currency given GDP ratio estimate
  who_choice <- who_choice %>% mutate(LCU_2021 = LCU_2010 * GDP_ratio)

# Calculating 2021 International Dollar value  
  who_choice <- who_choice %>% mutate(Int_Dollar_2021 = LCU_2021 / PPP_2021)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 5. Checking countries with missing values ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

who_choice_missing <- subset(who_choice, is.na(who_choice$Int_Dollar_2021))
table(who_choice_missing$GID_0)

# We can use the CCEMG EPPI Centre Cost Converter to input missing values  
  # Note: data not available for all countries with missing values
  # Countries below indicate where data is available
  who_choice <- who_choice %>%
    mutate(Int_Dollar_2021 = ifelse(GID_0 == "DJI", 35.68, Int_Dollar_2021)) %>%
    mutate(Int_Dollar_2021 = ifelse(GID_0 == "ERI", 5.59, Int_Dollar_2021)) %>%
    mutate(Int_Dollar_2021 = ifelse(GID_0 == "KWT", 813.98, Int_Dollar_2021)) %>%
    mutate(Int_Dollar_2021 = ifelse(GID_0 == "SMR", 1236.97, Int_Dollar_2021)) %>%
    mutate(Int_Dollar_2021 = ifelse(GID_0 == "TKM", 189.5, Int_Dollar_2021)) %>%
    mutate(Int_Dollar_2021 = ifelse(GID_0 == "YEM", 152.8, Int_Dollar_2021))
    
who_choice_missing <- subset(who_choice, is.na(who_choice$Int_Dollar_2021))
table(who_choice_missing$GID_0)
rm(who_choice_missing)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 6. Joining to African countries ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Population data at Admin 1 level
  WorldPop_Africa <- read_excel("Population_Africa_WorldPopADM1.xlsx",
                                col_types = c("text", "text", "text",
                                              "text", "numeric"))
# Joining
  choice_master <- left_join(WorldPop_Africa, who_choice, by = "GID_0")
  table(choice_master$Int_Dollar_2021, useNA = "always")

# Checking missing data  
  choice_master %>%
    filter(is.na(Int_Dollar_2021)) %>%
    filter(! duplicated(GID_0))
    # 7 countries without cost estimates

# Using median Int Dollar 2021 price to populate missing values  
  median_intDol21 <- median(choice_master$Int_Dollar_2021, na.rm=TRUE)

choice_master <- choice_master %>%
  mutate(Int_Dollar_2021 = ifelse(is.na(Int_Dollar_2021), median_intDol21, Int_Dollar_2021))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 7. Saving master data frame ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# save(choice_master, file = "choice_master.Rda")
#write.csv(choice_master, file = "choice_master.csv")
