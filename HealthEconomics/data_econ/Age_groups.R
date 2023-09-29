#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1. Background ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Author: Patrick Fahr
# Project: CEPI Economic Impact Assessment Lassa Fever
# Theme: Age Group Estimation
# Date: 18/03/2023


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2. Packages and working directory ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

folder_econ_inputs = c("HealthEconomics/data_econ/") 


options(scipen=999)
options(digits = 3)
library(readxl)
library(dplyr)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3. UN Population Data by Age Group ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Data specification:
  # United Nations | Population Division | Department of Economic and Social Affairs
  # World Population Prospects 2022
  # File POP/01-1: Total population (both sexes combined) by single age, region
  # Estimates 1950 - 2021
  # Published 2022
  # Citation: United Nations, Department of Economic and Social Affairs, Population Division (2022). World Population Prospects 2022, Online Edition.
  
  # Note: Excel file adjusted for use in R

# Data import
  WPP2022 <- read_excel(paste0(folder_econ_inputs, "WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES_for_R.xlsx"))
  names(WPP2022)

# Converting single age variables into variable of class "numeric" 
  WPP2022 <- WPP2022 %>% mutate_at(c(10:110), as.numeric)

# Reduce data to only include the year 2021
  WPP2022 <- subset(WPP2022, WPP2022$Year == 2021)
  
# Reduce data to only include the type "Country/Area
  WPP2022 <- subset(WPP2022, WPP2022$Type == "Country/Area")

# Rename the Region variable
  WPP2022 <- WPP2022 %>% rename(NAME_0 = Region)

  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4. Load Africa World Pop Data ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
WorldPop_Africa <- read_excel(paste0(folder_econ_inputs, "Population_Africa_WorldPopADM1.xlsx"))
  nrow(WorldPop_Africa)
  names(WorldPop_Africa)
  length(unique(WorldPop_Africa$GID_0))
    
  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 5. Joining data frames ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
age_master <- left_join(WorldPop_Africa, WPP2022, by = "NAME_0")
  table(age_master$A0, useNA = "always")  
  length(unique(age_master$GID_0))


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 6. Estimating population size proportions by age group across entire country ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Here, we will estimate population size based on 6 pre-defined age groups:
  # <1 | 1-4 | 5-17 | 18-44 | 45-59 | >59
  
names(age_master)

# Rename Population variable to indicate that this refers to Adm1 level population
  age_master <- age_master %>% rename(Population_GID_1 = Population)
  
# Total UN Population (sum of individual age group counts)
  names(age_master)
  age_master$total_UN_population <- rowSums(age_master[14:114])
  age_master <- age_master %>% mutate(total_UN_population = total_UN_population * 1000) # numbers are in '1000, hence multiplication

# Age Group "Below 1" Proportion
  age_master$age_zero <- rowSums(age_master[14])
  age_master <- age_master %>% mutate(age_zero = age_zero * 1000) %>%
    mutate(age_zero = age_zero * 100 / total_UN_population)

# Age Group "1 to 4" Proportion
  age_master$age_one_four <- rowSums(age_master[15:18])
  age_master <- age_master %>% mutate(age_one_four = age_one_four * 1000) %>%
    mutate(age_one_four = age_one_four * 100 / total_UN_population)
  
# Age Group "5 to 17" Proportion
  age_master$age_five_seventeen <- rowSums(age_master[19:31])
  age_master <- age_master %>% mutate(age_five_seventeen = age_five_seventeen * 1000) %>%
    mutate(age_five_seventeen = age_five_seventeen * 100 / total_UN_population)

# Age Group "18 to 44" Proportion
  age_master$age_eighteen_fourtyfour <- rowSums(age_master[32:58])
  age_master <- age_master %>% mutate(age_eighteen_fourtyfour = age_eighteen_fourtyfour * 1000) %>%
    mutate(age_eighteen_fourtyfour = age_eighteen_fourtyfour * 100 / total_UN_population)
  
# Age Group "45 to 59" Proportion
  age_master$age_fourtyfive_fiftynine <- rowSums(age_master[59:73])
  age_master <- age_master %>% mutate(age_fourtyfive_fiftynine = age_fourtyfive_fiftynine * 1000) %>%
    mutate(age_fourtyfive_fiftynine = age_fourtyfive_fiftynine * 100 / total_UN_population)
  
# Age Group "above 59" Proportion
  age_master$age_above_fiftynine <- rowSums(age_master[74:114])
  age_master <- age_master %>% mutate(age_above_fiftynine = age_above_fiftynine * 1000) %>%
    mutate(age_above_fiftynine = age_above_fiftynine * 100 / total_UN_population)
  
# Sum of age group proportions (for quality check)
  age_master$sum_proportions <- rowSums(age_master[116:121])


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 7. Estimating population size by age group for each ADM1 region ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
names(age_master)  

# Age Group "Below 1" Proportion  
  age_master <- age_master %>% mutate(total_age_zero = Population_GID_1 * (age_zero/100))  
  
# Age Group "1 to 4" Proportion
  age_master <- age_master %>% mutate(total_age_one_four = Population_GID_1 * (age_one_four/100))  
  
# Age Group "5 to 17" Proportion
  age_master <- age_master %>% mutate(total_age_five_seventeen = Population_GID_1 * (age_five_seventeen/100))
  
# Age Group "18 to 44" Proportion
  age_master <- age_master %>% mutate(total_age_eighteen_fourtyfour = Population_GID_1 * (age_eighteen_fourtyfour/100))
 
# Age Group "45 to 59" Proportion
  age_master <- age_master %>% mutate(total_age_fourtyfive_fiftynine = Population_GID_1 * (age_fourtyfive_fiftynine/100))
  
# Age Group "above 59" Proportion
  age_master <- age_master %>% mutate(total_age_above_fiftynine = Population_GID_1 * (age_above_fiftynine/100))
  
# The sum of these 6 age groups is equal to the variable Population_GID_1 at row-level
  
  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 8. Estimating weighted average age within age groups ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Age group 2: 1 to 4 years
  names(age_master)
  age_master <- age_master %>% mutate(total_number_1to4years = rowSums(age_master[15:18]))
  age_master <- age_master %>%
    mutate(across(.cols = c(A1:A4), .fns =~ .x/total_number_1to4years, .names = "{.col}To_n_Size"))
  #dummy <- dummy %>%
  #mutate(across(.cols = c(A1To_n_Size:A4To_n_Size), .fns =~ .x * seq(1:4), .names = "{.col}weightSum")) 
  age_master <- age_master %>%
    mutate(A1To_n_Size = A1To_n_Size * 1) %>%
    mutate(A2To_n_Size = A2To_n_Size * 2) %>%
    mutate(A3To_n_Size = A3To_n_Size * 3) %>%
    mutate(A4To_n_Size = A4To_n_Size * 4)
  names(age_master)
  age_master <- age_master %>%
    mutate(weighted_avg_age_A1_to_A4 = rowSums(age_master[130:133]))
  names(age_master)  
  age_master <- age_master %>% select(-c(total_number_1to4years:A4To_n_Size))

# Age group 3: 5 to 17
  names(age_master)
  age_master <- age_master %>% mutate(total_number_5to17years = rowSums(age_master[19:31]))
  age_master <- age_master %>%
    mutate(across(.cols = c(A5:A17), .fns =~ .x/total_number_5to17years, .names = "{.col}To_n_Size"))
  #dummy <- dummy %>%
  #mutate(across(.cols = c(A1To_n_Size:A4To_n_Size), .fns =~ .x * seq(1:4), .names = "{.col}weightSum")) 
  age_master <- age_master %>%
    mutate(A5To_n_Size = A5To_n_Size * 5) %>%
    mutate(A6To_n_Size = A6To_n_Size * 6) %>%
    mutate(A7To_n_Size = A7To_n_Size * 7) %>%
    mutate(A8To_n_Size = A8To_n_Size * 8) %>%
    mutate(A9To_n_Size = A9To_n_Size * 9) %>%
    mutate(A10To_n_Size = A10To_n_Size * 10) %>%
    mutate(A11To_n_Size = A11To_n_Size * 11) %>%
    mutate(A12To_n_Size = A12To_n_Size * 12) %>%
    mutate(A13To_n_Size = A13To_n_Size * 13) %>%
    mutate(A14To_n_Size = A14To_n_Size * 14) %>%
    mutate(A15To_n_Size = A15To_n_Size * 15) %>%
    mutate(A16To_n_Size = A16To_n_Size * 16) %>%
    mutate(A17To_n_Size = A17To_n_Size * 17)
  names(age_master)
  age_master <- age_master %>%
    mutate(weighted_avg_age_A5_to_A17 = rowSums(age_master[131:143]))
  names(age_master)  
  age_master <- age_master %>% select(-c(total_number_5to17years:A17To_n_Size))
  
# Age Group 4: 18 to 44
  names(age_master)
  age_master <- age_master %>% mutate(total_number_18to44years = rowSums(age_master[31:58]))
  age_master <- age_master %>%
    mutate(across(.cols = c(A18:A44), .fns =~ .x/total_number_18to44years, .names = "{.col}To_n_Size"))
  #dummy <- dummy %>%
  #mutate(across(.cols = c(A1To_n_Size:A4To_n_Size), .fns =~ .x * seq(1:4), .names = "{.col}weightSum")) 
  age_master <- age_master %>%
    mutate(A18To_n_Size = A18To_n_Size * 18) %>%
    mutate(A19To_n_Size = A19To_n_Size * 19) %>%
    mutate(A20To_n_Size = A20To_n_Size * 20) %>%
    mutate(A21To_n_Size = A21To_n_Size * 21) %>%
    mutate(A22To_n_Size = A22To_n_Size * 22) %>%
    mutate(A23To_n_Size = A23To_n_Size * 23) %>%
    mutate(A24To_n_Size = A24To_n_Size * 24) %>%
    mutate(A25To_n_Size = A25To_n_Size * 25) %>%
    mutate(A26To_n_Size = A26To_n_Size * 26) %>%
    mutate(A27To_n_Size = A27To_n_Size * 27) %>%
    mutate(A28To_n_Size = A28To_n_Size * 28) %>%
    mutate(A29To_n_Size = A29To_n_Size * 29) %>%
    mutate(A30To_n_Size = A30To_n_Size * 30) %>%
    mutate(A31To_n_Size = A31To_n_Size * 31) %>%
    mutate(A32To_n_Size = A32To_n_Size * 32) %>%
    mutate(A33To_n_Size = A33To_n_Size * 33) %>%
    mutate(A34To_n_Size = A34To_n_Size * 34) %>%
    mutate(A35To_n_Size = A35To_n_Size * 35) %>%
    mutate(A36To_n_Size = A36To_n_Size * 36) %>%
    mutate(A37To_n_Size = A37To_n_Size * 37) %>%
    mutate(A38To_n_Size = A38To_n_Size * 38) %>%
    mutate(A39To_n_Size = A39To_n_Size * 39) %>%
    mutate(A40To_n_Size = A40To_n_Size * 40) %>%
    mutate(A41To_n_Size = A41To_n_Size * 41) %>%
    mutate(A42To_n_Size = A42To_n_Size * 42) %>%
    mutate(A43To_n_Size = A43To_n_Size * 43) %>%
    mutate(A44To_n_Size = A44To_n_Size * 44)
  names(age_master)
  age_master <- age_master %>%
    mutate(weighted_avg_age_A18_to_A44 = rowSums(age_master[132:158]))
  names(age_master)  
  age_master <- age_master %>% select(-c(total_number_18to44years:A44To_n_Size))
 
# Age Group 5: 45 to 59
  names(age_master)
  age_master <- age_master %>% mutate(total_number_45to59years = rowSums(age_master[59:73]))
  age_master <- age_master %>%
    mutate(across(.cols = c(A45:A59), .fns =~ .x/total_number_45to59years, .names = "{.col}To_n_Size"))
  #dummy <- dummy %>%
  #mutate(across(.cols = c(A1To_n_Size:A4To_n_Size), .fns =~ .x * seq(1:4), .names = "{.col}weightSum")) 
  age_master <- age_master %>%
    mutate(A45To_n_Size = A45To_n_Size * 45) %>%
    mutate(A46To_n_Size = A46To_n_Size * 46) %>%
    mutate(A47To_n_Size = A47To_n_Size * 47) %>%
    mutate(A48To_n_Size = A48To_n_Size * 48) %>%
    mutate(A49To_n_Size = A49To_n_Size * 49) %>%
    mutate(A50To_n_Size = A50To_n_Size * 50) %>%
    mutate(A51To_n_Size = A51To_n_Size * 51) %>%
    mutate(A52To_n_Size = A52To_n_Size * 52) %>%
    mutate(A53To_n_Size = A53To_n_Size * 53) %>%
    mutate(A54To_n_Size = A54To_n_Size * 54) %>%
    mutate(A55To_n_Size = A55To_n_Size * 55) %>%
    mutate(A56To_n_Size = A56To_n_Size * 56) %>%
    mutate(A57To_n_Size = A57To_n_Size * 57) %>%
    mutate(A58To_n_Size = A58To_n_Size * 58) %>%
    mutate(A59To_n_Size = A59To_n_Size * 59)
  names(age_master)
  age_master <- age_master %>%
    mutate(weighted_avg_age_A45_to_A59 = rowSums(age_master[133:147]))
  names(age_master)  
  age_master <- age_master %>% select(-c(total_number_45to59years:A59To_n_Size))
  
# Age Group 6: 59 and above
  names(age_master)
  age_master <- age_master %>% mutate(total_number_60to83years = rowSums(age_master[74:97])) # 83 chosen as highest life expectancy in Africa
  age_master <- age_master %>%
    mutate(across(.cols = c(A60:A83), .fns =~ .x/total_number_60to83years, .names = "{.col}To_n_Size"))
  #dummy <- dummy %>%
  #mutate(across(.cols = c(A1To_n_Size:A4To_n_Size), .fns =~ .x * seq(1:4), .names = "{.col}weightSum")) 
  age_master <- age_master %>%
    mutate(A60To_n_Size = A60To_n_Size * 60) %>%
    mutate(A61To_n_Size = A61To_n_Size * 61) %>%
    mutate(A62To_n_Size = A62To_n_Size * 62) %>%
    mutate(A63To_n_Size = A63To_n_Size * 63) %>%
    mutate(A64To_n_Size = A64To_n_Size * 64) %>%
    mutate(A65To_n_Size = A65To_n_Size * 65) %>%
    mutate(A66To_n_Size = A66To_n_Size * 66) %>%
    mutate(A67To_n_Size = A67To_n_Size * 67) %>%
    mutate(A68To_n_Size = A68To_n_Size * 68) %>%
    mutate(A69To_n_Size = A69To_n_Size * 69) %>%
    mutate(A70To_n_Size = A70To_n_Size * 70) %>%
    mutate(A71To_n_Size = A71To_n_Size * 71) %>%
    mutate(A72To_n_Size = A72To_n_Size * 72) %>%
    mutate(A73To_n_Size = A73To_n_Size * 73) %>%
    mutate(A74To_n_Size = A74To_n_Size * 74) %>%
    mutate(A75To_n_Size = A75To_n_Size * 75) %>%
    mutate(A76To_n_Size = A76To_n_Size * 76) %>%
    mutate(A77To_n_Size = A77To_n_Size * 77) %>%
    mutate(A78To_n_Size = A78To_n_Size * 78) %>%
    mutate(A79To_n_Size = A79To_n_Size * 79) %>%
    mutate(A80To_n_Size = A80To_n_Size * 80) %>%
    mutate(A81To_n_Size = A81To_n_Size * 81) %>%
    mutate(A82To_n_Size = A82To_n_Size * 82) %>%
    mutate(A83To_n_Size = A83To_n_Size * 83)
  names(age_master)
  age_master <- age_master %>%
    mutate(weighted_avg_age_A60_to_A83 = rowSums(age_master[134:157]))
  names(age_master)  
  age_master <- age_master %>% select(-c(total_number_60to83years:A83To_n_Size))  
  
  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 9. Reducing size of data frame and estimating remaining life years based on life expectancy ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#  
names(age_master)  
  
age_master <- age_master %>% select(-c(GID_1:total_age_above_fiftynine))
  
age_master <- age_master %>% filter(! duplicated(NAME_0))
  
summary(age_master$weighted_avg_age_A1_to_A4)
summary(age_master$weighted_avg_age_A5_to_A17)
summary(age_master$weighted_avg_age_A18_to_A44)
summary(age_master$weighted_avg_age_A45_to_A59)
summary(age_master$weighted_avg_age_A60_to_A83)

# UN life expectancy by age based on Year 2020
  UN_life_exp <- read_excel(paste0(folder_econ_inputs, "WPP2022_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES_short.xlsx"))
    names(UN_life_exp)
    UN_life_exp <- UN_life_exp %>%
      select(c(Name_0, GID_0, A3, A11, A28, A52, A68)) %>%
      rename(NAME_0 = Name_0)

age_master <- left_join(age_master, UN_life_exp, by = c("GID_0"))
  
age_master %>%
  filter(is.na(A3)) %>%
  filter(! duplicated(GID_0))
  # 1 countries without cost estimates

  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 10. END Save ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# save(age_master, file='age_master.rda')

