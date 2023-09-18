library(tidyverse)
library(incidence)
library(EpiEstim)
library(fitdistrplus)
library(deSolve)
library(cowplot)

###############################
### RUN CUSTOMIZED EPIESTIM ###
###############################

source("housekeeping.R")
source("lassaX/custom_EpiEstim.R")


##################
### EBOLA DATA ###
##################

### Complete West Africa Ebola 2014/16 line lists by district from Garske et al
#### https://royalsocietypublishing.org/doi/full/10.1098/rstb.2016.0308#d1e1659

### Load data
df_ebola_garske = read.csv("LassaX/data/rstb20160308_si_001.csv", stringsAsFactors = F)

### Clean data
# how many cases are missing district? inferred onset date?
sum(is.na(df_ebola_garske$CL_DistrictRes))
sum(is.na(df_ebola_garske$DateOnsetInferred))

# arrange data by district and date, and remove NAs
df_ebola_garske_clean = df_ebola_garske%>%
  mutate(district = CL_DistrictRes,
         dates = as.Date(DateOnsetInferred))%>%
  filter(!is.na(district),
         !is.na(Country),
         !is.na(dates))%>%
  arrange(district, dates)

# how many cases are missing district? inferred onset date?
sum(is.na(df_ebola_garske_clean$district))
sum(is.na(df_ebola_garske_clean$dates))

### Complete data (dates with 0 cases)
# complete missing cases data for plotting
df_ebola_garske_cases_by_t_district_incompl = df_ebola_garske_clean%>%
  mutate(dates = as.Date(DateOnsetInferred))%>%
  group_by(Country, district, dates)%>%
  summarize(I = n())

# outbreak dates
df_ebola_garske_cases_by_t_district_drange = data.frame(dates = seq(min(df_ebola_garske_cases_by_t_district_incompl$dates), 
                                                                    max(df_ebola_garske_cases_by_t_district_incompl$dates),1))


# complete data
df_ebola_garske_cases_by_t_district = left_join(df_ebola_garske_cases_by_t_district_drange, 
                                                df_ebola_garske_cases_by_t_district_incompl,
                                                multiple = "all")%>%
  complete(dates,
           nesting(Country, district),
           fill = list(I=0))%>%
  filter(!is.na(Country))


### serial interval distribution from WHO Response Team N Engl J Med
# https://www.nejm.org/doi/suppl/10.1056/NEJMoa1411100/suppl_file/nejmoa1411100_appendix1.pdf

# take value from Table S2, (i) single day exposures, and considering (ii) all delays
si_mean = 8.7
si_sd = 7.7
si_shape = 1.41
si_rate = 0.154


### Algorithm to splice outbreaks with long gaps into sub-outbreaks 
df_ebola_garske_split = data.frame()

for(district_i in levels(factor(df_ebola_garske_clean$district))){
  
  print(district_i)
  
  dat_i = filter(df_ebola_garske_clean, district == district_i)%>%
    mutate(split = ifelse(is.na(lag(dates,1)) | as.numeric(dates - lag(dates,1)) > qgamma(0.95,si_shape,si_rate),1, 0))%>%
    mutate(outbreak = cumsum(split))%>%
    dplyr::select(Country, district, dates, outbreak)
  
  df_ebola_garske_split = rbind(df_ebola_garske_split, dat_i)
  
}

###########################
### Plot line list data ### 
###########################

p_ebola_incidence_cumul_district = df_ebola_garske_cases_by_t_district_incompl%>%
  group_by(Country, district)%>%summarise(I_total = sum(I))%>%
  ggplot(aes(y = district, x = I_total, fill = Country))+
  geom_bar(stat = "identity")+
  theme_bw()+
  theme(legend.position = "none")+
  facet_grid(rows = vars(Country), scales = "free_y", space = "free_y")+
  xlab("Cumulative EVD incidence")+ylab("District")
p_ebola_incidence_cumul_district


# Plot cases
p_ebola_incidence_t_district = ggplot(df_ebola_garske_cases_by_t_district, aes(x = dates, y = I, colour = district))+
  geom_line(stat = "identity", linewidth = 0.2)+
  facet_grid(rows = vars(Country))+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Daily EVD incidence")+xlab("Date of onset")
p_ebola_incidence_t_district



p_ebola_incidence_both = plot_grid(p_ebola_incidence_cumul_district,
                                   p_ebola_incidence_t_district,
                                   ncol = 2,
                                   labels = c("a.", "b."))
p_ebola_incidence_both

#################################
### DISTRICT POPULATION SIZES ###
#################################

### Catchments included in Ebola data
vec_catchments_garski = levels(factor(df_ebola_garske_split$district))

### load catchments for Sierra Leone
# Sierra Leone district population sizes from the following link
# "2015 Population and Housing Census Final Results" (PDF). Statistics Sierra Leone. Retrieved 11 May 2023.
# from https://www.statistics.sl/images/StatisticsSL/Documents/final-results_-2015_population_and_housing_census.pdf
df_catchments_ebola_SLE = read.csv("LassaX/data/districts_sierraleone.csv", stringsAsFactors = F)%>%
  mutate(Population = gsub(",", "", Population))%>%
  # customize Western: in Garski data, Western Urban and Western Rural are combined as 1
  dplyr::filter(Province != "Western")%>%
  rbind(., data.frame(Country = "Sierra Leone",
                      District = "Western",
                      Province = "Western",
                      Capital = "Freetown",
                      Area = 557,
                      Population = 1500234))%>%
  mutate(District = toupper(District))%>%
  dplyr::select(Country, District, Population)

### load catchments for Guinea
# 2014 census, from the following link: https://www.stat-guinee.org/#
df_catchments_ebola_GIN = read.csv("LassaX/data/districts_guinea.csv", stringsAsFactors = F)%>%
  mutate(Population = gsub(",", "", Population),
         District = toupper(District))%>%
  dplyr::select(Country, District, Population)

### load catchments for Liberia
# straight from Lassa spillover data
df_catchments_ebola_LBR = read.csv("LassaX/data/incidence_ADM_1_PB_2_JT1.csv", stringsAsFactors = F)%>%
  dplyr::select(GID_0, COUNTRY, GID_1, NAME_1, Population, Population_raster)%>%
  dplyr::filter(COUNTRY %in% c("Liberia"))%>%
  mutate(Country = COUNTRY,
         District = toupper(NAME_1))%>%
  mutate(District = case_when(District == "GBAPOLU" ~ "GBARPOLU",
                              T ~ District))%>%
  dplyr::select(Country, District, Population)

### merge catchments
df_catchments_ebola = rbind(df_catchments_ebola_SLE,
                      df_catchments_ebola_GIN,
                      df_catchments_ebola_LBR)%>%
  filter(District %in% vec_catchments_garski)

### save this dataframe
# write.csv(df_catchments_ebola, "inputs_df_catchments_ebola.csv")


### merge catchments with case counts from Garski
df_catchments_ebola_cases = left_join(df_catchments_ebola,
                                df_ebola_garske_cases_by_t_district%>%
                                  mutate(District = district)%>%
                                  group_by(Country, District)%>%
                                  summarise(Cases_cumulative = sum(I)),
                                by = c("Country", "District"))%>%
  mutate(Population = as.numeric(Population))

### linear model of cases as function of population
model_cases_population = glm(Cases_cumulative_log10 ~ Population_log10, family = gaussian, data = df_catchments_ebola_cases%>%
                               mutate(Cases_cumulative_log10 = log10(Cases_cumulative),
                                      Population_log10 = log10(Population)))

df_model_cases_population = data.frame(Population_log10 = log10(seq(50000, 2000000, by = 1000)))
df_model_cases_population$Cases_cumulative_predict_log10 = predict(model_cases_population, 
                                                             newdata = df_model_cases_population)

p_ebola_correlation_N_incidence = ggplot(df_catchments_ebola_cases,
                                         aes(x = log10(Population), y = log10(Cases_cumulative), fill = Country))+
  geom_point(shape = 21)+
  theme_bw()+
  ylab("Cumulative incidence (EVD)")+xlab("District population size")+
  geom_line(data = df_model_cases_population, 
            mapping = aes(x = Population_log10, y = Cases_cumulative_predict_log10,
                          fill = NULL))+
  scale_x_continuous(breaks = c(5, 6),
                     labels = c(expression(10^5), expression(10^6)))+
  scale_y_continuous(breaks = c(0, 1,2,3,4),
                     labels = c(0, expression(10^1), expression(10^2), expression(10^3), expression(10^4)))
p_ebola_correlation_N_incidence



###################
### ESTIMATE RT ###
###################


### Estimate Rt for each of these outbreaks and save the output
list_Rt_ebola_i = list()

qounter = 0

for(district_i in levels(factor(df_ebola_garske_clean$district))){
  
  print(district_i)
  dat_i = filter(df_ebola_garske_split, district == district_i)
  
  n_outbreaks_i = max(dat_i$outbreak)
  
  # individually assess each "outbreak" within each district
  
  for(outbreak_i in 1:n_outbreaks_i){
    
    dat_i_outbreak = dat_i%>%filter(outbreak == outbreak_i)
    
    # extract vector of dates and of imported vs local cases
    dates_i = dat_i_outbreak$dates
    
    # don't include outbreaks with <10 cases
    if(length(dates_i) < 10){next()}
    qounter = qounter + 1
    
    # use incidence function to generate incidence summary from line list
    inc_i = incidence(dates_i)
    
    # estimate Rt for each district
    Rt_i = estimate_R(inc_i, 
                      method="parametric_si",
                      config = make_config(list(
                        mean_si = si_mean, 
                        std_si = si_sd)))
    
    # update results
    list_Rt_ebola_i[[qounter]] = Rt_i
    
    list_Rt_ebola_i[[qounter]]["district"] = district_i
    
    list_Rt_ebola_i[[qounter]]["district_outbreak"] = outbreak_i
    
    list_Rt_ebola_i[[qounter]]["outbreak_all"] = qounter
    
  }
}

### for each outbreak:
### - draw 1000 quantiles
### - draw over all gamma(a_posterior, b_posterior) for that quantile
### - save that as a simulated Rt curve


for(outbreak_i in 1:length(list_Rt_ebola_i)){
  
  print(paste0("on outbreak ", outbreak_i, " of ", length(list_Rt_ebola_i)))
  
  district_i = list_Rt_ebola_i[[outbreak_i]][["district"]]
  district_outbreak_i = list_Rt_ebola_i[[outbreak_i]][["district_outbreak"]]
  outbreak_all_i = list_Rt_ebola_i[[outbreak_i]][["outbreak_all"]]
  
  # extract gamma parameters and omit NAs
  vec_estimates_shape_i = na.omit(list_Rt_ebola_i[[outbreak_i]][["a_posterior"]])
  
  vec_estimates_scale_i = na.omit(list_Rt_ebola_i[[outbreak_i]][["b_posterior"]])
  
  if(length(vec_estimates_shape_i) != length(vec_estimates_scale_i)){warning("unequal number of posterior estimates")}
  
  # determine number of days (n_Rt_estimates, b/c 1 estimate/day) and number of simulations to run
  n_Rt_estimates = length(vec_estimates_shape_i)
  length_dissipateRt = 50
  n_Rt_simulations = 1000
  
  # draw quantiles from within 95%
  vec_quantiles_j = runif(n_Rt_simulations, 0.25, 0.75)
  
  # create empty matrix for storing results
  m_Rt_simulated = matrix(NA, nrow = n_Rt_simulations, ncol = n_Rt_estimates + length_dissipateRt + 4)
  
  colnames(m_Rt_simulated) <-  c("district", "district_outbreak", "outbreak_all", "quantile", 1:(n_Rt_estimates+length_dissipateRt))
  rownames(m_Rt_simulated) <- 1:n_Rt_simulations
  
  qounter_j = 0
  for(quantile_j in vec_quantiles_j){
    
    qounter_j = qounter_j + 1
    
    Rt_sims_j = qgamma(quantile_j, shape = vec_estimates_shape_i, scale = vec_estimates_scale_i)
    
    ### RT EXTENSIONS 1: NATURAL DECLINE
    # add "length_dissipateRt" values to taper Rt down to zero 
    # number of days over Rt takes to reach 0 from final value

    vec_Rt_sims_extension = sapply(1:length_dissipateRt, function(x) tail(Rt_sims_j,1)*(1-(x/length_dissipateRt)))
    Rt_sims_j = append(Rt_sims_j, vec_Rt_sims_extension)
    
    m_Rt_simulated[qounter_j, ] <- c(district_i, district_outbreak_i, outbreak_all_i, quantile_j, Rt_sims_j)
  }
  
  # df_Rt_simulated = as.data.frame(m_Rt_simulated)%>%
  #   mutate(simulation_id = row_number())%>%
  #   pivot_longer(-c(simulation_id), names_to = "date", values_to = "Rt")%>%
  #   mutate(date = factor(date,
  #                        levels = paste0("V",1:n_Rt_estimates),
  #                        labels = 1:n_Rt_estimates))%>%
  #   mutate(date = as.numeric(as.character(date)),
  #          simulation_id = factor(simulation_id))
  
  list_Rt_ebola_i[[outbreak_i]]["m_Rt"] <- list(m_Rt_simulated)
  
}



## Plot to check results
df_Rt_ebola_i = as.data.frame(list_Rt_ebola_i[[3]][["m_Rt"]])%>%
  mutate(quantile_factor = factor(quantile))%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile, quantile_factor), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))


ggplot(df_Rt_ebola_i, aes(x = day, y = Rt, colour = quantile_factor))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")

## Clean plot to demonstrate variable Rt curves
df_Rt_ebola_quantiles_1 = as.data.frame(list_Rt_ebola_i[[1]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_2 = as.data.frame(list_Rt_ebola_i[[2]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_3 = as.data.frame(list_Rt_ebola_i[[3]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_4 = as.data.frame(list_Rt_ebola_i[[4]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_5 = as.data.frame(list_Rt_ebola_i[[5]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_6 = as.data.frame(list_Rt_ebola_i[[6]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_7 = as.data.frame(list_Rt_ebola_i[[7]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_8 = as.data.frame(list_Rt_ebola_i[[8]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_9 = as.data.frame(list_Rt_ebola_i[[9]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))
df_Rt_ebola_quantiles_10 = as.data.frame(list_Rt_ebola_i[[10]][["m_Rt"]])%>%
  pivot_longer(-c(district, district_outbreak, outbreak_all, quantile), names_to = "day", values_to = "Rt")%>%
  mutate(day = as.numeric(day),
         Rt = as.numeric(Rt))%>%
  group_by(district, district_outbreak, outbreak_all, day)%>%
  summarise(Rt_mean = mean(Rt), Rt_min = min(Rt), Rt_max = max(Rt))

df_Rt_ebola_quantiles = rbind(df_Rt_ebola_quantiles_1,
                              df_Rt_ebola_quantiles_2,
                              df_Rt_ebola_quantiles_3,
                              df_Rt_ebola_quantiles_4,
                              df_Rt_ebola_quantiles_5,
                              df_Rt_ebola_quantiles_6,
                              df_Rt_ebola_quantiles_7,
                              df_Rt_ebola_quantiles_8,
                              df_Rt_ebola_quantiles_9,
                              df_Rt_ebola_quantiles_10)%>%
  mutate(district_outbreak_label = paste0(district, " ", district_outbreak))


p_Rt_examples = ggplot(df_Rt_ebola_quantiles, aes(x = day, y = Rt_mean, ymax = Rt_max, ymin = Rt_min))+
  geom_ribbon(fill = "lightblue")+
  geom_line(colour = "darkblue")+
  theme_light()+
  theme(legend.position = "none")+
  facet_wrap(facets = vars(district_outbreak_label), 
             ncol = 2)+
  xlab("number of days since outbreak detected")+
  ylab("effective reproduction number (Rt)")
p_Rt_examples

# ggsave(filename = "p_Rt_examples.png",
#        plot = p_Rt_examples, 
#        width = 16,
#        height = 24,
#        units = "cm")


####################
### save Rt data ###
####################

# save(list_Rt_ebola_i, file = "inputs_list_Rt_curves.Rdata")


