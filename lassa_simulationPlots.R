library(scales)

#################
### LOAD DATA ###
#################

# by district
df_Lassa_csv_70_byDistrict_daily_quantiles_selectDistricts = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_csv_70_byDistrict_daily_quantiles_selectDistricts.Rdata")
df_Lassa_byDistrict_annual = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_byDistrict_annual.Rdata")
df_Lassa_byDistrict_total = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_byDistrict_total.Rdata")
df_Lassa_byDistrict_total_quantiles = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_byDistrict_total_quantiles.Rdata")

# by country
df_Lassa_byCountry_annual = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_byCountry_annual.Rdata")
df_Lassa_byCountry_total = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_byCountry_total.Rdata")
df_Lassa_byCountry_total_quantiles = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_byCountry_total_quantiles.Rdata")

# whole region
df_Lassa_allRegion_daily = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_allRegion_daily.Rdata")
df_Lassa_allRegion_daily_quantiles = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_allRegion_daily_quantiles.Rdata")
df_Lassa_allRegion_annual = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_allRegion_annual.Rdata")
df_Lassa_allRegion_total = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_allRegion_total.Rdata")
df_Lassa_allRegion_total_quantiles = loadRData("J:/projects/lassa_vaccination/lassa/simulations/outputs_clean/df_Lassa_allRegion_total_quantiles.Rdata")

#############
### PLOTS ###
#############


### daily cases averted in 3 districts
p_lassa_cases_averted_daily_selectDistrictsScenarios = ggplot(df_Lassa_csv_70_byDistrict_daily_quantiles_selectDistricts%>%
                                                                filter(time %in% 0:(365*3),
                                                                       scenario %in% c(1,3,5,7))%>%
                                                                mutate(scenario = factor(scenario,
                                                                                         levels = c(1,3,5,7),
                                                                                         labels = c("Scenario 1",
                                                                                                    "Scenario 3",
                                                                                                    "Scenario 5",
                                                                                                    "Scenario 7"))),
                                                              aes(x = time/365, y = mean, ymin = min, ymax = max, colour = catchmentID_label, fill = catchmentID_label))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_line(stat = "identity")+
  geom_ribbon(alpha = 0.3, colour = NA)+
  theme_bw()+
  ylab("Daily infections averted")+
  xlab("Year")+
  facet_wrap(facets = vars(scenario), ncol = 1)+
  scale_fill_manual(values = c("#1b9e77", "#7570b3", "#e7298a"))+
  scale_colour_manual(values = c("#1b9e77", "#7570b3", "#e7298a"))+
  scale_x_continuous(breaks = seq(0,10,1))+
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_blank())
p_lassa_cases_averted_daily_selectDistrictsScenarios


### cumulative cases averted in 4 vs 6 countries
p_lassa_cases_averted_byCountry_4 = ggplot(df_Lassa_byCountry_total_quantiles%>%
                                             filter(country %in% c("Guinea", "Liberia", "Nigeria", "Sierra Leone"),
                                                    outcome == "averted",
                                                    VE == 70)%>%
                                             mutate(scenario_label = factor(scenario,
                                                                            levels = 1:7,
                                                                            labels = c("Scenario 1", 
                                                                                       "Scenario 2",
                                                                                       "Scenario 3", 
                                                                                       "Scenario 4",
                                                                                       "Scenario 5", 
                                                                                       "Scenario 6", 
                                                                                       "Scenario 7"))),
                                           aes(x = scenario, y = mean, ymin = min, ymax = max, fill = scenario_label))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.3)+
  geom_errorbar(width = 0.35, linewidth = 0.3)+
  facet_wrap(facets = vars(country),
             ncol = 2)+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  ylab("Cumulative infections averted")+
  xlab("Vaccination scenario")+
  scale_fill_manual(values = pal_lancet()(7))+
  scale_colour_manual(values = pal_lancet()(7))+
  scale_x_continuous(breaks = 1:7)+
  scale_shape_manual(values = c(1, 2))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
p_lassa_cases_averted_byCountry_4

p_lassa_cases_averted_byCountry_6 = ggplot(df_Lassa_byCountry_total_quantiles%>%
                                           filter(country %in% c("Guinea", "Liberia", "Mali", "Niger", "Nigeria", "Sierra Leone"),
                                                  outcome == "averted",
                                                  VE == 70)%>%
                                           mutate(scenario_label = factor(scenario,
                                                                          levels = 1:7,
                                                                          labels = c("Scenario 1", 
                                                                                     "Scenario 2",
                                                                                     "Scenario 3", 
                                                                                     "Scenario 4",
                                                                                     "Scenario 5", 
                                                                                     "Scenario 6", 
                                                                                     "Scenario 7"))),
                                         aes(x = scenario, y = mean, ymin = min, ymax = max, fill = scenario_label))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.3)+
  geom_errorbar(width = 0.35, linewidth = 0.3)+
  facet_wrap(facets = vars(country),
             ncol = 2)+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  ylab("Cumulative infections averted")+
  xlab("Vaccination scenario")+
  scale_fill_manual(values = pal_lancet()(7))+
  scale_colour_manual(values = pal_lancet()(7))+
  scale_x_continuous(breaks = 1:7)+
  scale_shape_manual(values = c(1, 2))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
p_lassa_cases_averted_byCountry_6


### cumulative cases averted (/100,000) in 4 vs 6 countries
p_lassa_cases_averted_byCountry_rate_4 = ggplot(df_Lassa_byCountry_total_quantiles%>%
                                           filter(country %in% c("Guinea", "Liberia", "Nigeria", "Sierra Leone"),
                                                  outcome == "averted",
                                                  VE == 70)%>%
                                           mutate(scenario_label = factor(scenario,
                                                                          levels = 1:7,
                                                                          labels = c("Scenario 1", 
                                                                                     "Scenario 2",
                                                                                     "Scenario 3", 
                                                                                     "Scenario 4",
                                                                                     "Scenario 5", 
                                                                                     "Scenario 6", 
                                                                                     "Scenario 7"))),
                                         aes(x = scenario, y = mean_rate*100000, ymin = min_rate*100000, ymax = max_rate*100000, fill = scenario_label))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.3)+
  geom_errorbar(width = 0.35, linewidth = 0.3)+
  facet_wrap(facets = vars(country),
             ncol = 2)+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  ylab("Cumulative infections averted (/100,000 population)")+
  xlab("Vaccination scenario")+
  scale_fill_manual(values = pal_lancet()(7))+
  scale_colour_manual(values = pal_lancet()(7))+
  scale_x_continuous(breaks = 1:7)+
  scale_shape_manual(values = c(1, 2))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
p_lassa_cases_averted_byCountry_rate_4

p_lassa_cases_averted_byCountry_rate_6 = ggplot(df_Lassa_byCountry_total_quantiles%>%
                                                  filter(country %in% c("Guinea", "Liberia", "Mali", "Niger", "Nigeria", "Sierra Leone"),
                                                         outcome == "averted",
                                                         VE == 70)%>%
                                                  mutate(scenario_label = factor(scenario,
                                                                                 levels = 1:7,
                                                                                 labels = c("Scenario 1", 
                                                                                            "Scenario 2",
                                                                                            "Scenario 3", 
                                                                                            "Scenario 4",
                                                                                            "Scenario 5", 
                                                                                            "Scenario 6", 
                                                                                            "Scenario 7"))),
                                                aes(x = scenario, y = mean_rate*100000, ymin = min_rate*100000, ymax = max_rate*100000, fill = scenario_label))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_bar(stat = "identity", colour = "black", linewidth = 0.3)+
  geom_errorbar(width = 0.35, linewidth = 0.3)+
  facet_wrap(facets = vars(country),
             ncol = 2)+
  scale_y_continuous(labels = comma)+
  theme_bw()+
  ylab("Cumulative infections averted (/100,000 population)")+
  xlab("Vaccination scenario")+
  scale_fill_manual(values = pal_lancet()(7))+
  scale_colour_manual(values = pal_lancet()(7))+
  scale_x_continuous(breaks = 1:7)+
  scale_shape_manual(values = c(1, 2))+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
p_lassa_cases_averted_byCountry_rate_6




### daily cases averted over the whole region
p_lassa_cases_averted_allRegion_daily = ggplot(df_Lassa_70, allRegion_daily_quantiles%>%
         filter(outcome == "averted",
                VE != 0)%>%
         mutate(VE = factor(VE,
                            levels = c(70, 90),
                            labels = c("70% vaccine efficacy", "90% vaccine efficacy")),
                `Vaccination scenario (coverage %)` = factor(scenario,
                                  levels = c(1:7),
                                  labels = c("\n1. Outbreak response only\n",
                                             "\n2. High-risk groups in endemic districts (80%)\n",
                                             "\n3. Endemic districts (80%)\n",
                                             "4. Endemic districts (80%) +\nnon-endemic districts of endemic countries (5%)\n",
                                             "5. Endemic districts (80%) +\nnon-endemic districts of all countries (5%)\n",
                                             "6. Endemic districts (constrained to 55%) +\nnon-endemic districts of endemic countries (5%)\n",
                                             "7. Endemic districts (constrained to 32.5%) +\nnon-endemic districts of all countries (5%)\n")
                                  )
                ), aes(x = time/365, y = mean, ymin = min, ymax = max, fill = `Vaccination scenario (coverage %)`, colour = `Vaccination scenario (coverage %)`))+
  geom_hline(yintercept = 0, alpha = 0.5)+
  geom_line(linewidth = 0.5)+
  geom_ribbon(alpha = 0.5, colour = NA)+
  theme_bw()+
  ylab("Cumulative infections averted")+
  xlab("Year")+
  scale_y_continuous(label = comma, breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000))+
  scale_x_continuous(breaks = seq(0,10, by = 1))+
  facet_wrap(facets = vars(VE), ncol = 2)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual("Vaccination scenario (coverage %)\n[all scenarios include outbreak response]",
                    values = pal_lancet()(7))+
  scale_colour_manual("Vaccination scenario (coverage %)\n[all scenarios include outbreak response]",
                      values = pal_lancet()(7))
p_lassa_cases_averted_allRegion_daily



p_fig2 = plot_grid(p_lassa_cases_averted_allRegion_daily,
          plot_grid(plot_grid(p_lassa_cases_averted_byCountry_6,
                              p_lassa_cases_averted_byCountry_rate_6,
                              align = "h", axis = "tb", ncol = 2, labels = c("B", "C")),
                    p_lassa_cases_averted_daily_selectDistrictsScenarios,
                    ncol = 2,
                    labels = c("", "D"), rel_widths = c(1,0.7)),
          nrow = 2, rel_heights = c(0.6,1),
          labels = c("A", ""))
p_fig2

ggsave(p_fig2, file = "J:/projects/lassa_vaccination/lassa/plots/Fig2.png",
       width = 27,
       height = 24, unit = "cm", bg = "white")

ggsave(p_fig2, file = "J:/projects/lassa_vaccination/lassa/plots/Fig2.pdf",
       width = 27,
       height = 24, unit = "cm", bg = "white")


p_fig2_alt = plot_grid(p_lassa_cases_averted_allRegion_daily,
                   plot_grid(plot_grid(p_lassa_cases_averted_byCountry_4,
                                       p_lassa_cases_averted_byCountry_rate_4,
                                       align = "h", axis = "tb", ncol = 2, labels = c("B", "C")),
                             p_lassa_cases_averted_daily_selectDistrictsScenarios,
                             ncol = 2,
                             labels = c("", "D"), rel_widths = c(1,0.7)),
                   nrow = 2, rel_heights = c(0.6,1),
                   labels = c("A", ""))
p_fig2_alt

ggsave(p_fig2_alt, file = "J:/projects/lassa_vaccination/lassa/plots/Fig2_alt.png",
       width = 27,
       height = 24, unit = "cm", bg = "white")

ggsave(p_fig2_alt, file = "J:/projects/lassa_vaccination/lassa/plots/Fig2_alt.pdf",
       width = 27,
       height = 24, unit = "cm", bg = "white")

# plot_grid(p_lassa_cases_averted_allRegion_daily,
#           plot_grid(p_lassa_cases_averted_byCountry,
#                     p_lassa_cases_averted_byCountry_rate,
#                     align = "h", axis = "tb", ncol = 2),
#           p_lassa_cases_averted_daily_selectDistrictsScenarios,
#           nrow = 2, rel_heights = c(0.6,1),
#           labels = c("A", ""))
