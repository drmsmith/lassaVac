library(tidyverse)

#################
### LOAD DATA ###
#################

# howManyRuns = 1
howManyRuns = 10

if(howManyRuns == 1){
  # filepaths
  filepath_runs = c("J:/projects/lassa_Vaccination/lassa/outcomes/1run/")
  filepath_tables = c("J:/projects/lassa_Vaccination/lassa/tables/1run/")
}
if(howManyRuns == 10){
  # filepaths
  filepath_runs = c("J:/projects/lassa_Vaccination/lassa/outcomes/10runs/")
  filepath_tables = c("J:/projects/lassa_Vaccination/lassa/tables/10runs/")
}

load(paste0(filepath_runs, "df_outputs_total_summarized.Rdata"))
load(paste0(filepath_runs, "df_outputs_averted_total_summarized.Rdata"))
load(paste0(filepath_runs, "df_outputs_total_summarized_allRegion.Rdata"))
load(paste0(filepath_runs, "df_outputs_averted_total_summarized_allRegion.Rdata"))

################################
### FORMAT AND EXPORT TABLES ###
################################

vec_outcomes_burden = c("N_cases", "N_symptoms", "N_hospital", "N_death", "YLL", 
                        "DALY_total", "DALY_total_disc", "YWL", "N_catastrophic", "N_impoverished")

vec_outcomes_costs = c("Cost_OOP", "Cost_OOP_disc", "Cost_hosp", "Cost_hosp_disc", "Cost_prod_total", "Cost_prod_total_disc",
                       "Cost_DALY_total", "Cost_DALY_total_disc", "Cost_VSL", "Cost_VSLY")

vec_outcomes_costs_noDisc = c("Cost_OOP", "Cost_hosp", "Cost_prod_total", "Cost_DALY_total", "Cost_VSL", "Cost_VSLY")

vec_outcomes_table_maintext = c("N_cases", "DALY_total_disc", "N_catastrophic", "N_impoverished",
                                "Cost_societal_disc", "Cost_DALY_total_disc", "Cost_VSL")

#########################
### BASELINE OUTCOMES ###
#########################

### WHOLE REGION ###
### Baseline outcomes without vaccination
table_outcomes_noVacc_allRegion = df_outputs_total_summarized_allRegion%>%
  filter(scenario == "no vaccine",
         vaccEff == "no vaccine")%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(outcome, prob_hosp, label)%>%
  pivot_wider(id_cols = c(prob_hosp), names_from = outcome, values_from = label)

write.csv2(table_outcomes_noVacc_allRegion, file = paste0(filepath_tables, "table_outcomes_noVacc_allRegion.csv"))

### Baseline burden without vaccination
table_burden_noVacc_allRegion = df_outputs_total_summarized_allRegion%>%
  filter(outcome %in% vec_outcomes_burden,
         scenario == "no vaccine",
         vaccEff == "no vaccine")%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_burden))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(outcome, prob_hosp, label)%>%
  pivot_wider(names_from = outcome, values_from = label)

write.csv2(table_burden_noVacc_allRegion, file = paste0(filepath_tables, "table_burden_noVacc_allRegion.csv"))

### Baseline costs without vaccination (excluding discounted costs: for Lassa-X, only a two year span so doesn't make much sense)
table_costs_noVacc_allRegion = df_outputs_total_summarized_allRegion%>%
  filter(outcome %in% vec_outcomes_costs,
         scenario == "no vaccine",
         vaccEff == "no vaccine")%>%
  mutate(discounted = case_when(grepl("disc", outcome) ~ "Y",
                                T ~ "N"))%>%
  mutate(label = paste0(prettyNum(round(mean/1000000, 1), big.mark = ",", scientific = F), 
                        "M (", 
                        prettyNum(round(q025/1000000, 1), big.mark = ",", scientific = F), 
                        "M-", 
                        prettyNum(round(q975/1000000, 1), big.mark = ",", scientific = F), 
                        "M)"),
         outcome = factor(outcome, levels = vec_outcomes_costs, labels = c(rep(vec_outcomes_costs_noDisc[1:4], each = 2), vec_outcomes_costs_noDisc[5:6])))%>%
  ungroup()%>%
  arrange(outcome, prob_hosp, discounted)%>%
  dplyr::select(prob_hosp, outcome, discounted, label)%>%
  pivot_wider(names_from = outcome, values_from = label)

write.csv2(table_costs_noVacc_allRegion, file = paste0(filepath_tables, "table_costs_noVacc_allRegion.csv"))


### BY COUNTRY ###
### Baseline burden without vaccination
# absolute
table_burden_noVacc_bycountry = df_outputs_total_summarized%>%
  filter(outcome %in% vec_outcomes_burden,
         scenario == "no vaccine",
         prob_hosp == 0.015,
         vaccEff == "no vaccine")%>%
  left_join(., df_district_names_national, by = "GID_0")%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_burden))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(COUNTRY, outcome, label)%>%
  pivot_wider(names_from = outcome, values_from = label)

write.csv2(table_burden_noVacc_bycountry, file = paste0(filepath_tables, "table_burden_noVacc_bycountry.csv"))

# rates
table_burden_noVacc_bycountry_rates = df_outputs_total_summarized%>%
  filter(outcome %in% vec_outcomes_burden,
         scenario == "no vaccine",
         prob_hosp == 0.015,
         vaccEff == "no vaccine")%>%
  left_join(., df_district_names_national, by = "GID_0")%>%
  mutate(label = paste0(prettyNum(round(mean_rate*100000,0), big.mark = ",", scientific = F),
                        " (", 
                        prettyNum(round(q025_rate*100000), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975_rate*100000), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_burden))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(COUNTRY, outcome, label)%>%
  pivot_wider(names_from = outcome, values_from = label)


write.csv2(table_burden_noVacc_bycountry_rates, file = paste0(filepath_tables, "table_burden_noVacc_bycountry_rates.csv"))


### Baseline costs without vaccination
table_costs_noVacc_bycountry = df_outputs_total_summarized%>%
  filter(outcome %in% vec_outcomes_costs,
         scenario == "no vaccine",
         prob_hosp == 0.015,
         vaccEff == "no vaccine")%>%
  left_join(., df_district_names_national, by = "GID_0")%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_costs))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(COUNTRY, outcome, label)%>%
  pivot_wider(names_from = outcome, values_from = label)

write.csv2(table_costs_noVacc_bycountry, file = paste0(filepath_tables, "table_costs_noVacc_bycountry.csv"))



########################
### OUTCOMES AVERTED ###
########################

### WHOLE REGION, BASELINE PROB HOSP (p=015) ###

### Selected outcomes (main table) averted due to vaccination across whole region, baseline prob hosp
table_outcomesAverted_mainTable_allRegion_p015 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% c(vec_outcomes_table_maintext),
         prob_hosp == 0.015,
         vaccEff == "70_70")%>%
  mutate(label = case_when(outcome %in% c("N_cases", "DALY_total_disc", "N_catastrophic", "N_impoverished") ~ 
                             paste0(prettyNum(round(mean/1000, 1), big.mark = ",", scientific = F), 
                                    "k (", 
                                    prettyNum(round(q025/1000, 1), big.mark = ",", scientific = F), 
                                    "k-", 
                                    prettyNum(round(q975/1000, 1), big.mark = ",", scientific = F), 
                                    "k)"),
                           T ~ paste0(prettyNum(round(mean/1000000, 1), big.mark = ",", scientific = F), 
                                      "M (", 
                                      prettyNum(round(q025/1000000, 1), big.mark = ",", scientific = F), 
                                      "M-", 
                                      prettyNum(round(q975/1000000, 1), big.mark = ",", scientific = F), 
                                      "M)")),
         outcome = factor(outcome, levels = c(vec_outcomes_table_maintext)))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(scenario, outcome, label)%>%
  arrange(scenario)%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  t()

write.csv2(table_outcomesAverted_mainTable_allRegion_p015, file = paste0(filepath_tables, "table_outcomesAverted_mainTable_allRegion_p015.csv"))



### All outcomes (burden and costs) averted due to vaccination across whole region, baseline prob hosp
table_outcomesAverted_allRegion_p015 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% c(vec_outcomes_burden, vec_outcomes_costs_noDisc),
         prob_hosp == 0.015)%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = c(vec_outcomes_burden, vec_outcomes_costs_noDisc)))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(scenario, vaccEff, outcome, label)%>%
  arrange(scenario, vaccEff)%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  t()

write.csv2(table_outcomesAverted_allRegion_p015, file = paste0(filepath_tables, "table_outcomesAverted_allRegion_p015.csv"))


### Burden averted due to vaccination across whole region, vary prob hosp
table_burdenAverted_allRegion_p015_p0006 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% vec_outcomes_burden)%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_burden))%>%
  ungroup()%>%
  arrange(outcome, prob_hosp)%>%
  dplyr::select(scenario, vaccEff, outcome, prob_hosp, label)%>%
  pivot_wider(names_from = c(outcome, prob_hosp), values_from = label)%>%
  t()

write.csv2(table_burdenAverted_allRegion_p015_p0006, file = paste0(filepath_tables, "table_burdenAverted_allRegion_p015_p0006.csv"))


### Costs (undiscounted) averted due to vaccination across whole region, baseline prob hosp
table_costsAverted_allRegion_p015_p0006 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% vec_outcomes_costs)%>%
  mutate(discounted = case_when(grepl("disc", outcome) ~ "Y",
                                T ~ "N"))%>%
  mutate(label = paste0(prettyNum(round(mean/1000000,1), big.mark = ",", scientific = F), 
                        "M (", 
                        prettyNum(round(q025/1000000,1), big.mark = ",", scientific = F), 
                        "M-", 
                        prettyNum(round(q975/1000000,1), big.mark = ",", scientific = F), 
                        "M)"),
         outcome = factor(outcome, levels = vec_outcomes_costs, labels = c(rep(vec_outcomes_costs_noDisc[1:4], each = 2), vec_outcomes_costs_noDisc[5:6])))%>%
  ungroup()%>%
  arrange(scenario, outcome, prob_hosp, discounted)%>%
  dplyr::select(scenario, vaccEff, outcome, prob_hosp, discounted, label)%>%
  pivot_wider(names_from = c(scenario, vaccEff), values_from = label)

write.csv2(table_costsAverted_allRegion_p015_p0006, file = paste0(filepath_tables, "table_costsAverted_allRegion_p015_p0006.csv"))



### WHOLE REGION, ALTERNATIVE PROB HOSPs (p=0006) ###

### All outcomes (burden and costs) averted due to vaccination across whole region, baseline prob hosp
table_outcomesAverted_allRegion_p0006 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% c(vec_outcomes_burden, vec_outcomes_costs_noDisc),
         prob_hosp == 0.0006)%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = c(vec_outcomes_burden, vec_outcomes_costs_noDisc)))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(scenario, vaccEff, outcome, label)%>%
  arrange(scenario, vaccEff)%>%
  pivot_wider(names_from = outcome, values_from = label)%>%
  t()

write.csv2(table_outcomesAverted_allRegion_p0006, file = paste0(filepath_tables, "table_outcomesAverted_allRegion_p0006.csv"))


### Burden averted due to vaccination across whole region, baseline prob hosp
table_burdenAverted_allRegion_p0006 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% vec_outcomes_burden,
         prob_hosp %in% c(0.0006))%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_burden))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(scenario, vaccEff, outcome, prob_hosp, label)%>%
  arrange(scenario, vaccEff, outcome, prob_hosp)%>%
  pivot_wider(names_from = c(outcome,prob_hosp), values_from = label)%>%
  t()

write.csv2(table_burdenAverted_allRegion_p0006, file = paste0(filepath_tables, "table_burdenAverted_allRegion_p0006.csv"))


### Costs averted due to vaccination across whole region, baseline prob hosp
table_costsAverted_allRegion_p0006 = df_outputs_averted_total_summarized_allRegion%>%
  filter(outcome %in% vec_outcomes_costs,
         prob_hosp %in% c(0.0006))%>%
  mutate(label = paste0(prettyNum(round(mean,0), big.mark = ",", scientific = F), 
                        " (", 
                        prettyNum(round(q025), big.mark = ",", scientific = F), 
                        "-", 
                        prettyNum(round(q975), big.mark = ",", scientific = F), 
                        ")"),
         outcome = factor(outcome, levels = vec_outcomes_costs))%>%
  ungroup()%>%
  arrange(outcome)%>%
  dplyr::select(scenario, vaccEff, outcome, prob_hosp, label)%>%
  arrange(scenario, vaccEff, outcome, prob_hosp)%>%
  pivot_wider(names_from = c(outcome,prob_hosp), values_from = label)%>%
  t()

write.csv2(table_costsAverted_allRegion_p0006, file = paste0(filepath_tables, "table_costsAverted_allRegion_p0006.csv"))
