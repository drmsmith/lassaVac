f_lassa_econOutcomes = function(vec_GID_0, # vector of countries
                                vec_simulations,
                                vec_years, 
                                vec_runs, 
                                vec_prob_hosp,
                                vec_scenarios,
                                vec_vaccEff){
  
  ### initiate qounters for loops
  qounter_unaverted = 0
  qounter_averted = 0
  
  ### empty matrices to store unaverted and averted outcomes
  # for each unaverted outcome, there are n_scenarios * n_vaccEff averted outcomes
  
  m_colnames = c("outcome", "GID_0", "simulation", "realyear", "run", "prob_hosp", "scenario", "vaccEff", 
                 "val_prob_hosp",
                 "N_cases", "N_symptoms",  "N_hospital","N_death", "N_sequelae", "N_catastrophic", "N_impoverished",
                 "N_W_cases", "N_W_symptoms",  "N_W_hospital","N_W_death", "N_W_sequelae", "N_W_catastrophic", "N_W_impoverished",
                 "YLL", "YLL_disc", "YWL", "YWL_disc", "YLS", "YLS_disc"
  )
  
  nrows_unaverted = length(vec_GID_0) * length(vec_simulations) * length(vec_years) * length(vec_runs) * length(vec_prob_hosp)
  nrows_averted = length(vec_GID_0) * length(vec_simulations) * length(vec_years) * length(vec_runs) * length(vec_prob_hosp) * length(vec_scenarios) * length(vec_vaccEff)
  
  m_outputs = matrix(NA, 
                     nrow = nrows_unaverted,
                     ncol = length(m_colnames))
  
  m_outputs_averted = matrix(NA, 
                             nrow = nrows_averted,
                             ncol = length(m_colnames))
  
  ### LOOP: country_i
  for(GID_0_i in vec_GID_0){
    
    ### load country-specific data
    
    # lassa transmission simulation outputs
    df_simulationOutputsFinal_Deltas_i = df_simulationOutputsFinal_Deltas%>%
      filter(GID_0 == GID_0_i)
    
    # age pyramids
    df_age_distr_i = df_age_distr%>%filter(GID_0 == GID_0_i)
    
    # probabilities of catastrophic healthcare expenditure and impoverishment
    df_catastrophe_i = df_catastrophe%>%filter(GID_0 == GID_0_i)
    prob_catastrophic_i = df_catastrophe_i$prop_at_risk_catastrp_HCexp/100
    prob_impoverishment_i = df_catastrophe_i$prop_at_risk_impoverishing/100
    
    # proportion of population working
    df_GNI_prop_working_i = df_GNI_prop_working%>%filter(GID_0 == GID_0_i)
    prop_working_i = df_GNI_prop_working_i$prop_working
    
    ### LOOP: simulation_j
    for(simulation_j in vec_simulations){
      
      ### LOOP: year_k
      for(year_k in vec_years){
        
        ### Load Lassa simulation data specific to this simulation and year
        df_simulationOutputsFinal_Deltas_i_j_k_l = df_simulationOutputsFinal_Deltas_i%>%
          filter(simulation == simulation_j,
                 realyear == year_k)
        
        ### Select "dummy scenario" to extract unpruned outcomes (identical across all scenarios and vaccEff)
        df_simulationOutputsFinal_Deltas_i_j_k_l_dummyScenario = df_simulationOutputsFinal_Deltas_i_j_k_l%>%
          filter(scenario == 1,
                 vaccEff == "70")
        
        ### Number of cases (total) = number of cases (unvaccinated) + number of cases (vaccinated)
        N_cases_U = round(df_simulationOutputsFinal_Deltas_i_j_k_l_dummyScenario$IncCumul_U_noVacc)
        N_cases_V = round(df_simulationOutputsFinal_Deltas_i_j_k_l_dummyScenario$IncCumul_V_noVacc)
        N_cases = N_cases_U + N_cases_V
        if(N_cases < 0){warning("negative cases"); break}
        
        ### LOOP: run_l
        # introduces stochasticity across runs
        for(run_l in vec_runs){
          
          ### Randomly draw ages of all cases according to country-specific age pyramids
          df_cases = sample(x = df_age_distr_i$age_numeric,
                            size = N_cases,
                            prob = df_age_distr_i$age_proportion,
                            replace = T)%>%
            table(., dnn = list("age_numeric"))%>%
            as.data.frame(responseName = "N_cases")%>%
            mutate(age_numeric = as.numeric(as.character(age_numeric)))%>%
            # join to age pyramids to ensure all ages included (even if no cases in certain age bands)
            full_join(., df_age_distr_i%>%ungroup()%>%
                        dplyr::select(age_numeric, age_proportion),
                      by = "age_numeric")%>%
            # replace missing cases with 0 cases
            replace_na(list(N_cases = 0))%>%
            mutate(GID_0 = GID_0_i)%>%
            arrange(age_numeric)%>%
            # merge age distribution to later calculate YLL and YWL
            left_join(., df_age_distr_i%>%dplyr::select("GID_0", "age_numeric", "life_exp_at_age_x", "years_left_to_work"),
                      by = c("GID_0", "age_numeric"))
          
          ### Isolate data for later use
          # vector of cases across age groups
          vec_N_cases = df_cases$N_cases
          # number of age groups
          n_agegroups = length(df_cases$age_numeric)
          
          ### LOOP: prob_hosp_m
          for(prob_hosp_m in vec_prob_hosp){
            
            qounter_unaverted = qounter_unaverted + 1
            
            ### WORK IN PROGRESS ###
            ### IMPLEMENT PARAMETER UNCERTAINTY ###
            # val_prob_hosp = rnorm(1, mean = prob_hosp_m, sd = prob_hosp_m/10)
            ### for now the "uncertain" parameter is just the input parameter
            val_prob_hosp = prob_hosp_m
            
            print(paste0("for country ", GID_0_i, 
                         ", on simulation ", simulation_j, " of ", length(vec_simulations), 
                         ", on year ", year_k, " of ", length(vec_years),
                         ", on run ", run_l, " of ", length(vec_runs),
                         " and on prob_hosp ", prob_hosp_m, " of ", length(vec_prob_hosp)))
            
            ### probability of symptoms (assumes prob_hosp_given_symptoms is stable)
            prob_symptoms = as.numeric(val_prob_hosp)/prob_hosp_given_symptoms
            
            ### Based on corresponding probabilities, randomly draw numbers of individuals per age group having each outcome
            vec_N_symptoms = rbinom(n_agegroups, size = vec_N_cases, prob = prob_symptoms)
            vec_N_hospital = rbinom(n_agegroups, size = vec_N_symptoms, prob = prob_hosp_given_symptoms)
            vec_N_death = rbinom(n_agegroups, size = vec_N_hospital, prob = prob_death)
            vec_N_sequelae = rbinom(n_agegroups, size = vec_N_hospital - vec_N_death, prob = prob_sequelae)
            vec_N_catastrophic = rbinom(n_agegroups, size = vec_N_hospital, prob = prob_catastrophic_i)
            vec_N_impoverished = rbinom(n_agegroups, size = vec_N_hospital, prob = prob_impoverishment_i)
            
            ### For outcomes specifically in the working population, multiply by the proportion working
            vec_N_W_cases = c(rep(0,15), vec_N_cases[16:65], rep(0,36))*prop_working_i
            vec_N_W_symptoms = c(rep(0,15), vec_N_symptoms[16:65], rep(0,36))*prop_working_i
            vec_N_W_hospital = c(rep(0,15), vec_N_hospital[16:65], rep(0,36))*prop_working_i
            vec_N_W_death = c(rep(0,15), vec_N_death[16:65], rep(0,36))*prop_working_i
            vec_N_W_sequelae = c(rep(0,15), vec_N_sequelae[16:65], rep(0,36))*prop_working_i
            vec_N_W_catastrophic = c(rep(0,15), vec_N_catastrophic[16:65], rep(0,36))*prop_working_i
            vec_N_W_impoverished = c(rep(0,15), vec_N_impoverished[16:65], rep(0,36))*prop_working_i
            
            ### Update output dataframe with these outcomes
            df_cases$N_symptoms = vec_N_symptoms
            df_cases$N_hospital = vec_N_hospital
            df_cases$N_death = vec_N_death
            df_cases$N_sequelae = vec_N_sequelae
            df_cases$N_catastrophic = vec_N_catastrophic
            df_cases$N_impoverished = vec_N_impoverished
            
            df_cases$N_W_cases = vec_N_W_cases
            df_cases$N_W_symptoms = vec_N_W_symptoms
            df_cases$N_W_hospital = vec_N_W_hospital
            df_cases$N_W_death = vec_N_W_death
            df_cases$N_W_sequelae = vec_N_W_sequelae
            df_cases$N_W_catastrophic = vec_N_W_catastrophic
            df_cases$N_W_impoverished = vec_N_W_impoverished
            
            ### Calculate relevant outcomes over person-time (YLL, YWL, YLS)
            df_outcomes_unaverted = df_cases%>%
              # both undiscounted and discounted values (ref for continuous discount rate: https://resource-allocation.biomedcentral.com/articles/10.1186/1478-7547-11-18)
              mutate(YLL = N_death*life_exp_at_age_x,
                     YLL_disc = N_death*((1/discRate)*(1-exp(-discRate*life_exp_at_age_x))),
                     YWL = N_W_death*years_left_to_work,
                     YWL_disc = N_W_death*((1/discRate)*(1-exp(-discRate*years_left_to_work))),
                     YLS = N_sequelae*life_exp_at_age_x,
                     YLS_disc = N_sequelae*(1/discRate)*(1-exp(-discRate*life_exp_at_age_x)))
            
            ### Fill final matrix with outputs
            m_outputs[qounter_unaverted,] = c("outcome" = "baseline",
                                              "GID_0" = GID_0_i,
                                              "simulation" = simulation_j,
                                              "realyear" = year_k,
                                              "run" = run_l,
                                              "prob_hosp" = prob_hosp_m,
                                              "scenario" = "no vaccine",
                                              "vaccEff" = "no vaccine",
                                              "val_prob_hosp" = val_prob_hosp,
                                              "N_cases" = N_cases,
                                              "N_symptoms" = sum(df_outcomes_unaverted$N_symptoms),
                                              "N_hospital" = sum(df_outcomes_unaverted$N_hospital),
                                              "N_death" = sum(df_outcomes_unaverted$N_death),
                                              "N_sequelae" = sum(df_outcomes_unaverted$N_sequelae),
                                              "N_catastrophic" = sum(df_outcomes_unaverted$N_catastrophic),
                                              "N_impoverished" = sum(df_outcomes_unaverted$N_impoverished),
                                              "N_W_cases" = sum(df_outcomes_unaverted$N_W_cases),
                                              "N_W_symptoms" = sum(df_outcomes_unaverted$N_W_symptoms),
                                              "N_W_hospital" = sum(df_outcomes_unaverted$N_W_hospital),
                                              "N_W_death" = sum(df_outcomes_unaverted$N_W_death),
                                              "N_W_sequelae" = sum(df_outcomes_unaverted$N_W_sequelae),
                                              "N_W_catastrophic" = sum(df_outcomes_unaverted$N_W_catastrophic),
                                              "N_W_impoverished" = sum(df_outcomes_unaverted$N_W_impoverished),
                                              "YLL" = sum(df_outcomes_unaverted$YLL),
                                              "YLL_disc" = sum(df_outcomes_unaverted$YLL_disc),
                                              "YWL" = sum(df_outcomes_unaverted$YWL),
                                              "YWL_disc" = sum(df_outcomes_unaverted$YWL_disc),
                                              "YLS" = sum(df_outcomes_unaverted$YLS),
                                              "YLS_disc" = sum(df_outcomes_unaverted$YLS_disc))
            
            ### LOOP: vaccine scenarios
            for(scenario_n in vec_scenarios){
              # scenario_n = "5" ## test case
              
              ### LOOP: vaccine efficacies
              for(vaccEff_o_p in vec_vaccEff){
                # vaccEff_o_p = "0_70" ## test case
                
                ### update qounter for outcomes averted matrix
                qounter_averted = qounter_averted + 1
                
                ### extract VE for infection and disease
                vaccEff_infection = as.numeric(sub("_.*", "", vaccEff_o_p))
                vaccEff_disease = as.numeric(sub(".*_", "", vaccEff_o_p))
                
                ### filter final corresponding cases for this vaccine scenario
                df_simulationOutputsFinal_Deltas_i_j_k_l_m_n_o = df_simulationOutputsFinal_Deltas_i_j_k_l%>%
                  filter(scenario == scenario_n,
                         vaccEff == vaccEff_infection)
                
                ### How many cases remaining after pruning?
                N_cases_pruned_U = round(df_simulationOutputsFinal_Deltas_i_j_k_l_m_n_o$IncCumul_U_final)
                N_cases_pruned_V = round(df_simulationOutputsFinal_Deltas_i_j_k_l_m_n_o$IncCumul_V_final)
                
                ### Total number of cases pruned
                N_cases_pruned = N_cases_pruned_U + N_cases_pruned_V
                if(N_cases_pruned < 0){warning("negative cases"); break}
                
                ### Total number of cases averted = total cases minus total cases pruned
                N_cases_averted = N_cases - N_cases_pruned
                if(N_cases_averted < 0){warning("negative cases"); break}
                
                ### Calculate number of cases averted due to disease-prevention
                N_cases_averted_diseaseVacc = round(N_cases_pruned_V*(vaccEff_disease/100))
                
                ### Calculation proportions of cases averted due to infection blocking vs disease + infection blocking
                prop_cases_averted_infection = N_cases_averted/N_cases
                prop_cases_averted_infection_disease = (N_cases_averted + N_cases_averted_diseaseVacc)/N_cases
                
                ### Create DF for averted cases, where unaverted outcomes are multipled by the outcomes averted
                df_cases_averted = df_cases
                
                df_cases_averted$N_cases = vec_N_cases*prop_cases_averted_infection
                df_cases_averted$N_symptoms = vec_N_symptoms*prop_cases_averted_infection_disease
                df_cases_averted$N_hospital = vec_N_hospital*prop_cases_averted_infection_disease
                df_cases_averted$N_death = vec_N_death*prop_cases_averted_infection_disease
                df_cases_averted$N_sequelae = vec_N_sequelae*prop_cases_averted_infection_disease
                df_cases_averted$N_catastrophic = vec_N_catastrophic*prop_cases_averted_infection_disease
                df_cases_averted$N_impoverished = vec_N_impoverished*prop_cases_averted_infection_disease
                
                df_cases_averted$N_W_cases = vec_N_W_cases*prop_cases_averted_infection
                df_cases_averted$N_W_symptoms = vec_N_W_symptoms*prop_cases_averted_infection_disease
                df_cases_averted$N_W_hospital = vec_N_W_hospital*prop_cases_averted_infection_disease
                df_cases_averted$N_W_death = vec_N_W_death*prop_cases_averted_infection_disease
                df_cases_averted$N_W_sequelae = vec_N_W_sequelae*prop_cases_averted_infection_disease
                df_cases_averted$N_W_catastrophic = vec_N_W_catastrophic*prop_cases_averted_infection_disease
                df_cases_averted$N_W_impoverished = vec_N_W_impoverished*prop_cases_averted_infection_disease
                
                ### Calculate relevant outcomes AVERTED over person-time (YLL, YWL, YLS)
                df_outcomes_averted = df_cases_averted%>%
                  # both undiscounted and discounted values (ref for continuous discount rate: https://resource-allocation.biomedcentral.com/articles/10.1186/1478-7547-11-18)
                  mutate(YLL = N_death*life_exp_at_age_x,
                         YLL_disc = N_death*((1/discRate)*(1-exp(-discRate*life_exp_at_age_x))),
                         YWL = N_W_death*years_left_to_work,
                         YWL_disc = N_W_death*((1/discRate)*(1-exp(-discRate*years_left_to_work))),
                         YLS = N_sequelae*life_exp_at_age_x,
                         YLS_disc = N_sequelae*(1/discRate)*(1-exp(-discRate*life_exp_at_age_x)))
                
                ### final vector of outputs AVERTED
                m_outputs_averted[qounter_averted,] = c("outcome" = "averted",
                                                        "GID_0" = GID_0_i,
                                                        "simulation" = simulation_j,
                                                        "realyear" = year_k,
                                                        "run" = run_l,
                                                        "prob_hosp" = prob_hosp_m,
                                                        "scenario" = scenario_n,
                                                        "vaccEff" = vaccEff_o_p,
                                                        "val_prob_hosp" = val_prob_hosp,
                                                        "N_cases" = N_cases_averted,
                                                        "N_symptoms" = sum(df_outcomes_averted$N_symptoms),
                                                        "N_hospital" = sum(df_outcomes_averted$N_hospital),
                                                        "N_death" = sum(df_outcomes_averted$N_death),
                                                        "N_sequelae" = sum(df_outcomes_averted$N_sequelae),
                                                        "N_catastrophic" = sum(df_outcomes_averted$N_catastrophic),
                                                        "N_impoverished" = sum(df_outcomes_averted$N_catastrophic),
                                                        "N_W_cases" = sum(df_outcomes_averted$N_W_cases),
                                                        "N_W_symptoms" = sum(df_outcomes_averted$N_W_symptoms),
                                                        "N_W_hospital" = sum(df_outcomes_averted$N_W_hospital),
                                                        "N_W_death" = sum(df_outcomes_averted$N_W_death),
                                                        "N_W_sequelae" = sum(df_outcomes_averted$N_W_sequelae),
                                                        "N_W_catastrophic" = sum(df_outcomes_averted$N_W_catastrophic),
                                                        "N_W_impoverished" = sum(df_outcomes_averted$N_W_catastrophic),
                                                        "YLL" = sum(df_outcomes_averted$YLL),
                                                        "YLL_disc" = sum(df_outcomes_averted$YLL_disc),
                                                        "YWL" = sum(df_outcomes_averted$YWL),
                                                        "YWL_disc" = sum(df_outcomes_averted$YWL_disc),
                                                        "YLS" = sum(df_outcomes_averted$YLS),
                                                        "YLS_disc" = sum(df_outcomes_averted$YLS_disc))
              }
            }
          }
        }
      }
    }
  }
  
  m_econOutcomes = rbind(m_outputs,
                         m_outputs_averted)
  
  colnames(m_econOutcomes) = m_colnames
  
  return(m_econOutcomes)
}
