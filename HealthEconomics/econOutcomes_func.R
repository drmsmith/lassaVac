########################
### LASSA-X OR LASSA ###
########################
# Function to calculate health-economic outcomes by post-processing Lassa / Lassa-X simulations

f_econOutcomes = function(vec_GID_0, # vector of countries
                          vec_simulations, # vector of simulations
                          vec_years, 
                          vec_runs,
                          vec_prob_seq,
                          vec_scenarios,
                          vec_vaccEff,
                          df_params){
  
  ### initiate qounters for loops
  qounter_unaverted = 0
  qounter_averted = 0
  
  ### empty matrices to store unaverted and averted outcomes
  # for each unaverted outcome, there are n_scenarios * n_vaccEff averted outcomes
  
  m_colnames = c("outcome", "GID_0", "simulation", "realyear", "run", "prob_seq", "scenario", "vaccEff", 
                 "N_cases", "N_symptoms",  "N_hospital","N_death", "N_sequelae", "N_catastrophic", "N_impoverished",
                 "N_W_cases", "N_W_symptoms",  "N_W_hospital","N_W_death", "N_W_sequelae", "N_W_catastrophic", "N_W_impoverished",
                 "YLL", "YLL_disc", "YWL", "YWL_disc", "YLS", "YLS_disc",
                 "DALY_fever", "DALY_hospital", "DALY_sequelae", "DALY_sequelae_disc", "DALY_death", "DALY_death_disc",
                 "DALY_total", "DALY_total_disc",
                 "Cost_VSL", "Cost_VSLY", "Cost_DALY_total",
                 "Cost_outpatient_gvt", "Cost_outpatient_oop", "Cost_hosp_gvt", "Cost_hosp_oop", "Cost_care_oop", "Cost_care_gvt",
                 "Cost_prod_fever", "Cost_prod_hospital", "Cost_prod_death", "Cost_prod_death_disc", "Cost_prod_total",
                 "Cost_DALY_total_disc", "Cost_care_gvt_disc", "Cost_care_oop_disc", "Cost_prod_total_disc",
                 "Cost_societal", "Cost_societal_disc")
  
  nrows_unaverted = length(vec_GID_0) * length(vec_simulations) * length(vec_years) * length(vec_prob_seq) * length(vec_runs)
  nrows_averted = length(vec_GID_0) * length(vec_simulations) * length(vec_years) * length(vec_prob_seq) * length(vec_runs) * length(vec_scenarios) * length(vec_vaccEff)
  
  m_outputs = matrix(NA, 
                     nrow = nrows_unaverted,
                     ncol = length(m_colnames))
  
  m_outputs_averted = matrix(NA, 
                             nrow = nrows_averted,
                             ncol = length(m_colnames))
  
  ### LOOP: country_i
  for(GID_0_i in vec_GID_0){
    
    ### filter country-specific data and load associated parameters
    
    # lassa transmission simulation outputs
    df_simulationOutputsFinal_Deltas_i = df_simulationOutputsFinal_Deltas%>%
      filter(GID_0 == GID_0_i)
    
    # age pyramids and averages
    df_age_distr_i = df_age_distr%>%filter(GID_0 == GID_0_i)
    df_country_avg_age_i = df_country_avg_age%>%filter(GID_0 == GID_0_i)
    df_age_distr_avg_age_i = df_age_distr_i%>%filter(age_numeric == df_country_avg_age_i$avg_age)
    avg_years_left_to_live_i = df_age_distr_avg_age_i$life_exp_at_age_x
    
    # probabilities of catastrophic healthcare expenditure and impoverishment
    df_catastrophe_i = df_catastrophe%>%filter(GID_0 == GID_0_i)
    prob_catastrophic_i = df_catastrophe_i$prop_at_risk_catastrp_HCexp/100
    prob_impoverishment_i = df_catastrophe_i$prop_at_risk_impoverishing/100
    
    # proportion of population working, gross national income
    df_GNI_prop_working_i = df_GNI_prop_working%>%filter(GID_0 == GID_0_i)
    prop_working_i = df_GNI_prop_working_i$prop_working
    cost_GNI_i = df_GNI_prop_working_i$GNI
    cost_GNI_daily_i = df_GNI_prop_working_i$GNI_daily
    
    # DALY value
    df_daly_i = df_daly%>%filter(GID_0 == GID_0_i)
    cost_daly_i = df_daly_i$daly_value
    
    # VSL / VSLY
    df_VSL_i = df_VSL%>%filter(GID_0 == GID_0_i)
    VSL_i = df_VSL_i$Int_Dollar_2021
    VSLY_i = VSL_i/avg_years_left_to_live_i
    
    # treatment costs (hospital)
    df_scaling_i = df_scaling%>%filter(GID_0 == GID_0_i)
    cost_treatment_i = df_scaling_i$treat_cost_2021IntD
    cost_hosp_oop_i = df_scaling_i$countr_specific_OOP_2021IntD
    cost_hosp_gvt_i = cost_treatment_i - cost_hosp_oop_i
    
    # treatment costs (outpatient)
    df_cost_outpatient_visit_i = df_cost_outpatient_visit%>%filter(GID_0 == GID_0_i)
    cost_outpatientvisit_i = df_cost_outpatient_visit_i$Cost_USD_2021
    
    
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
          filter(scenario == vec_scenarios[1],
                 vaccEff == "70")
        
        ### Number of cases (total) = number of cases (unvaccinated) + number of cases (vaccinated)
        N_cases_U = df_simulationOutputsFinal_Deltas_i_j_k_l_dummyScenario$IncCumul_U_noVacc
        N_cases_V = df_simulationOutputsFinal_Deltas_i_j_k_l_dummyScenario$IncCumul_V_noVacc
        N_cases_total = N_cases_U + N_cases_V
        if(N_cases_total < 0){warning("negative cases"); break}
        
        ### Determine number of cases according to country-specific age pyramids
        # assume no relationship between age and infection risk
        df_cases = df_age_distr_i%>%ungroup()%>%
          dplyr::select(age_numeric, age_proportion)%>%
          mutate(N_cases = age_proportion * N_cases_total)%>%
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
        
        ### LOOP: prob_seq_l 
        # each "run" is a Monte Carlo draw of random parameter values
        for(prob_seq_l in vec_prob_seq){
          
          ### LOOP: run_m
          for(run_m in vec_runs){
            
            print(paste0("for country ", GID_0_i, 
                         ", on simulation ", simulation_j, " of ", length(vec_simulations), 
                         ", on year ", year_k, " of ", length(vec_years),
                         ", on run ", run_m, " of ", length(vec_runs)))
            
            ### update qounter for unaverted outcomes
            qounter_unaverted = qounter_unaverted + 1
            
            ### Load Monte Carlo parameter values
            df_params_m = df_params%>%filter(n_sim_montecarlo == run_m)
            
            # prob treatment
            prob_treat_comm_gvt_h = df_params_m$prob_treat_comm_gvt
            prob_treat_comm_oop_h = df_params_m$prob_treat_comm_oop
            if(prob_treat_comm_oop_h<0){warning("negative probability of non-gvt care")}
            
            # clinical parameters
            prob_symptoms = df_params_m$prob_symptoms
            prob_hosp = df_params_m$prob_hosp
            prob_death = df_params_m$prob_death
            prob_sequelae = prob_seq_l
            
            # durations
            dur_fever_h = df_params_m$dur_fever
            dur_ill_prehosp_h = df_params_m$dur_ill_prehosp
            dur_hosp_survived_h = df_params_m$dur_hosp_survived
            dur_hosp_died_h = df_params_m$dur_hosp_died
            
            # disutility
            disutility_fever_h = df_params_m$disutility_fever
            disutility_hosp_h = df_params_m$disutility_hospital
            disutility_sequelae_h = df_params_m$disutility_sequelae
            
            # DALY loss scaled by durations
            DALYperpatient_fever_h = dur_fever_h*disutility_fever_h/365
            DALYperpatient_hosp_survived_h = (dur_ill_prehosp_h + dur_hosp_survived_h)*disutility_hosp_h/365
            DALYperpatient_hosp_died_h = (dur_ill_prehosp_h + dur_hosp_died_h)*disutility_hosp_h/365
            DALYperpatient_sequelae_annual_h = disutility_sequelae_h
            
            ### Based on corresponding probabilities, determine numbers of individuals per age group having each outcome
            vec_N_symptoms = vec_N_cases * prob_symptoms
            vec_N_hospital = vec_N_cases * prob_hosp
            vec_N_death = vec_N_hospital * prob_death
            vec_N_sequelae = (vec_N_hospital - vec_N_death) * prob_sequelae
            vec_N_catastrophic = vec_N_hospital * prob_catastrophic_i
            vec_N_impoverished = vec_N_hospital * prob_impoverishment_i
            
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
              # add year and discount year
              mutate(realyear = year_k,
                     year_disc = as.numeric(realyear)-1)%>%
              # both undiscounted and discounted values (ref for continuous discount rate: https://resource-allocation.biomedcentral.com/articles/10.1186/1478-7547-11-18)
              mutate(YLL = N_death*life_exp_at_age_x,
                     YLL_disc = N_death*((1/discRate)*(1-exp(-discRate*life_exp_at_age_x))),
                     YWL = N_W_death*years_left_to_work,
                     YWL_disc = N_W_death*((1/discRate)*(1-exp(-discRate*years_left_to_work))),
                     YLS = N_sequelae*life_exp_at_age_x,
                     YLS_disc = N_sequelae*(1/discRate)*(1-exp(-discRate*life_exp_at_age_x)))%>%
              # calculate DALYs for fever (community), hospitalization and death
              mutate(DALY_fever = N_symptoms*DALYperpatient_fever_h, # DALYs for those with fever in community
                     DALY_hospital = (N_hospital-N_death)*DALYperpatient_hosp_survived_h + N_death*DALYperpatient_hosp_died_h,
                     DALY_sequelae = DALYperpatient_sequelae_annual_h*YLS,
                     DALY_sequelae_disc = DALYperpatient_sequelae_annual_h*YLS_disc,
                     DALY_death = YLL,
                     DALY_death_disc = YLL_disc)%>%
              mutate(DALY_total = DALY_fever + DALY_hospital + DALY_sequelae + DALY_death,
                     DALY_total_disc = DALY_fever + DALY_hospital + DALY_sequelae_disc + DALY_death_disc,
                     Cost_VSL = N_death*VSL_i,
                     Cost_VSLY = DALY_death_disc*VSLY_i)%>%
              # calculate DALY cost, outpatient costs, hospitalization costs (based on LOS and daily cost in Int_dollar)
              mutate(Cost_DALY_total = DALY_total*cost_daly_i,
                     Cost_outpatient_gvt = N_symptoms*prob_treat_comm_gvt_h*cost_outpatientvisit_i,
                     Cost_outpatient_oop = N_symptoms*prob_treat_comm_oop_h*cost_outpatientvisit_i,
                     Cost_hosp_gvt = N_hospital*cost_hosp_gvt_i,
                     Cost_hosp_oop = N_hospital*cost_hosp_oop_i)%>%
              # calculate OOP costs vs government care costs
              mutate(Cost_care_oop = Cost_outpatient_oop + Cost_hosp_oop,
                     Cost_care_gvt = Cost_outpatient_gvt + Cost_hosp_gvt)%>%
              # calculate productivity losses
              mutate(Cost_prod_fever = N_W_symptoms*cost_GNI_daily_i*dur_fever_h,
                     Cost_prod_hospital = (N_W_hospital-N_W_death)*cost_GNI_daily_i*(dur_ill_prehosp_h + dur_hosp_survived_h) + N_W_death*cost_GNI_daily_i*(dur_ill_prehosp_h + dur_hosp_died_h),
                     Cost_prod_death = cost_GNI_i * YWL,
                     Cost_prod_death_disc = cost_GNI_i * YWL_disc,
                     Cost_prod_total = Cost_prod_fever + Cost_prod_hospital + Cost_prod_death)%>%
              # calculate total costs and discount all future costs
              mutate(Cost_DALY_total_disc = (DALY_total_disc*cost_daly_i)*(1/(1+discRate)^year_disc),
                     Cost_care_gvt_disc = Cost_care_gvt*(1/(1+discRate)^year_disc),
                     Cost_care_oop_disc = Cost_care_oop*(1/(1+discRate)^year_disc),
                     Cost_prod_total_disc = (Cost_prod_fever + Cost_prod_hospital + Cost_prod_death_disc)*(1/(1+discRate)^year_disc))%>%
              # group societal costs
              mutate(Cost_societal = Cost_care_gvt + Cost_care_oop + Cost_prod_total,
                     Cost_societal_disc = Cost_care_gvt_disc + Cost_care_oop_disc + Cost_prod_total_disc)
            
            ### Fill final matrix with outputs
            m_outputs[qounter_unaverted,] = c("outcome" = "baseline",
                                              "GID_0" = GID_0_i,
                                              "simulation" = simulation_j,
                                              "realyear" = year_k,
                                              "run" = run_m,
                                              "prob_seq" = prob_seq_l,
                                              "scenario" = "no vaccine",
                                              "vaccEff" = "no vaccine",
                                              "N_cases" = N_cases_total,
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
                                              "YLS_disc" = sum(df_outcomes_unaverted$YLS_disc),
                                              "DALY_fever" = sum(df_outcomes_unaverted$DALY_fever),
                                              "DALY_hospital" = sum(df_outcomes_unaverted$DALY_hospital),
                                              "DALY_sequelae" = sum(df_outcomes_unaverted$DALY_sequelae),
                                              "DALY_sequelae_disc" = sum(df_outcomes_unaverted$DALY_sequelae_disc),
                                              "DALY_death" = sum(df_outcomes_unaverted$DALY_death),
                                              "DALY_death_disc" = sum(df_outcomes_unaverted$DALY_death_disc),
                                              "DALY_total" = sum(df_outcomes_unaverted$DALY_total),
                                              "DALY_total_disc" = sum(df_outcomes_unaverted$DALY_total_disc),
                                              "Cost_VSL" = sum(df_outcomes_unaverted$Cost_VSL),
                                              "Cost_VSLY" = sum(df_outcomes_unaverted$Cost_VSLY),
                                              "Cost_DALY_total" = sum(df_outcomes_unaverted$Cost_DALY_total),
                                              "Cost_outpatient_gvt" = sum(df_outcomes_unaverted$Cost_outpatient_gvt),
                                              "Cost_outpatient_oop" = sum(df_outcomes_unaverted$Cost_outpatient_oop),
                                              "Cost_hosp_gvt" = sum(df_outcomes_unaverted$Cost_hosp_gvt),
                                              "Cost_hosp_oop" = sum(df_outcomes_unaverted$Cost_hosp_oop),
                                              "Cost_care_oop" = sum(df_outcomes_unaverted$Cost_care_oop),
                                              "Cost_care_gvt" = sum(df_outcomes_unaverted$Cost_care_gvt),
                                              "Cost_prod_fever" = sum(df_outcomes_unaverted$Cost_prod_fever),
                                              "Cost_prod_hospital" = sum(df_outcomes_unaverted$Cost_prod_hospital),
                                              "Cost_prod_death" = sum(df_outcomes_unaverted$Cost_prod_death),
                                              "Cost_prod_death_disc" = sum(df_outcomes_unaverted$Cost_prod_death_disc),
                                              "Cost_prod_total" = sum(df_outcomes_unaverted$Cost_prod_total),
                                              "Cost_DALY_total_disc" = sum(df_outcomes_unaverted$Cost_DALY_total_disc),
                                              "Cost_care_gvt_disc" = sum(df_outcomes_unaverted$Cost_care_gvt_disc),
                                              "Cost_care_oop_disc" = sum(df_outcomes_unaverted$Cost_care_oop_disc),
                                              "Cost_prod_total_disc" = sum(df_outcomes_unaverted$Cost_prod_total_disc),
                                              "Cost_societal" = sum(df_outcomes_unaverted$Cost_societal),
                                              "Cost_societal_disc" = sum(df_outcomes_unaverted$Cost_societal_disc))
            
            ### LOOP: vaccine scenarios
            for(scenario_n in vec_scenarios){
              # scenario_n = "5" ## test case for Lassa
              # scenario_n = "160d_0.2" ## test case for Lassa-X
              
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
                N_cases_pruned_U = df_simulationOutputsFinal_Deltas_i_j_k_l_m_n_o$IncCumul_U_final
                N_cases_pruned_V = df_simulationOutputsFinal_Deltas_i_j_k_l_m_n_o$IncCumul_V_final
                
                ### Total number of cases pruned
                N_cases_pruned = N_cases_pruned_U + N_cases_pruned_V
                if(N_cases_pruned < 0){warning(paste0("negative cases pruned: ", N_cases_pruned))}
                
                ### Total number of cases averted = total cases minus total cases pruned
                N_cases_averted = N_cases_total - N_cases_pruned
                if(N_cases_averted < 0){warning(paste0("negative cases averted: ", N_cases_averted))}
                
                ### Calculate number of cases averted due to disease-prevention
                N_cases_averted_diseaseVacc = round(N_cases_pruned_V*(vaccEff_disease/100))
                
                ### Calculation proportions of cases averted due to infection blocking vs disease + infection blocking
                prop_cases_averted_infection = N_cases_averted/N_cases_total
                prop_cases_averted_infection_disease = (N_cases_averted + N_cases_averted_diseaseVacc)/N_cases_total
                
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
                  # add year and discount year
                  mutate(realyear = year_k,
                         year_disc = as.numeric(realyear)-1)%>%
                  # both undiscounted and discounted values (ref for continuous discount rate: https://resource-allocation.biomedcentral.com/articles/10.1186/1478-7547-11-18)
                  mutate(YLL = N_death*life_exp_at_age_x,
                         YLL_disc = N_death*((1/discRate)*(1-exp(-discRate*life_exp_at_age_x))),
                         YWL = N_W_death*years_left_to_work,
                         YWL_disc = N_W_death*((1/discRate)*(1-exp(-discRate*years_left_to_work))),
                         YLS = N_sequelae*life_exp_at_age_x,
                         YLS_disc = N_sequelae*(1/discRate)*(1-exp(-discRate*life_exp_at_age_x)))%>%
                  # calculate DALYs for fever (community), hospitalization and death
                  mutate(DALY_fever = N_symptoms*DALYperpatient_fever_h, # DALYs for those with fever in community
                         DALY_hospital = (N_hospital-N_death)*DALYperpatient_hosp_survived_h + N_death*DALYperpatient_hosp_died_h,
                         DALY_sequelae = DALYperpatient_sequelae_annual_h*YLS,
                         DALY_sequelae_disc = DALYperpatient_sequelae_annual_h*YLS_disc,
                         DALY_death = YLL,
                         DALY_death_disc = YLL_disc)%>%
                  mutate(DALY_total = DALY_fever + DALY_hospital + DALY_sequelae + DALY_death,
                         DALY_total_disc = DALY_fever + DALY_hospital + DALY_sequelae_disc + DALY_death_disc,
                         Cost_VSL = N_death*VSL_i,
                         Cost_VSLY = DALY_death_disc*VSLY_i)%>%
                  # calculate DALY cost, outpatient costs, hospitalization costs (based on LOS and daily cost in Int_dollar)
                  mutate(Cost_DALY_total = DALY_total*cost_daly_i,
                         Cost_outpatient_gvt = N_symptoms*prob_treat_comm_gvt_h*cost_outpatientvisit_i,
                         Cost_outpatient_oop = N_symptoms*prob_treat_comm_oop_h*cost_outpatientvisit_i,
                         Cost_hosp_gvt = N_hospital*cost_hosp_gvt_i,
                         Cost_hosp_oop = N_hospital*cost_hosp_oop_i)%>%
                  # calculate OOP costs vs government care costs
                  mutate(Cost_care_oop = Cost_outpatient_oop + Cost_hosp_oop,
                         Cost_care_gvt = Cost_outpatient_gvt + Cost_hosp_gvt)%>%
                  # calculate productivity losses
                  mutate(Cost_prod_fever = N_W_symptoms*cost_GNI_daily_i*dur_fever_h,
                         Cost_prod_hospital = (N_W_hospital-N_W_death)*cost_GNI_daily_i*(dur_ill_prehosp_h + dur_hosp_survived_h) + N_W_death*cost_GNI_daily_i*(dur_ill_prehosp_h + dur_hosp_died_h),
                         Cost_prod_death = cost_GNI_i * YWL,
                         Cost_prod_death_disc = cost_GNI_i * YWL_disc,
                         Cost_prod_total = Cost_prod_fever + Cost_prod_hospital + Cost_prod_death)%>%
                  # calculate total costs and discount all future costs
                  mutate(Cost_DALY_total_disc = (DALY_total_disc*cost_daly_i)*(1/(1+discRate)^year_disc),
                         Cost_care_gvt_disc = Cost_care_gvt*(1/(1+discRate)^year_disc),
                         Cost_care_oop_disc = Cost_care_oop*(1/(1+discRate)^year_disc),
                         Cost_prod_total_disc = (Cost_prod_fever + Cost_prod_hospital + Cost_prod_death_disc)*(1/(1+discRate)^year_disc))%>%
                  # group societal costs
                  mutate(Cost_societal = Cost_care_gvt + Cost_care_oop + Cost_prod_total,
                         Cost_societal_disc = Cost_care_gvt_disc + Cost_care_oop_disc + Cost_prod_total_disc)
                
                ### final vector of outputs AVERTED
                m_outputs_averted[qounter_averted,] = c("outcome" = "averted",
                                                        "GID_0" = GID_0_i,
                                                        "simulation" = simulation_j,
                                                        "realyear" = year_k,
                                                        "run" = run_m,
                                                        "prob_seq" = prob_seq_l,
                                                        "scenario" = scenario_n,
                                                        "vaccEff" = vaccEff_o_p,
                                                        "N_cases" = N_cases_averted,
                                                        "N_symptoms" = sum(df_outcomes_averted$N_symptoms),
                                                        "N_hospital" = sum(df_outcomes_averted$N_hospital),
                                                        "N_death" = sum(df_outcomes_averted$N_death),
                                                        "N_sequelae" = sum(df_outcomes_averted$N_sequelae),
                                                        "N_catastrophic" = sum(df_outcomes_averted$N_catastrophic),
                                                        "N_impoverished" = sum(df_outcomes_averted$N_impoverished),
                                                        "N_W_cases" = sum(df_outcomes_averted$N_W_cases),
                                                        "N_W_symptoms" = sum(df_outcomes_averted$N_W_symptoms),
                                                        "N_W_hospital" = sum(df_outcomes_averted$N_W_hospital),
                                                        "N_W_death" = sum(df_outcomes_averted$N_W_death),
                                                        "N_W_sequelae" = sum(df_outcomes_averted$N_W_sequelae),
                                                        "N_W_catastrophic" = sum(df_outcomes_averted$N_W_catastrophic),
                                                        "N_W_impoverished" = sum(df_outcomes_averted$N_W_impoverished),
                                                        "YLL" = sum(df_outcomes_averted$YLL),
                                                        "YLL_disc" = sum(df_outcomes_averted$YLL_disc),
                                                        "YWL" = sum(df_outcomes_averted$YWL),
                                                        "YWL_disc" = sum(df_outcomes_averted$YWL_disc),
                                                        "YLS" = sum(df_outcomes_averted$YLS),
                                                        "YLS_disc" = sum(df_outcomes_averted$YLS_disc),
                                                        "DALY_fever" = sum(df_outcomes_averted$DALY_fever),
                                                        "DALY_hospital" = sum(df_outcomes_averted$DALY_hospital),
                                                        "DALY_sequelae" = sum(df_outcomes_averted$DALY_sequelae),
                                                        "DALY_sequelae_disc" = sum(df_outcomes_averted$DALY_sequelae_disc),
                                                        "DALY_death" = sum(df_outcomes_averted$DALY_death),
                                                        "DALY_death_disc" = sum(df_outcomes_averted$DALY_death_disc),
                                                        "DALY_total" = sum(df_outcomes_averted$DALY_total),
                                                        "DALY_total_disc" = sum(df_outcomes_averted$DALY_total_disc),
                                                        "Cost_VSL" = sum(df_outcomes_averted$Cost_VSL),
                                                        "Cost_VSLY" = sum(df_outcomes_averted$Cost_VSLY),
                                                        "Cost_DALY_total" = sum(df_outcomes_averted$Cost_DALY_total),
                                                        "Cost_outpatient_gvt" = sum(df_outcomes_averted$Cost_outpatient_gvt),
                                                        "Cost_outpatient_oop" = sum(df_outcomes_averted$Cost_outpatient_oop),
                                                        "Cost_hosp_gvt" = sum(df_outcomes_averted$Cost_hosp_gvt),
                                                        "Cost_hosp_oop" = sum(df_outcomes_averted$Cost_hosp_oop),
                                                        "Cost_care_oop" = sum(df_outcomes_averted$Cost_care_oop),
                                                        "Cost_care_gvt" = sum(df_outcomes_averted$Cost_care_gvt),
                                                        "Cost_prod_fever" = sum(df_outcomes_averted$Cost_prod_fever),
                                                        "Cost_prod_hospital" = sum(df_outcomes_averted$Cost_prod_hospital),
                                                        "Cost_prod_death" = sum(df_outcomes_averted$Cost_prod_death),
                                                        "Cost_prod_death_disc" = sum(df_outcomes_averted$Cost_prod_death_disc),
                                                        "Cost_prod_total" = sum(df_outcomes_averted$Cost_prod_total),
                                                        "Cost_DALY_total_disc" = sum(df_outcomes_averted$Cost_DALY_total_disc),
                                                        "Cost_care_gvt_disc" = sum(df_outcomes_averted$Cost_care_gvt_disc),
                                                        "Cost_care_oop_disc" = sum(df_outcomes_averted$Cost_care_oop_disc),
                                                        "Cost_prod_total_disc" = sum(df_outcomes_averted$Cost_prod_total_disc),
                                                        "Cost_societal" = sum(df_outcomes_averted$Cost_societal),
                                                        "Cost_societal_disc" = sum(df_outcomes_averted$Cost_societal_disc))
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
