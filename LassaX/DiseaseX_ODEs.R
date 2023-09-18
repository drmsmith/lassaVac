############
### ODEs ###
############

### SEIR model with vaccination describing Lassa-X transmission dynamics at the ADM1 (district) level

### Vaccination scenarios (in line with 100 days mission): 
# low scenario: 10M doses/yr, reaching 2.5% of popn
# medium scenario: vaccinate 20% of country popn/yr
# high scenario: vaccinate 40% of country popn/yr for first two years, remaining 20 over following

### considerations:
# parVacStrat parameter: vaccines target vaccines only S (==1) or equally S and R () equally S and R, or weighted S over R?


f_ODEs_SEIR_Rt <- function(time,
                           state,
                           parameters){
  
  with(as.list(c(state,parameters)),{
    
    ### time-varying transmission rate
    # interpolate daily reproductive number
    Rt <- f_interpolate_Rt(time)
    
    ### calculate beta from Rt = beta*S/gamma
    # but assume S=1 (reflecting overall small Ebola outbreaks relative to population size, and so
    # little impact of exhaustion of susceptibles on Rt)
    beta = as.numeric(Rt*gamma)#/(S/(S+E+I+R))
    
    # switch parameter to turn off transmission if everyone vaccinated
    switch = 1
    
    ### ODEs
    # CASE 1: vaccination only targets susceptibles
    if(parVacStrat == 1){
      
      ### if vaccination not yet initiated, no doses
      if(time < t_vacc_start){doses_eff <- 0}else{
        ### if no susceptibles left, no doses
        if(S <= doses){doses_eff <- 0; switch = 0}else{doses_eff <- doses}
      }
      
      ### if tiny numbers of infections, set to 0
      if(E < 0.001){E=0}
      if(I < 0.001){I=0}
      if(VE < 0.001){VE=0}
      if(VI < 0.001){VI=0}
      
      dS <- -beta*S*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch - doses_eff*(1-wastage)
      dE <- beta*S*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch - alpha*E
      dI <- alpha*E-gamma*I
      dR <- gamma*I
      
      dV <- doses_eff*(1-wastage) - (1-vacc_eff)*beta*V*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch
      dVE <- (1-vacc_eff)*beta*V*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch - alpha*VE
      dVI <- alpha*VE-gamma*VI
      dVR <- gamma*VI
      
      # incidence
      dIncCumul_U = beta*S*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch
      dIncCumul_V = (1-vacc_eff)*beta*V*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch
      # include wasted doses in dosesCumul to have total number "used" from production POV
      dDosesCumul = doses_eff
    }
    
    # CASE 2: vaccination indiscriminate susceptibles/recovereds
    if(parVacStrat == 2){
      
      ### if vaccination not yet initiated, no doses
      if(time < t_vacc_start){doses_eff <- 0}else{
        ### if no susceptibles or recovereds left, turn off vaccination
        if(S+R <= doses){doses_eff <- 0; switch = 0}else{doses_eff =  doses}
      }
      
      ### if tiny numbers of infections, set to 0
      if(E < 0.001){E=0}
      if(I < 0.001){I=0}
      if(VE < 0.001){VE=0}
      if(VI < 0.001){VI=0}
      
      dS <- -beta*S*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch - doses_eff*(1-wastage)*(S/(S+R))
      dE <- beta*S*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch - alpha*E
      dI <- alpha*E-gamma*I
      dR <- gamma*I - doses_eff*(1-wastage)*(R/(S+R))
      
      dV <- doses_eff*(1-wastage)*(S/(S+R)) - (1-vacc_eff)*beta*V*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch
      dVE <- (1-vacc_eff)*beta*V*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch - alpha*VE
      dVI <- alpha*VE-gamma*VI
      dVR <- doses_eff*(1-wastage)*(R/(S+R)) + gamma*VI
      
      # incidence
      dIncCumul_U = beta*S*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch
      dIncCumul_V = (1-vacc_eff)*beta*V*(I+VI)/(S+E+I+R+V+VE+VI+VR)*switch
      # include wasted doses in dosesCumul to have total number "used" from production POV
      dDosesCumul = doses_eff
    }
    
    # return outputs
    return(list(c(dS,dE,dI,dR,dV,dVE,dVI,dVR,dIncCumul_U,dIncCumul_V,dDosesCumul)))})
}


################
### RUN ODEs ###
################

## function to integrate ODEs
f_simulate_ODEs <- function(time,
                            states_init,
                            ODEs,
                            parameters, 
                            method_simu = 'bdf_d'){
  
  out<-ode(y=states_init,
           times=time,
           func=ODEs,
           parms=parameters,
           method=method_simu)
  
  return(as.data.frame(out))
}


############
### TEST ###
############

# ### Example ODE simulation
# 
# # Rt curve
# randInteger = round(runif(1,1,1000))
# f_interpolate_Rt = approxfun(list_Rt_ebola_i[[7]][["m_Rt"]][randInteger,5:ncol(list_Rt_ebola_i[[7]][["m_Rt"]])], rule = 2)
# 
# # incubation period
# par_alpha_shape = 11.1191707
# par_alpha_rate = 1.084107
# par_alpha = 1/(par_alpha_shape/par_alpha_rate) # duration of non-infectious exposed period
# 
# # infectious period
# par_gamma_shape = 1.862467
# par_gamma_rate = 0.164666
# par_gamma = 1/(par_gamma_shape/par_gamma_rate) # duration of infectious period
# 
# df_ODEs_simulated = f_simulate_ODEs(time = 0:500,
#                                      states_init = c(S = 100000, E = 100, I = 100, R=0, V=0, VE = 0, VI = 0, VR = 0, 
#                                                      dIncCumul_U = 0, dIncCumul_V = 0, DosesCumul = 0),
#                                      ODEs = f_ODEs_SEIR_Rt,
#                                      parameters = c(alpha = par_alpha, gamma = par_gamma,
#                                                     parVacStrat = 2, t_vacc_start = 10, vacc_eff = 0.6,
#                                                     wastage = 0.1,
#                                                     doses = 100))
# 
# df_ODEs_simulated%>%
#   ggplot(aes(x = time, y = E+I + VE + VI))+
#   geom_line()+
#   theme_bw()+
#   theme(legend.position = "none")
