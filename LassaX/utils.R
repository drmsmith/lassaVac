# hyperbolic funciton for outbreak shape 
# sinh curve used to simulate outbreak 
shin_curve = function(xs=xdates,
                      amplitude=params[1],
                      h_transl=xdates[which(ycases == max(ycases))],
                      s_l=params[2],
                      s_r=params[3]){
    # simulate curve 
    ys =  amplitude*2 / ( exp((xs-h_transl)*s_l) + exp((-xs+h_transl)*s_r) )
    # add noise 
    # ys = ys+rnorm(length(ys), mean(ys), mean(ys)*0.05)
    # # filter out <0
    # ys = ifelse(ys<0, 0, ys)
    return(ys)
}



### Set 1 ###
# Long output, 1 simulation
# vaccine 50%, 70% or 90% effective against infection
# no immunity at outset (propImmun = 0)
# allocate vac to recovereds and susceptibles (parVacStrat = 2)
# 10% wastage of vaccine doses
# 2 year time horizon for each outbreak in each district
if(output_set == 1){
    output_format = "output_long"
    first_sim = 1
    n_simulations = 1 # a single simulation
    vec_vacc_eff = c(0.5, 0.7,0.9) # effective rate of vaccination
    par_propImmun = 0 # proportion immune upon simulation onset
    par_parVacStrat = 2 # vaccine allocation strategy (see ODEs)
    wastage = 0.1
    n_duration_j = 365*2 # duration of simulation within each district
}

### Set 2 ###
# Brief output, 100 simulations
# vaccine 90% effective against infection
# no immunity at outset (propImmun = 0)
# allocate vac to recovereds and susceptibles (parVacStrat = 2)
# 10% wastage of vaccine doses
# 2 year time horizon for each outbreak in each district
if(output_set == 2){
    output_format = "output_brief"
    first_sim = 1
    n_simulations = 100 # a single simulation
    vec_vacc_eff = c(0, 0.5, 0.7,0.9) # effective rate of vaccination
    par_propImmun = 0 # proportion immune upon simulation onset
    par_parVacStrat = 2 # vaccine allocation strategy (see ODEs)
    wastage = 0.1
    n_duration_j = 365*2 # duration of simulation within each district
}





#### parallel computation progress bar 
# Progress combine function
# a progress bar which does not work.... but worked in the example.... 
f <- function(iterator){
    pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
    count <- 0
    function(...) {
        count <<- count + length(list(...)) - 1
        setTxtProgressBar(pb, count)
        flush.console()
        list(...) # this can feed into .combine option of foreach
    }
}