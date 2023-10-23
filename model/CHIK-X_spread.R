library('tidyverse')
library('geosphere')

if (!interactive()) { # not run when file is sourced to avoid duplication
    set.seed(18102023)
    # run p_spillover.R to obtain the file below 
    # from df_burden which contains 205 UN-adj worldpop data per country 
    df_burden = read.csv('chikX/data/df_burden_with_pop_size_2015_spillover.csv')
}



#####################
### GRAVITY MODEL ###
#####################


### Function for gravity model (daily probability of spread over assumed duration of spread)
# duration spread given in days 
f_gravity_model = function(
        duration_spread, # in days 
        # regions/countries to loop over, needs code and p_spillover columns  
        df_catchments,
        # mobility matrix determining the probability of spread 
        # between any two locations 
        mat_mob_p,
        .doPrint = F){ # for internal debugging 
    
    # Matrix of spread across catchments (to be updated in loop with gravity model)
    m_spread = matrix(0, nrow = nrow(df_catchments), ncol = duration_spread)
    colnames(m_spread) = 1:duration_spread
    rownames(m_spread) = df_catchments$code
    
    # identify first catchment, add to vector of infected catchments, update spread matrix
    catchment0 = sample(df_catchments$code, size = 1, prob = df_catchments$p_spillover)
    
    vec_catchments_infected = catchment0
    
    # fill rest of row with 1 (assumption is that catchments do not return to susceptible)
    m_spread[catchment0,] = 1
    
    # go through outbreak and update matrices day-by-day
    for(date_i in 1:duration_spread){
        if(.doPrint == T){print(paste0("evaluating spread on day ", date_i))}
        
        # for each catchment currently infected
        for(catchment_infect_j in vec_catchments_infected){
            # print(paste0("on day ", date_i, ", evaluating potential spread from ", catchment_infect_j))
            
            # evaluate probability of spread to all catchments not currently infected
            for(catchment_suscept_k in df_catchments$code[!df_catchments$code %in% vec_catchments_infected]){
                
                # gravity term --> MVT MATRIX 
                # p moving a->b
                # vaccine code (no vaccination for now -- keep )
                infect_i_j = rbinom(1, 1, mat_mob_p[catchment_infect_j, catchment_suscept_k])
                
                # if infected, update infected catchments and corresponding m_spread matrix
                if(infect_i_j == 1){
                    # print(paste0(
                    #     "on day ", date_i, " there was spread from ", 
                    #     catchment_infect_j, " to ", catchment_suscept_k))
                    
                    vec_catchments_infected = append(vec_catchments_infected, 
                                                     catchment_suscept_k)
                    
                    m_spread[catchment_suscept_k, date_i:ncol(m_spread)] = 1
                }
            }
        }
    }
    
    return(m_spread)
}




### TEST
# m_spread = f_gravity_model(doPrint = T)
# which(m_spread[,1] == 1)
# which(m_spread[,365] == 1)
# which(m_spread[,365*2] == 1)
# which(m_spread[,365*2] == 0)

# quick check where/when there has been spread
if (!interactive()) { # not run when file is sourced 
    m_spread1 = f_gravity_model(duration_spread = 365*2, 
                                df_catchments = df_burden, 
                                mat_mob_p = mat_mob_p)
    apply(m_spread1, 1, sum) %>% .[.!=0] %>% sort(decreasing = T) #%>% length
}


###################################
### RUN N TIMES AND SAVE OUTPUT ###
###################################

# runs gravity model for desired number of iterations 
# can save output 
f_gravity_model_run = function(
        .df_catchments, # data frame with potential spread locations 
        .mat_mob = mat_mob_p, # mobility matrix ~ p_movemment from a to b 
        .n_spread_matrices = 100, 
        .duration_spread = 365*2,
        .save_res = F, # returns list when F
        .dest_dir = NULL, # string 
        .print_res = F # will slow down running the model
) {
    # takes about 2-3 min to run 
    # has a progress bar 
    list_gravity_spread = purrr::map(
        1:.n_spread_matrices, # for 1:nreps do 
        function(gravity_i){
            # call f_gravity_model
            m_spread = f_gravity_model(
                duration_spread = .duration_spread, 
                df_catchments = .df_catchments, 
                mat_mob_p = .mat_mob
                )
            if (.print_res == T) {
                print(paste0(
                    "on stochastic gravity run ", gravity_i, " of ", .n_spread_matrices
                    ))
                spread_sums = apply(m_spread, 1, function(row) sum(row!=0))
                spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% print
            }
            return(m_spread)
        }, 
        .progress=T)

    #### SAVE OUTPUT ####     
    if (.save_res == T){
        if (is.null(.dest_dir)==T || file.exists(.dest_dir)==F) {
            stop(paste('invalid dest_dir:\n"', .dest_dir,'"', sep='')) 
            } else {
                # cook up correct file name depending on input style  
                fname = ifelse(
                    endsWith(.dest_dir, '/'), 
                    paste(.dest_dir, "inputs_ls_spread.RData", sep=''),
                    paste(.dest_dir, "/inputs_ls_spread.RData", sep='')
                               )
                save(
                    list_gravity_spread,
                    file = fname
                )
                return(list_gravity_spread)
                }
    } else return(list_gravity_spread)
}


# quick check if function runs as desired 
if (!interactive()) { # not run when file is sourced 
    f_gravity_model_run(.n_spread_matrices = 5, .save_res = T, .dest_dir = 'chikX/res')
}



