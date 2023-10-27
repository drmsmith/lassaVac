################################
### MODEL INITIAL CONDITIONS ###
################################

library('conflicted')
library('tidyverse')
conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    .quiet = T
)

if (!interactive()) { # not run when file is sourced to avoid duplication
    set.seed(18102023)
    # run p_spillover.R to obtain the file below 
    # from df_burden which contains 205 UN-adj worldpop data per country 
    df_burden = read.csv('chikX/data/df_burden_with_pop_size_2015_spillover.csv')
    ### Load gravity spread list needed to inform initial conditions
    # list_gravity_spread = get(load("LassaX/data_chik/inputs_list_gravity_spread.RData"))
    list_gravity_spread = get(load("chikX/res/inpts_ls_spread.RData"))
}


# for any given m_spread as simulated through gravity model, 
# function to establish corresponding initial conditions for ODEs

# there are seven initial conditions with this model 
# no vac, 3x vac rates, 2x vac efficiencies 

# will return NULL for simulations where CHIK-X does not establish anywhere!
f_initialConditions = function(m_spread){
    
    ### Based on gravity spread, determine timing of 
    ### outbreaks and vaccination, and doses of vaccine
    m_spread_conditions = matrix(NA, nrow = nrow(m_spread), ncol = 3)
    rownames(m_spread_conditions) = rownames(m_spread)
    colnames(m_spread_conditions) = c("code", "timing", 'amplitude')
    sim_length_days = ncol(m_spread)
    
    ### assume initial sizes upon outbreak detection are exponentially 
    # decreasing from n = n_inits at outbreak onset to n = n_subs at 1 year
    # (removed initial size estimation for now) 

    ### Determine initial outbreak size and timing for each district's outbreak
    for(catchment_i in rownames(m_spread)){
        
        m_spread_i = m_spread[catchment_i,]
        
        outbreak_length = sum(m_spread_i)
        timing_i = ifelse(outbreak_length > 0,
                          sim_length_days - outbreak_length + 1, # start day = 1
                          NA) # NA if no spread to a location 
        
        # P_ESTABLISHMENT ~ sampling mean/min/max infections 
        # sometimes min=0 ==> no establishment ==> removed
        # unofrm sample from max / min / mean 
        amplitude = sample(
            df_burden[df_burden$code==catchment_i,
                      c('infections_mean', 'infections_min','infections_max')], 1)
        amplitude = unlist(unname(amplitude))
        
        # update matrix with these values
        m_spread_conditions[catchment_i, "code"] = catchment_i
        m_spread_conditions[catchment_i, "timing"] = timing_i
        m_spread_conditions[catchment_i, "amplitude"] = amplitude
    }
    
    # create dataframe binding final updated m_spread_conditions with 
    # population size of each
    # select only the districts that have any transmission
    df_spread_conditions = as.data.frame(m_spread_conditions) %>%
        left_join(
            df_burden %>% 
                dplyr::select(code, country, total_pop_size),
            by = "code"
        ) %>%
        dplyr::filter( !is.na(timing) & amplitude > 0 ) %>%
        mutate(
            timing = as.numeric(timing),
            amplitude = as.numeric(amplitude)
        )
    if (nrow(df_spread_conditions) < 1) { 
        return(NULL)} else { return(df_spread_conditions) }
}

############
### TEST ###
############

# m_spread1 = f_gravity_model()
# which(m_spread1[,365*2]==0)
# m_spread2 = f_gravity_model()
# 
# df_initialConditions1 = f_initialConditions(m_spread1)
# df_initialConditions2 = f_initialConditions(m_spread2)




########################################################################
### GENERATE INITIAL CONDITIONS FROM SPREAD MATRICES AND SAVE OUTPUT ###
########################################################################


f_initialConditions_run = function(
        list_gravity_spread, # list of matrices showing when the outbreak reached a place 
        .save_res=F, # by default returns the list of initial conditions  
        .dest_dir=NULL, # directory where file should be saved (should exist)
        .print_progr=F){ # for debugging -- there is a progress bar by default

    ### Determine initial conditions for each element of list_gravity_spread
    list_initial_conditions = map(list_gravity_spread, function(m_spread_i){
        list_initial_conditions_i = f_initialConditions(m_spread_i)
    }, .progress = T)
    ### Save initial conditions
    if (.save_res == T){
        if (is.null(.dest_dir)==T || file.exists(.dest_dir)==F) {
            stop(paste('invalid dest_dir:\n"', .dest_dir,'"', sep='')) 
        } else {
            # cook up correct file name depending on input style  
            fname = ifelse(
                endsWith(.dest_dir, '/'), 
                paste(.dest_dir, "inputs_ls_initial_conditions.RData", sep=''),
                paste(.dest_dir, "/inputs_ls_initial_conditions.RData", sep='')
            )
            save(list_initial_conditions, file = fname)
            return(list_initial_conditions)
            }
    } else return(list_initial_conditions)
}


# quick check if function runs as desired 
if (!interactive()) { # not run when file is sourced 
    f_initialConditions_run(list_gravity_spread = list_gravity_spread,
                            .save_res = T, .dest_dir = 'chikX/res')
}

