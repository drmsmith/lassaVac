library('tidyverse')
library('cowplot')

###################
# POST-PROCESSING #
###################

# get all file paths 
all_files = list.files('res/individual_simulations/', 'list_diseaseX_i_', full.names = T) %>%
    str_sort(numeric = TRUE) #%>% .[1:20]

#############################
# all simulations in one df # 
#############################
df_all_sims = map(all_files, function(dirname){
    res = get(load(dirname))
    map(res, function(.x) .x$sim_res) %>% bind_rows
}) %>% bind_rows


# write.csv(df_all_sims, file = 'res/sim_res_100.csv', row.names=F)

#### FOR THESE NOT SO GRACEFUL PLOTS 
# SUBSET THE FIRST 20 SIMULATIONS IN THE FIRST 
# LOOP ABOVE i.e. all_files[1:20]
# (otherwise, plots are illegible)

# TIME 
ggplot(df_all_sims, aes(time_years, cases_sim)) + 
    geom_point(aes(color=timing), size=1, alpha=0.2) +  # size=timing
    facet_wrap(~simulation, scales = 'free_y', nrow = 4) + 
    theme_bw()

# COUNTRY
ggplot(df_all_sims, aes(time_years, cases_sim)) + 
    geom_point(aes(color=code), size=1, alpha=0.2) +  # size=timing
    facet_wrap(~simulation, scales = 'free_y', nrow = 4) + 
    theme_bw()




##########################################
# PLOT individual simulations by country # 
##########################################
# save plots for every sim ~countries 

# extract each sim into one large list 
ls_all_sim_res = map(all_files, function(dirname){
    res = get(load(dirname))
    map(res, function(.x) .x$sim_res) %>% bind_rows
},.progress=T) 


# save plots for every sim ~countries 
# here, every panel has a unique y-axis
# remove 'free_y' below to undo this 
map(ls_all_sim_res, function(.x) {
    
    df_timing = .x[,c('timing', 'country')] %>% unique 
    df_timing$timing = df_timing$timing/365.25
    n_sim = .x[1,'simulation']
    
    ggp = ggplot(.x, aes(time_years, daily_infections_sim)) + 
        geom_point(aes(color=country), size=1, alpha=0.2) + 
        geom_vline(data=df_timing, aes(xintercept = timing, color=country), linetype='dashed') + 
        facet_wrap(~country, scales = 'free_y', nrow = 4) + 
        guides(color=guide_legend(override.aes = list(linetype = 0))) + 
        labs(x='Time (years)', y='Simulated Daily Infections', 
             title=paste('Simulation', n_sim, sep=' ')) +
        theme_light() + guides(color='none')
    
    ggsave(plot = ggp, dpi=250, height = 5, width = 7, units = 'in',
           filename = paste('figs/diagnostic_plots/sim_', n_sim, '.png', sep=''))
    # return(ggp)
}, 
.progress = T)


###################################
# save giff with all simulations # 
##################################
# be careful because this easily uses  6-7 GB of RAM 
# on my machine, not sure why.... 
# run gc() to free up memory 

library('magick')

imgs <- list.files('figs/diagnostic_plots', full.names = TRUE, pattern='.png') %>%
    str_sort(numeric = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 1 frame per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated, path = 'figs/all_outbreaks.gif')

# free up RAM 
gc()


#####################################################
# examine outbreak size vs pop size and estimation # 
####################################################
# compare cumulative infections to population size 
# and simulated peak size to mean/min/max estimtd infections 

df_burden = read.csv('data/df_burden_with_pop_size_2015_spillover.csv')

# extract only country, peak, and cumulative infections 
sim_out_sizes = df_all_sims %>% 
    group_by(simulation, code) %>%
    mutate(peak_inf = max(daily_infections_sim)) %>%
    ungroup() %>%
    select(country, code, peak_inf, IncCumul_U_final) %>%
    unique %>% arrange(code) %>% as.data.frame

# extract population size and estimated infections 
df_burden_inf_pop =  df_burden %>% select(code, infections_mean,
                                          infections_min, infections_max,
                                          total_pop_size) %>% arrange(code) 

# large data frame which allows comparisons
df_combined = merge(sim_out_sizes, df_burden_inf_pop, by = 'code') %>% 
    mutate(larger_than_pop = ifelse(total_pop_size - IncCumul_U_final < 0, 'yes', 'no'))


# scatter shows the size of peak for every simulation
# panels = countries 
# size = cumulative infections (simulated)
# color = whether cumulative infections > 2015 UN-adj worldpop size
ggplot(df_combined, aes(1, peak_inf)) + 
    geom_jitter(aes(color = larger_than_pop, size = IncCumul_U_final), alpha=0.5) + 
    facet_wrap(~code, scales = 'free_y') + 
    theme_light() + 
    labs(y = 'Peak of Simulated Daily Infections', x='Arbitrary') +
    theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    ) + 
    guides(size = guide_legend(title='Cumul infect'), 
           color = guide_legend(title='Cumul infect > \npop size'))







###################################################
# ACCESSING SPECIFIC SIMULATIONS AND/OR COUNTRIES # 
###################################################


########################################
# select one simulation and/or country #
########################################
# from large df with all results 

nsim = 1
ccode = 'JPN'

df_all_sims %>% filter(simulation==nsim, code==ccode) %>% 
    .$daily_infections_sim %>% sum
# .$daily_infections_sim %>% max



###########################################
# also allows to extract shape parameters #
###########################################
# loads in the simulation from file 

grab_sim_data = function(n_sim = 62, fpaths = all_files, sim_info=F) {
    fname = paste0('simulation_', n_sim)
    fpath = fpaths[str_detect(fpaths, fname)]
    l_sim_res = get(load(fpath))
    if (sim_info == T) {
        df_simres = map(l_sim_res, function(.x) .x$sim_params_info) 
    } else {
        df_simres = map(l_sim_res, function(.x) .x$sim_res) %>% bind_rows        
    }
    
}

# example 
nsim = 79#71#12
cname = 'India'#'Malaysia'#'Japan'#'Singapore'

grab_sim_data(nsim, sim_info=T) %>% map(function(.dat) {
    if (.dat$simulation_info[1,'country'] == cname) bind_rows(.dat)
    else NULL
}) %>% compact # drops NULL list entries

grab_sim_data(nsim, sim_info=F) %>% filter(country==cname) %>% 
    select(time_years, daily_infections_sim, timing) %>% dim
#    select(time_years, daily_infections_sim) %>% plot




