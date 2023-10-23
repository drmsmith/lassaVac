library('tidyverse')
library('cowplot')

###### OLD RES BEFORE FIXING SIMULATION CURVES 
# all_files = list.files('chikX/res/', 'list_diseaseX_i_outputSet_2') %>%
#     str_sort(numeric = TRUE) #%>% .[1:20]
# changed chikX/res/ to chikX/res
# sim_res --> sim_res
# daily_infections_sim --> daily_infections_sim


all_files = list.files('chikX/res/', 'list_diseaseX_i_', full.names = T) %>%
    str_sort(numeric = TRUE) #%>% .[1:20]


# all simulations in one df 
df_all_sims = map(all_files, function(dirname){
    res = get(load(dirname))
    map(res, function(.x) .x$sim_res) %>% bind_rows
}) %>% bind_rows


############ PLOT SIMS ############ 
# save plots for every sim ~countries 

# extract each sim into one large list 
ls_all_sim_res = map(all_files, function(dirname){
    res = get(load(dirname))
    map(res, function(.x) .x$sim_res) %>% bind_rows
},.progress=T) 


# save plots for every sim ~countries 
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
           filename = paste('chikX/diagnostics_new/sim_', n_sim, '.png', sep=''))
    # return(ggp)
}, 
.progress = T)




ls_all_sim_res[[2]]

ls_all_sim_res[[23]]



df_all_sims %>% filter(simulation==1, code=='PAK') %>% 
    .$daily_infections_sim %>% sum
    # .$time_years %>% range
    # .$daily_infections_sim %>% max

shape_params_fits = map(all_files, function(dirname){
    res = get(load(paste('chikX/res/', dirname, sep='')))
    map(res, function(.x) .x$params$sample_params) %>% bind_rows
}) %>% bind_rows

shape_params_fits$country %>% unique %>% sort

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

# 12, singapore 
# 18, south korea, india
# 50, panama 
# 62 indonesia 
# 71, japan, malaysia, croatia 
# 79, india 
# 82, laos, vietnam 
# 93, india 
sim62 = grab_sim_data(62, sim_info=T) %>% map(function(.dat) {
    if (.dat$simulation_info[1,'country'] == 'Indonesia') bind_rows(.dat)
    else NULL
    }) %>% compact
sim62 %>% filter(country=='Indonesia') %>% select(time_years, daily_infections_sim, timing)
    select(time_years, daily_infections_sim) %>% plot
699/365

nsim = 79#71#12
cname = 'India'#'Malaysia'#'Japan'#'Singapore'

grab_sim_data(nsim, sim_info=T) %>% map(function(.dat) {
    if (.dat$simulation_info[1,'country'] == cname) bind_rows(.dat)
    else NULL
}) %>% compact
grab_sim_data(nsim, sim_info=F) %>% filter(country==cname) %>% 
    select(time_years, daily_infections_sim, timing) %>% dim
    select(time_years, daily_infections_sim) %>% plot
622/365

# the problem is always brazil 
# the underlying problem seems to be 
# the dt calculation of the duration of spread 






# IDENTIFYING SHAPES TO REMOVE 

poor_shapes_tab = read.table(file = 'clipboard', sep=',', header=T) %>% as.list 
codes = poor_shapes_tab %>%
    .$code %>% str_remove_all(' ') %>% str_split('-') 
ls_poor_shapes = map2(poor_shapes_tab$sim, codes, function(.x,.y) list(nsim=.x, codes=.y))


# fetches specified countries from specified simulations in 
# poor_shapes_tab 
poor_shapes_params = map(ls_poor_shapes, function(.shapes_ls){
    nsim = .shapes_ls$nsim 
    codes_i = .shapes_ls$codes
    fname = all_files[all_files==paste('list_diseaseX_i_simulation_', 
                                       nsim, '.RData', sep='')]
    fpath = paste('chikX/res/', fname, sep='')
    res = get(load(fpath))
    sim_params = map(res, function(.country) {
        code_i = .country$sim_res[1,'code']
        if (code_i %in% codes_i == T){
            .params = .country$params
            sim_p = c(id = 'sim_p', nsim=nsim, code_i, .params$s_l, .params$s_r)
            sample_p = c(
                id = 'sample_p', 
                nsim=nsim, unlist(.params$sample_params[1,c('code','s_l','s_r')]))
            df_out = data.frame(sim_p=sim_p, sample_p=sample_p
                                ) %>% t %>% as.data.frame
            return(df_out)
        } else invisible()
        }) %>% compact
    }) %>% bind_rows
rownames(poor_shapes_params) = NULL

poor_shapes_params %>% filter(id=='sample_p') %>% select(code) %>%
    unlist %>% unname %>% 
    countrycode::countrycode(., origin='iso3c', destination='country.name') %>%
    table %>% sort(decreasing = T) 
    unique %>% sort %>% 
    countrycode::countrycode(., origin='iso3c', destination='country.name')
    
