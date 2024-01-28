library('tidyverse')
conflicted::conflicts_prefer(
    dplyr::filter(), 
    .quiet = T
)


###############
# WHO REGIONS #
###############

who_regions = read.csv('preprocessing/data/who-regions.csv')

dim(who_regions)

who_codes_6 = read.table('clipboard', header=F, sep='\t')
colnames(who_codes_6) = c('region_code', 'region_name')

who_reg_name_code = map(
    unique(who_regions$WHO.region), function(.reg_name) {
        reg_code = who_codes_6$region_code[str_detect(who_codes_6$region_name, .reg_name)]
        reg_code = ifelse(length(reg_code)==0, NA, reg_code)
        out = data.frame(
            region_name = .reg_name,
            region_code = reg_code
        )
        return(out)
    }) %>% bind_rows


colnames(who_regions) = c('country_name', 'country_code', 'year', 'region_name')
who_regions <- select(who_regions, country_name, country_code, region_name)

df_countries_who_regions_codes <- right_join(who_regions, who_reg_name_code, by='region_name')

# write.csv(df_countries_who_regions_codes, 'methods/df_countries_who_regions_codes.csv', row.names=F)






sim_res = get(load('res/individual_simulations/list_diseaseX_i_simulation_2.RData'))
df_res = sim_res[[1]]$sim_res
plot(df_res$time_years, df_res$daily_infections_sim)
df_res$derived = c(0, diff(df_res$daily_infections_sim))
plot(df_res$time_years, df_res$derived)

df_res$inds = sapply(df_res$derived, function(.x) ifelse(all.equal(.x, 0, tolerance=0.9)==T, T, F))
fltrd = filter(df_res, !((time_years > 1) & (inds==T)))

fltrd = filter(df_res, time_years > 2.5)


# works 
# abs(derived) > 900
fltrd = filter(df_res, (abs(derived) > max(derived)*0.01))
plot(fltrd$time_years, fltrd$daily_infections_sim)

max(fltrd$daily_infections_sim)
scaling_f = max(fltrd[c(1, nrow(fltrd)),2])
fltrd$daily_inf_adj = max(fltrd$daily_infections_sim)*(fltrd$daily_infections_sim - scaling_f) / max(fltrd$daily_infections_sim)
fltrd$daily_inf_adj = ifelse(fltrd$daily_inf_adj >=0, fltrd$daily_inf_adj, 0)
max(fltrd$daily_inf_adj)
min(fltrd$daily_inf_adj)
plot(fltrd$time_years, fltrd$daily_inf_adj)#, ylim=c(0, 1000))

fltrd[c(1, nrow(fltrd)),]

plot(sort(abs(df_res$derived)))
quantile(abs(df_res$derived), 0.5)
max(df_res$derived)*0.01

res[which(inds==T)] %>% plot


#### ordering plots by sim start date 

res = get(load('res/individual_simulations/list_diseaseX_i_simulation_100.RData'))
df_res = map(res, function(.x) .x$sim_res) %>% bind_rows 

df_timing = df_res[,c('timing', 'country')] %>% unique %>% arrange(timing)
df_timing$timing = df_timing$timing/365.25
n_sim = df_res[1,'simulation']
df_res$country = factor(df_res$country, levels=df_timing$country)

ggp = ggplot(df_res, aes(time_years, daily_infections_sim)) + 
    geom_point(aes(color=country), size=1, alpha=0.2) + 
    geom_vline(data=df_timing, aes(xintercept = timing, color=country), linetype='dashed') + 
    facet_wrap(~factor(country, levels=df_timing$country), scales = 'free_y', nrow = 4) + 
    guides(color=guide_legend(override.aes = list(linetype = 0))) + 
    labs(x='Time (years)', y='Simulated Daily Infections', 
            title=paste('Simulation', n_sim, sep=' ')) +
    theme_light() + guides(color='none')

ggsave(plot = ggp, dpi=250, height = 5, width = 7, units = 'in',
        filename = paste('figs/diagnostic_plots/sim_', n_sim, '.png', sep=''))



# run p_spillover.R to obtain the file below 
# from df_burden which contains 205 UN-adj worldpop data per country 
df_burden = read.csv('chikX/data/df_burden_with_pop_size_2015_spillover.csv')

df_burden$mean_incidence = df_burden$infections_mean / df_burden$total_pop_size
df_burden$max_incidence = df_burden$infections_max / df_burden$total_pop_size

### Probability of spillover in each catchment area
# proportional to total prevalence 
n_spillover = sum(df_burden$infections_mean)
### Add column for the proportion of all spillovers occurring in each catchment
df_burden$p_spillover = df_burden$infections_mean/n_spillover

#write.csv(df_burden, 'LassaX/data_chik/df_burden_with_pop_size_2015_spillover.csv')

# load mobility matrix for country codes with mobility data 
mat_mob_p = read.csv("LassaX/data_chik/mat_mob_prob.csv")
all_codes = colnames(mat_mob_p)
rownames(mat_mob_p) = all_codes




# PROFILING CODE 
library(profvis)
profvis({
    list_gravity_spread = f_gravity_model_run(
        .df_catchments = df_burden, 
        .n_spread_matrices = 3, 
        .save_res = F, 
        .dest_dir = 'chikX/data'
    )
})

