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

