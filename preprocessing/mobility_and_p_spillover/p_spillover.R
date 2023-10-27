library("conflicted")
library('tidyverse')
conflicts_prefer(
    dplyr::filter(),
    .quiet=T
)



################################
# process burden with pop size #
# to obtain p_spillover        # 
################################

df_burden <- read.csv("preprocessing/data/df_burden_with_pop_size_2015.csv")

### Probability of spillover in each catchment area
n_spillover = sum(df_burden$infections_mean)
### Add column for the proportion of all spillovers occurring in each catchment
df_burden$p_spillover = df_burden$infections_mean/n_spillover

### read in the wide-format raw mobility matrix 
mat_mob_p = read.csv("preprocessing/data/mat_mobility_2015.csv")  
all_codes = colnames(mat_mob_p)

# update burden df because no mobility information available for 
# five regions (see mobility_provessing.R)
# French Guiana, French Polynesia, New Caledonia, Puerto Rico, South Sudan
df_burden = df_burden[df_burden$code %in% all_codes,]

write.csv(df_burden, "data/df_burden_with_pop_size_2015_spillover.csv", 
          row.names = F)


message('finished running `p_spillover.R`')