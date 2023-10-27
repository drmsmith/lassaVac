library('conflicted')
library('tidyverse')
library('RColorBrewer')
conflicts_prefer(
    dplyr::filter(), 
    .quiet=T
)


####################################
# DAILY MOB MAT WITH FUDGE FACTOR # 
####################################


# read in burden estimates with annual infeciton numbers
df_burden_with_pop_size_2015 <- read.csv("data/df_burden_with_pop_size_2015_spillover.csv")

# mobility matrix = pairwise number of trips 
mat_mobility_2015 <- read.csv("preprocessing/data/mat_mobility_2015.csv")
rownames(mat_mobility_2015) = mat_mobility_2015[,1]

# drop the rownames column 
mat_mob_raw = mat_mobility_2015[,2:ncol(mat_mobility_2015)] %>% as.matrix


#### ANNUAL TRIPS PER PERSON 

mat_mob_prob = map(seq_len(nrow(mat_mob_raw)), function(i) {
    pop = df_burden_with_pop_size_2015$total_pop_size[df_burden_with_pop_size_2015$code == colnames(mat_mob_raw)[i]]
    (mat_mob_raw[i,] / pop)
}) %>% bind_rows() %>% as.matrix
rownames(mat_mob_prob) = mat_mobility_2015[,1]


# replace NA with 0 
mat_mob_prob[is.na(mat_mob_prob)] = 0


#### ADJUSTED DAILY TRIPS PER PERSON (2015)

### since 96% of vals <0.01 and 98.8% <0.05, use fudge factor to increase probability 
# to make quicker (i.e. more computationally feasible) stochastic simulations 
FUGDE_FACT = 5
mat_mob_prob = (mat_mob_prob*FUGDE_FACT)/365.25 
mat_mob_prob = as.matrix(mat_mob_prob)

# this is the mobility matrix which is fed into the 
# spread model 
write.csv(mat_mob_prob, 
          file='data/df_mat_mob_prob_fudge.csv', row.names = F)




#################
# HEATMAP PLOT # 
################

if (!interactive()) { # not run when file is sourced 
    # create legend and colors 
    n = 10
    cols = colorRampPalette(brewer.pal(8,"Blues"))(n)
    lgnd = seq(from=min(mat_mob_prob, na.rm = T),
            to=max(mat_mob_prob, na.rm = T), 
            length.out = n+1)
    lgnd = format(lgnd, scientific=F, digits = 2)
    lgnd = paste('(', lgnd[1:n], ' - ', lgnd[2:(n+1)], ']', sep='')

    # plot -- save with custom params manually
    # because the plot is too large to preview in R
    par(mar = c(1, 0, 1, 1))
    mat_mob_prob %>% as.matrix %>% 
        heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
    legend(x='topright', legend=lgnd, fill=cols, 
        title='Adjusted daily \ntrips per person', cex=0.9, box.col='white')
}


message('finished running `mat_mob_prob_fugde.R`')