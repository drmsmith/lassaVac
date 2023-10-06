library('tidyverse')
library('RColorBrewer')

# read in burden estimates with annual infeciton numbers
df_burden_with_pop_size_2015 <- read.csv("LassaX/data_chik/df_burden_with_pop_size_2015.csv")
df_burden_with_pop_size_2015$prevalence = df_burden_with_pop_size_2015$infections_mean/df_burden_with_pop_size_2015$total_pop_size

# mobility matrix = pairwise number of trips 
mat_mobility_2015 <- read.csv("LassaX/data_chik/mat_mobility_2015.csv")
rownames(mat_mobility_2015) = mat_mobility_2015[,1]

mat_mob_raw = as.matrix(mat_mobility_2015[,2:ncol(mat_mobility_2015)])
mat_mob_nonzero = mat_mob_raw[mat_mob_raw!=0]

sub_cols = colnames(mat_mobility_2015[,2:ncol(mat_mobility_2015)])
df_burden = df_burden_with_pop_size_2015[df_burden_with_pop_size_2015$code %in% sub_cols,]




# ls_mob = lapply(seq_len(nrow(mat_mob)), function(i) {
#     pop = df_burden$total_pop_size[df_burden$code == colnames(mat_mob)[i]]
#     mat_mob[i,] / pop
#     })
# names(ls_mob) = rownames(mat_mob)


mat_mob_prob = map(seq_len(nrow(mat_mob_raw)), function(i) {
    pop = df_burden$total_pop_size[df_burden$code == colnames(mat_mob_raw)[i]]
    prev = df_burden$prevalence[df_burden$code == colnames(mat_mob_raw)[i]]
    (mat_mob_raw[i,] / pop)*prev
}) %>% bind_rows() %>% as.matrix

par(mar = c(4, 4, 4, 4))
hist(mat_mob_prob, breaks = 100, ylim = c(0,100))
hist(mat_mob_raw, breaks = 100, ylim = c(0,100))

hist(df_burden$prevalence, breaks = 50)

# 10% of matrix is 0s 
(108^2 - sum(mat_mob_raw==0, na.rm = T)) / 108^2
(108^2 - sum(mat_mob_prob==0, na.rm = T)) / 108^2

mat_mob_prob[mat_mob_prob!=0 & !is.na(mat_mob_prob)] %>% 
    hist(., breaks=100, ylim = c(0,100))
# create legend and colors 
n = 10
cols = colorRampPalette(brewer.pal(8,"Blues"))(n)
lgnd = seq(from=min(mat_mob_prob, na.rm = T),
           to=max(mat_mob_prob, na.rm = T), 
           length.out = n+1)
lgnd = format(lgnd, scientific=F, digits = 2)
lgnd = paste('(', lgnd[1:n], ' - ', lgnd[2:(n+1)], ']', sep='')

# plot -- save with custom params manually
par(mar = c(1, 0, 1, 1))
mat_mob_prob %>% as.matrix %>% 
    heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
legend(x='topright', legend=lgnd, fill=cols, 
       title='Trips per person', cex=0.9, box.col='white')

write.csv(mat_mob_prob, file='LassaX/data_chik/mat_mob_prob.csv', row.names = F)
          