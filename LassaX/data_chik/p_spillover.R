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
mat_mob_zero = mat_mob_raw[mat_mob_raw==0]
length(mat_mob_nonzero)
length(mat_mob_zero)

sub_cols = colnames(mat_mobility_2015[,2:ncol(mat_mobility_2015)])
df_burden = df_burden_with_pop_size_2015[df_burden_with_pop_size_2015$code %in% sub_cols,]




# ls_mob = lapply(seq_len(nrow(mat_mob)), function(i) {
#     pop = df_burden$total_pop_size[df_burden$code == colnames(mat_mob)[i]]
#     mat_mob[i,] / pop
#     })
# names(ls_mob) = rownames(mat_mob)


mat_mob_prob = map(seq_len(nrow(mat_mob_raw)), function(i) {
    pop = df_burden$total_pop_size[df_burden$code == colnames(mat_mob_raw)[i]]
    # prev = df_burden$prevalence[df_burden$code == colnames(mat_mob_raw)[i]]
    # (mat_mob_raw[i,] / pop)*prev
    (mat_mob_raw[i,] / pop)
}) %>% bind_rows() %>% as.matrix
rownames(mat_mob_prob) = mat_mobility_2015[,1]


#################
par(mar = c(4, 4, 4, 4))
hist(mat_mob_prob, breaks = 100, ylim = c(0,100), 
     main='p (one person A-->B)', xlab='n_trips / pop_size_source')
hist(mat_mob_raw, breaks = 100, ylim = c(0,100))

unravelled = as.vector(mat_mob_prob)
length(unravelled)
nonzeroes = unravelled[unravelled>0] 
length(nonzeroes)
vals = hist(nonzeroes, breaks = 100)
vals$counts[1]
unravelled[which(unravelled<0.05)] %>% hist(breaks=100, ylim=c(0,100))#length
unravelled[which(unravelled<0.01)] %>% length 
# total mat size = 108*108 = 11664

# "BHS-USA" "BRN-MYS" "CYP-TUR" "MLT-ITA" "SGP-MYS" "SWZ-ZAF"
# BHS-USA, BRN-MYS, CYP-TUR, MLT-ITA, SGP-MYS, SWZ-ZAF
# trips between these countries are very likely
map(seq_len(nrow(mat_mob_raw)), function(i) {
    row = mat_mob_prob[i,]
    rown = rownames(mat_mob_prob)[i]
    pairs = colnames(mat_mob_prob)[which(row>1)]
    if (length(pairs>0)) return(paste(rown, pairs, sep='-')) else return(NULL)
    # prev = df_burden$prevalence[df_burden$code == colnames(mat_mob_raw)[i]]
    # (mat_mob_raw[i,] / pop)*prev
}) %>% unlist %>% cat(sep=', ')

rownames(mat_mob_prob)[1]


########################
hist(df_burden$prevalence, breaks = 50, main='Henrik estd prev', xlab='mean_infect / pop_size')

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



### plot -- save with custom params manually
par(mar = c(1, 0, 1, 1))
mat_mob_prob %>% as.matrix %>% 
    heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
legend(x='topright', legend=lgnd, fill=cols, 
       title='Trips per person', cex=0.9, box.col='white')




########################################
# NORMALISED MOB MAT WITH FUDGE FACTOR # 
########################################

mat_mob_prob = map(seq_len(nrow(mat_mob_raw)), function(i) {
    pop = df_burden$total_pop_size[df_burden$code == colnames(mat_mob_raw)[i]]
    # prev = df_burden$prevalence[df_burden$code == colnames(mat_mob_raw)[i]]
    # (mat_mob_raw[i,] / pop)*prev
    (mat_mob_raw[i,] / pop)
}) %>% bind_rows() %>% as.matrix
rownames(mat_mob_prob) = mat_mobility_2015[,1]


# replace NA with 0 for modelling 
mat_mob_prob[is.na(mat_mob_prob)] = 0

### since 96% of vals <0.01 and 98.8% <0.05, use fudge factor to increase probability 
# to make quicker stochastic simulations 
FUGDE_FACT = 5
#mat_mob_prob[which(unravelled<0.05)] = mat_mob_prob[which(unravelled<0.05)]*FUGDE_FACT
mat_mob_prob = mat_mob_prob*FUGDE_FACT/365.25

## normalise matrix to (0-1)
matmin = min(mat_mob_prob)
matmax = max(mat_mob_prob)
mat_mob_prob_norm = (mat_mob_prob - matmin) / (matmax - matmin)
rownames(mat_mob_prob_norm) = mat_mobility_2015[,1]
mat_mob_prob_norm = as.matrix(mat_mob_prob_norm)

# write.csv(mat_mob_prob, file='LassaX/data_chik/mat_mob_prob.csv', row.names = F)
write.csv(mat_mob_prob_norm, file='LassaX/data_chik/mat_mob_prob_fudge.csv', row.names = F)
write.csv(as.matrix(mat_mob_prob), 
          file='LassaX/data_chik/mat_mob_prob_fudge.csv', row.names = F)



par(mar = c(5,5,3,3))
hist(mat_mob_prob_norm, breaks = 100, ylim = c(0,100), 
     main='p_tweaked (one person A-->B)', xlab='n_trips / pop_size_source')


# create legend and colors 
n = 10
cols = colorRampPalette(brewer.pal(8,"Blues"))(n)
lgnd = seq(from=min(mat_mob_prob_norm, na.rm = T),
           to=max(mat_mob_prob_norm, na.rm = T), 
           length.out = n+1)
lgnd = format(lgnd, scientific=F, digits = 2)
lgnd = paste('(', lgnd[1:n], ' - ', lgnd[2:(n+1)], ']', sep='')



### plot -- save with custom params manually
par(mar = c(1, 0, 1, 1))
mat_mob_prob_norm %>% as.matrix %>% 
    heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
legend(x='topright', legend=lgnd, fill=cols, 
       title='Trips per person', cex=0.9, box.col='white')

