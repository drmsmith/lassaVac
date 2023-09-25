library('tidyverse')
library('countrycode')

df_burden_with_pop_size = read.csv(file = 'LassaX/data_chik/df_burden_with_pop_size.csv')
mobility = read.csv(file = 'LassaX/data_chik/KCMD_EUI_GMP_Estimated_trips.csv')
colnames(mobility) = c('reporting', 'secondary', 'year', 'value')

(all_years = mobility$year %>% unique) # 2011 2012 2013 2014 2015 2016
all_codes = mobility[,1:2] %>% unlist %>% unique %>% sort
# 278 unique codes in reporting and seconrary in every year 
# i.e. all 0s also present

# codes to be removed based on burden estimates
ccodes_burden = df_burden_with_pop_size$code %>% unique
codes_rm = all_codes[!(all_codes %in% ccodes_burden)] 

# # or based on lack of mapping
# country_codes = data.frame(code=all_codes,
#                            country=countrycode(all_codes, origin = 'iso3c',
#                                                destination = 'country.name'))
# codes_rm = country_codes %>% filter(is.na(country)) %>% select(code) %>%
#     unlist %>% unname %>% sort

# mobility data not available for 
# French Guiana, French Polynesia, New Caledonia, Puerto Rico, South Sudan
ccodes_burden[!(ccodes_burden %in% all_codes)] %>%
    countrycode(., origin = 'iso3c', destination = 'country.name') %>% cat(sep=", ")


# separate mobility data by year 
mobility_by_year = map(all_years, function(.x) mobility %>% filter(., year == .x))
names(mobility_by_year) = paste("y", all_years, sep='')

# all years are the same length 
map(mobility_by_year, function(.x) .x$secondary %>% unique %>% length) %>% 
    unlist %>% unique %>% length # check 1 

##################################
# large mobility pairwise matrix # 
##################################
example = mobility_by_year$y2015 %>% select('reporting', 'secondary', 'value') %>% 
    reshape(direction='wide', idvar='reporting', timevar='secondary')

cnames = colnames(example) %>% str_remove_all('value.')
match(cnames[2:length(cnames)], all_codes) %>% is.na %>% sum # check 0 
colnames(example) = cnames


# there are 39160 pairs in each data set 
# and 38124 NAs in the matrix 
apply(example,1, function(.x) sum(is.na(.x[2:length(.x)]))) %>% unname #%>% sum





mat_mobility = mobility_by_year$y2015 %>% select('reporting', 'secondary', 'value') %>% 
    filter(!(reporting %in% codes_rm)) %>% 
    filter(!(secondary %in% codes_rm)) %>%
    arrange(secondary) %>%
    reshape(direction='wide', idvar='reporting', timevar='secondary') %>%
    arrange(reporting)

cnames = colnames(mat_mobility) %>% str_remove_all('value.')
colnames(mat_mobility) = cnames
rownames(mat_mobility) = mat_mobility[,1]

# only 1 NA per row which is for mapping onto self i.e. internal travel 
apply(mat_mobility, 1, function(.x) sum(is.na(.x[2:length(.x)]))) %>% unname %>% sum
# diagonal is all NAs 
mat_mobility[,2:NCOL(mat_mobility)] %>% as.matrix %>% diag %>% is.na %>% sum


# check that rownames and colnames match 
ex_cols = colnames(mat_mobility)
ex_cols = ex_cols[2:length(ex_cols)]
identical(ex_cols, mat_mobility[,1])


###########
# heatmap # 
###########
# create legend and colors 
n = 10
cols = colorRampPalette(brewer.pal(8,"Blues"))(n)
lgnd = seq(from=min(mat_mobility[,2:ncol(mat_mobility)], na.rm = T),
           to=max(mat_mobility[,2:ncol(mat_mobility)], na.rm = T), 
           length.out = n+1)
lgnd = format((lgnd/1e6), scientific=F, digits = 2)
lgnd = paste('(', lgnd[1:n], ' - ', lgnd[2:(n+1)], ']', sep='')

# plot -- save with custom params manually
par(mar = c(1, 0, 1, 1))
mat_mobility[,2:ncol(mat_mobility)] %>% as.matrix %>% 
    heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
legend(x='topright', legend=lgnd, fill=cols, 
       title='Trips (million)', cex=0.9, box.col='white')
# columns in heatmap are secondary country 
# rows are reporting country 

# # extracting population 
# inds = match(country_codes$code, pop_2016$`Country Code`)
# inds = inds[complete.cases(inds)]
# pop_sub = pop_2016[inds,]

# columns = secondary country; rows = reporting country 
write.csv(mat_mobility, file='LassaX/data_chik/mat_mobility_2015.csv', row.names=F)