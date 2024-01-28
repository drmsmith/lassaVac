library("conflicted")
library('tidyverse')
library('countrycode')
conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    .quiet=T
)


# read in suitability with pop size and regions
df_burden_with_pop_size = read.csv(file = 'data/df_suit_means_pop_weighted_pop_size.csv')
# read in raw mobility data 
mobility = read.csv(
    file = 'preprocessing/data/KCMD_EUI_GMP_Estimated_trips.csv')
colnames(mobility) = c('reporting', 'secondary', 'year', 'value')


#######################
# raw data eda and    #
# tidy up with burden #
#######################

(all_years = mobility$year %>% unique) # 2011 2012 2013 2014 2015 2016
all_codes = mobility[,1:2] %>% unlist %>% unique %>% sort
# 279 unique codes in reporting and seconrary in every year 
# i.e. all 0s also present

# codes to be removed based on burden estimates
ccodes_burden = df_burden_with_pop_size$country_code %>% unique # 220
codes_rm = all_codes[!(all_codes %in% ccodes_burden)]  # removing 91 codes 
n_codes_remaining = length(all_codes) - length(codes_rm) # 188 for sanity checking

if (!interactive()) { # not run when file is sourced 
    # mobility data not available for 
    # French Guiana, French Polynesia, New Caledonia, Puerto Rico, South Sudan
    ccodes_burden[!(ccodes_burden %in% all_codes)] %>%
        countrycode(., origin = 'iso3c', destination = 'country.name') %>% cat(sep=", ")
}


##################################
# LARGE MOBILITY PAIRWISE MATRIX # 
##################################

# separate mobility data by year 
mobility_by_year = map(all_years, function(.x) mobility %>% filter(., year == .x))
names(mobility_by_year) = paste("y", all_years, sep='')

                # select data from 2015 
mat_mobility = mobility_by_year$y2015 %>% dplyr::select(., 'reporting', 'secondary', 'value') %>% 
    filter(!(reporting %in% codes_rm)) %>% # keep only burden data 
    filter(!(secondary %in% codes_rm)) %>% 
    arrange(secondary) %>% # sort cols 
    reshape(direction='wide', idvar='reporting', timevar='secondary') %>% # reshape into wide format 
    arrange(reporting) # sort rows 

cnames = colnames(mat_mobility) %>% str_remove_all('value.') # tidy up colnames 
colnames(mat_mobility) = cnames 
rownames(mat_mobility) = mat_mobility[,1]

##################
# sanity checks # 
##################

# only 1 NA per row which is for mapping onto self i.e. internal travel 
apply(mat_mobility, 1, function(.x) sum(is.na(.x[2:length(.x)]))) %>% 
    unname %>% sum == n_codes_remaining
# diagonal is all NAs 
mat_mobility[,2:NCOL(mat_mobility)] %>% as.matrix %>% 
    diag %>% is.na %>% sum == n_codes_remaining


# check that rownames and colnames match 
ex_cols = colnames(mat_mobility)
ex_cols = ex_cols[2:length(ex_cols)]
identical(ex_cols, mat_mobility[,1])


# columns = secondary country; rows = reporting country 
write.csv(mat_mobility, file='preprocessing/data/mat_mobility_2015.csv', row.names=F)



###########
# HEATMAP # 
###########
if (!interactive()) { # not run when file is sourced 
    # create legend and colors 
    n = 10
    cols = colorRampPalette(brewer.pal(8,"Blues"))(n)
    lgnd = seq(from=min(mat_mobility[,2:ncol(mat_mobility)], na.rm = T),
            to=max(mat_mobility[,2:ncol(mat_mobility)], na.rm = T), 
            length.out = n+1)
    lgnd = format((lgnd/1e6), scientific=F, digits = 2)
    lgnd = paste('(', lgnd[1:n], ' - ', lgnd[2:(n+1)], ']', sep='')

    # plot -- save with custom params manually
    # because the plot is too large to preview in R
    par(mar = c(1, 0, 1, 1))
    mat_mobility[,2:ncol(mat_mobility)] %>% as.matrix %>% 
        heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
    legend(x='topright', legend=lgnd, fill=cols, 
        title='Trips (million)', cex=0.9, box.col='white')
    # columns in heatmap are secondary country 
    # rows are reporting country 
}

message('finished running `mobility_processing.R`')