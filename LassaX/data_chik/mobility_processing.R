library('tidyverse')
library('countrycode')


mobility = read.csv(file = 'LassaX/data_chik/KCMD_EUI_GMP_Estimated_trips.csv')
colnames(mobility) = c('reporting', 'secondary', 'year', 'value')

(all_years = mobility$year %>% unique) # 2011 2012 2013 2014 2015 2016
all_codes = mobility[,1:2] %>% unlist %>% unique %>% sort
# 278 unique codes in reporting and seconrary in every year 
# i.e. all 0s also present

map(mobility_by_year, function(.x) .x$secondary %>% unique %>% length)

mobility_by_year = map(all_years, function(.x) mobility %>% filter(., year == .x))
names(mobility_by_year) = paste("y", all_years, sep='')


example = mobility_by_year$y2016 %>% select('reporting', 'secondary', 'value') %>% 
    reshape(direction='wide', idvar='reporting', timevar='secondary')

cnames = colnames(example) %>% str_remove_all('value.')
match(cnames[2:length(cnames)], all_codes) %>% is.na %>% sum # check
colnames(example) = cnames


# there are 39160 pairs in each data set 
# and 38124 NAs in the matrix 
apply(example,1, function(.x) sum(is.na(.x[2:length(.x)]))) %>% unname #%>% sum


country_codes = data.frame(code=all_codes, 
                           country=countrycode(all_codes, origin = 'iso3c',
                                               destination = 'country.name'))

codes_rm = country_codes %>% filter(is.na(country)) %>% select(code) %>% 
    unlist %>% unname %>% sort


exmpl_flt = mobility_by_year$y2011 %>% select('reporting', 'secondary', 'value') %>% 
    filter(!(reporting %in% codes_rm)) %>% 
    filter(!(secondary %in% codes_rm)) %>%
    arrange(secondary) %>%
    reshape(direction='wide', idvar='reporting', timevar='secondary') %>%
    arrange(reporting)

cnames = colnames(exmpl_flt) %>% str_remove_all('value.')
colnames(exmpl_flt) = cnames

# only 1 NA which is for mapping onto self 
apply(exmpl_flt, 1, function(.x) sum(is.na(.x[2:length(.x)]))) %>% unname %>% sum
# diagonal is all NAs 
exmpl_flt[,2:197] %>% as.matrix %>% diag %>% is.na %>% sum

countrycode('AGO', origin = 'iso3c', destination = 'country.name')

ex_cols = colnames(exmpl_flt)
ex_cols = ex_cols[2:length(ex_cols)] %>% sort
ex_rows = exmpl_flt[,1] %>% sort
identical(ex_cols, ex_rows)


exmpl_flt[,2:197] %>% as.matrix %>% heatmap(.,Colv = NA, Rowv = NA, revC = T)

# extracting population 
inds = match(country_codes$code, pop_2016$`Country Code`)
inds = inds[complete.cases(inds)]
pop_sub = pop_2016[inds,]


