library('lubridate')
library('tidyverse')
library('ggplot2')



months = month(1:12, label=TRUE) %>% as.character
month_year = paste('01-', rep(months, times=5), '-',
                   rep(2013:2017, each=12), sep='')[12:(12*5)]
dates = dmy(month_year)

# create dummy data frame for data collection 
df_paho = data.frame(date=dates)

# input values for each month 
results = purrr::map(month_year, function(i){
    readline(cat(i, ':\n')) %>% as.integer
}) %>% unlist

# print results if corrections needed
results %>% cat(sep=', ')

# add to data frame 
df_paho$el_salvador = results

# plot results to check
plot(df_paho$date, df_paho[,7], pch=20, type='b', cex=0.5)


# combine extant data with new input 
paho_2014_2017 = cbind(paho_2014_2017, df_paho[,2:7])
colnames(paho_2014_2017) %>% cat(sep=',')

# countries to extract data from 
# Argentina, Bahamas, Barbados, Belize, Bolivia, Brazil, 
# Chile, Colombia, Costa Rica, Cuba, Dominican Republic, Ecuador, 
#### 16 more to go 
# El Salvador, Guatemala, Guyana, Haiti, Honduras, Jamaica, Mexico, Nicaragua, Panama, Paraguay, Peru, Suriname, Trinidad & Tobago, United States, Uruguay, Venezuela

# mobility data not available for 
# French Guiana, French Polynesia, New Caledonia, Puerto Rico, South Sudan

# write.csv(paho_2014_2017, file='data_thief/paho_2014-2017.csv', row.names=F)

# brazil=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 171, 0, 1922, 806, 1115, 1339, 436, 160, 0, 5276, 400, 2791, 1582, 2506, 0, 2593, 0, 2766, 36251, 28098, 55580, 82228, 87428, 128674, 87428, 20185, 27311, NA, 3754, 2184, 16560, 54095, NA, 50800, 31386, 37183, 0, 0, 0)

# argentina=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 51, 48, 2596, 335, 451, 97, 138, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# bahamas = c(0, 0, 0, 0, 0, 0, 0, 1, 79, 0, 506, 1005, 50, 222, 1, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# bolivia=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 575, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5173, 1725, 0, 8890, 4270, 0, 0, 359, 256, NA, NA, NA, 539, 0, 750, 1789, 63, 64, 30, 37, 14, 72)

# equador = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 63, 147, 3693, 0, 3848, 14041, 1823, 668, 368, 199, 4157, 281, 242, 186, 0, 486, NA, NA, NA, NA, NA, 0, NA, NA, 9, 20, 37, 19, 70, NA, 0, 30, 5, 2, 4)

cntry = read.table('clipboard', sep='\n', header=F)
cntry2 = read.table('clipboard', sep='\n', header=F)

codes_paho = countrycode::countrycode(c(cntry[,1], cntry2[,1], 'Uruguay', 'Venezuela'), 'country.name', 'iso3c') %>% unique

# # all codes 
# AIA, ATG, ARG, ABW, BHS, BRB, BLZ, BMU, BOL, BES, BRA, VGB, CAN, CYM, CHL, COL, CRI, CUB, CUW, DMA, DOM, ECU, SLV, GUF, GRD, GLP, GTM, NA, GUY, HTI, HND, JAM, MTQ, MEX, MSR, NIC, PAN, PRY, PER, PRI, BLM, KNA, LCA, VCT, SXM, SUR, TTO, TCA, USA, VIR, URY, VEN
# # codes in burden 
# ARG, BHS, BRB, BLZ, BOL, BRA, CHL, COL, CRI, CUB, DOM, ECU, SLV, GUF, GTM, GUY, HTI, HND, JAM, MEX, NIC, PAN, PRY, PER, PRI, SUR, TTO, USA, URY, VEN
codes_paho[codes_paho %in% df_burden_with_pop_size_2015$code] %>%
    countrycode::countrycode('iso3c', 'country.name') %>% cat(sep=', ')

df_paho_long = reshape2::melt(data = paho_2014_2017, id='date') %>% 
    rename(cases=value, country=variable)
df_paho_long$country = as.factor(df_paho_long$country)

ggplot(df_paho_long, aes(date, cases)) + 
    geom_line(aes(col=country), alpha=0.5) + 
    geom_point(aes(col=country), size=1) + 
    facet_wrap(vars(country), scales='free_y') + 
    theme_light() + 
    theme(legend.position = 'none')

paho_2014_2017$date_frac = popEpi::get.yrs(paho_2014_2017$date, 
                                           year.length = "approx")








