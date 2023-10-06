library('tidyverse')
library('ggplot2')
library('lubridate')

# read in original PAHO data 
df_paho_full <- read.csv("data_thief/PAHO_data_copy.csv")

df_paho_long = df_paho_full %>%
    reshape2::melt(id=c('Date', 'Country', 'lab')) %>%  # long format data 
    na.omit() %>%                                       # lots of NAs droppped
    pivot_wider(names_from = lab, values_from = value) %>%  # decouple records and cases
    as.data.frame %>%
    rename(date=Date, country=Country, m.y = variable, 
           cases=Cases, records="Number of Records") %>% 
    filter(records>=1)      # only include cases where records exist 

df_paho_long$records %>% unique # sanity check that it equals 1 


df_paho_long$cases = str_replace_all(df_paho_long$cases, ',','') # for int conversion 
df_paho_long$cases = as.integer(df_paho_long$cases)
# convert for working with dates
df_paho_long$date = mdy(df_paho_long$date)
# fractional data for plotting and param fitting 
df_paho_long$date_frac = popEpi::get.yrs(df_paho_long$date, year.length = "approx")
# get country codes (will throw error about st martin)
df_paho_long$code = countrycode::countrycode(df_paho_long$country, 'country.name', 'iso3c')

# finally drop $records and arrange by country and date 
df_paho_long = df_paho_long %>% 
    select(date, date_frac, m.y, country, code, cases) %>%
    arrange(country, date)


### 108 countries with mobility info 
mob_info = c("AGO", "ARG", "BDI", "BEN", "BFA", "BGD", "BHS", "BLZ", "BOL", "BRA", "BRB",
             "BRN", "BTN", "BWA", "CAF", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL",
             "COM", "CPV", "CRI", "CUB", "CYP", "DJI", "DOM", "ECU", "ERI", "ETH", "FJI",
             "FRA", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "GTM", "GUY", "HND", "HRV",
             "HTI", "IDN", "IND", "IRN", "ISR", "ITA", "JAM", "JPN", "KEN", "KHM", "KOR",
             "LAO", "LBR", "LKA", "MDG", "MDV", "MEX", "MLI", "MLT", "MMR", "MOZ", "MRT",
             "MUS", "MWI", "MYS", "NAM", "NER", "NGA", "NIC", "NPL", "OMN", "PAK", "PAN",
             "PER", "PHL", "PNG", "PRY", "QAT", "RWA", "SAU", "SDN", "SEN", "SGP", "SLE",
             "SLV", "SOM", "SUR", "SWZ", "TCD", "TGO", "THA", "TLS", "TTO", "TUR", "TZA",
             "UGA", "URY", "USA", "VEN", "VNM", "YEM", "ZAF", "ZMB", "ZWE")
# all paho countries
all_countries = unique(df_paho_long$code)
# countries to subset 
country_subs = all_countries[all_countries %in% mob_info]

# list of all relevant relevant country dfs (in case easier to check/use)
ls_paho_sub = purrr::map(country_subs,
                         function(.code) filter(df_paho_long, code==.code))
names(ls_paho_sub) = country_subs
# df of all relevant countries (same as filtered df_paho_long)
df_paho_sub = ls_paho_sub %>% bind_rows()

# plot one to check 
### all good 
plot(ls_paho_sub$ARG$date, ls_paho_sub$ARG$cases, type='b')

# plot all countries with mobility data 
ggplot(df_paho_sub, aes(date, cases)) + 
    geom_line(aes(col=country), alpha=0.5) + 
    geom_point(aes(col=country), size=1) + 
    facet_wrap(vars(country), scales='free_y') + 
    theme_light() + 
    theme(legend.position = 'none')

# write.csv(df_paho_long, file='data_thief/PAHO_long.csv', row.names=F)
# write.csv(df_paho_sub, file='data_thief/PAHO_long_sub.csv', row.names=F)
# save(ls_paho_sub, file='data_thief/ls_PAHO_long_sub.RData')

###################
# LOAD READY DATA # 
###################

## code above obtains files below 

ls_paho_sub = get(load("data_thief/ls_PAHO_long_sub.RData"))
df_paho_sub = read.csv(file="data_thief/PAHO_long_sub.csv")
df_paho_sub$country = factor(df_paho_sub$country)
df_paho_sub$date = as.Date(df_paho_sub$date)

df_burden_with_pop_size_2015 <- read.csv("LassaX/data_chik/df_burden_with_pop_size_2015.csv")

country_subs = df_paho_sub$code %>% unique


#############################
# GET PER CAPITA INCIDENCE # 
#############################

### load the saved ones 
ls_paho_per_capita = get(load("data_thief/ls_PAHO_per_capita.RData"))
df_paho_per_capita = read.csv(file="data_thief/PAHO_long_per_capita.csv")
df_paho_per_capita$country = factor(df_paho_per_capita$country)
df_paho_per_capita$date = as.Date(df_paho_per_capita$date)

## code below obtains files above 

# obtain incidence per capita 
# using the total population size 
# NOT effective population size 
ls_paho_per_capita = purrr::map(
    country_subs,
    function(.code) {
        .data = filter(df_paho_sub, code==.code)
        pop = df_burden_with_pop_size_2015$total_pop_size[df_burden_with_pop_size_2015$code=='ARG']
        .data$inc_per_capita = .data$cases / pop 
        return(.data)
        })
names(ls_paho_per_capita) = country_subs
# df of all relevant countries (same as filtered df_paho_long)
df_paho_per_capita = ls_paho_per_capita %>% bind_rows()


# plot per capita incidence for all countries  
ggplot(df_paho_per_capita, aes(date, inc_per_capita)) + 
    geom_line(aes(col=country), alpha=0.5) + 
    geom_point(aes(col=country), size=1) + 
    facet_wrap(vars(country), scales='free_y') + 
    theme_light() + 
    theme(legend.position = 'none')


# write.csv(df_paho_per_capita, file='data_thief/PAHO_long_per_capita.csv', row.names=F)
# save(ls_paho_per_capita, file='data_thief/ls_PAHO_per_capita.RData')
