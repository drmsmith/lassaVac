library("conflicted")
library("tidyverse")
library("ggplot2")
library("lubridate")
library("popEpi")
library("countrycode")
conflicts_prefer(
    dplyr::filter(),
    dplyr::select(),
    .quiet=T
)


##########################################
#### SELECT PAHO DATA FOR SIMULATIONS ####
##########################################


df_paho_long <- read.csv("preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_long_full.csv")

head(df_paho_long)
# !!!!! there is a suspicious NA in this list???? 
countries = df_paho_long$code %>% unique
# Saint Martin maps onto NA, outbreak plot below 
# filter(df_paho_long, is.na(code)) %>% select(date_frac, cases)  %>% plot


# df_percentage_coverage = map(countries, function(.country) {
#     df_country = filter(df_paho_long, code == .country)
#     quants = sum(sort(df_country$cases) < 1) / nrow(df_country)
#     names(quants) = .country
#     return(quants)
# }) %>% unlist %>% as.data.frame

# names(df_percentage_coverage) = c('perc_below_1')

# mutate(df_percentage_coverage, threshold_40 = perc_below_1 < 0.4) %>%
#     filter(threshold_40 == T)


# excluding any place with fewer than 100 peak cases, 
# super abrupt and strange data e.g. super short outbreaks,
# outbreaks with total cases < 500  
codes_exclude = c(
    'ABW', "AIA", "ARG", "BES", "BLM", "BLZ", "BMU", "CAN", "CUW", "CYM", "GLP",
    "GUY", "HTI", "MSR", "MTQ", "USA", "VCT", "VGB", NA, "CHL", "CUB", "TCA", "URY"
)

df_paho_subs = df_paho_long %>% filter(!code %in% codes_exclude) %>% drop_na()


# plot paho monthly cases  
ggplot(df_paho_subs, aes(x=date_frac, y=cases)) +
    geom_point(aes(col = country), size = 0.5) +
    geom_line(aes(col = country)) +
    # geom_histogram(aes(fill = country)) +
    facet_wrap(vars(code), scales = "free_y") + # country or code
    theme_light() +
    theme(legend.position = "none") + 
    labs(x='Date', y = 'Monthly cases')

ggsave(
    filename = 'figs/paho_cases.png', dpi=330, bg='transparent',
    width = 10, height = 7, units = 'in'
)


######################
# get outbreak sizes #
######################

df_outbreak_sizes = map(
    unique(df_paho_subs$code), 
    function(.ccode) {
        data.frame(
            code = .ccode, 
            total_cases = filter(df_paho_subs, code == .ccode) %>% select(cases) %>% sum
            )
        }) %>% bind_rows 

# save
# write.csv(df_outbreak_sizes, 'data/df_paho_outbreak_sizes.csv', row.names=F)

# plot outbreak sizes 
ggplot(df_outbreak_sizes, aes(x=1, y=total_cases, label=code)) + 
    geom_text(
        aes(
            x=1, y=log(total_cases), size=log(total_cases)
            ), 
        position=position_jitter(), alpha=0.7,
        fontface='bold'
        ) + 
    # geom_jitter(aes(x=1, y=diff, col=change)) + 
    theme_light() + 
    theme(axis.text.x = element_blank()) +
    labs(x='', y='Outbreak size (log)') + 
    guides(color='none', size='none')

ggsave(
    filename = 'figs/paho_oubtreak_sizes.png', dpi=330, bg='transparent',
    width = 10, height = 7, units = 'in'
)





##################################
#### get n cases on first day ####
##################################

# helper for date format 
m.y_func <- function(m.y) {
    m <- month(m.y, label = T, abbr = T)
    y <- year(m.y) %>% as.character %>% str_sub(3,4)
    m.y_string = paste(m, y, sep='.')
    return(m.y_string)
}
# vectorise for use in map()
## note this is not for speed but logic 
my_func_vec = Vectorize(m.y_func)

# calculate the percentage of total outbreak 
# made up by monthly cases 
percentage_first_report <- df_paho_subs %>% 
    filter(cases > 1) %>%
    group_by(country, code) %>% 
    mutate(percentage = 100*prop.table(cases)) %>%
    filter(date == first(date)) %>% 
    arrange(desc(percentage)) %>%
    left_join(df_outbreak_sizes, by = join_by(code==code))
#    summarise(cases=cases, start_date = first(date))

# add a month of having one case to outbreaks where 
# the first non-zero case is more than 3% of the total outbreak
added_low_cases <- percentage_first_report %>% 
    ungroup() %>% 
    mutate(
        date = case_when(
            percentage > 3 ~ floor_date(as.Date(date), 'month') - months(1)
        ), 
        # cases = case_when(percentage > 3 ~ 0.03*total_cases)
        cases = case_when(percentage > 3 ~ 1)
    ) %>% drop_na %>% 
    mutate(
        date_frac = decimal_date(date),
        m.y = my_func_vec(date)
        ) %>%
    select(date, date_frac, m.y, country, code, cases)

# percentage_first_report %>% filter(percentage > 3) %>% select(code)


df_added_lag_cases <- bind_rows(
    added_low_cases, mutate(df_paho_subs, date = ymd(date))) %>% 
    arrange(country, date) 


##########################
# convert to daily cases #
########################## 

df_paho_daily_cases <- map(unique(df_added_lag_cases$code), function(.ccode) {
    # going by country
    df_country <- filter(df_added_lag_cases, code == .ccode)
    # filter trailing zeros
    first_nonzero <- which(df_country$cases >= 1)[1]
    ## introduce lag, potentially
    # first_nonzero = ifelse(first_nonzero > 1, first_nonzero-1, first_nonzero)
    subdf <- df_country[first_nonzero:nrow(df_country), ]
    # convert to daily cases
    subdf_long <- pmap(subdf, function(date, date_frac, m.y, country, code, cases) {
        n_days <- lubridate::days_in_month(date)
        daily_cases <- as.integer(cases) / n_days
        daily_df <- data.frame(daily_cases = rep(daily_cases, times = n_days)) %>%
            mutate(
                date = dmy(paste(1:n_days, m.y, sep = ".")),
                country = country,
                code = code
            )
        return(daily_df)
    }) %>% bind_rows()
    return(subdf_long)
}, .progress = T) %>% bind_rows() %>%
    # exclude countries with high reporting bias
    filter(!code %in% c('JAM','COL','SUR','BHS'))


# save
# write.csv(df_paho_daily_cases, 'data/df_paho_daily_cases.csv', row.names=F)
# write.csv(df_paho_daily_cases, 'data/df_paho_daily_cases_lagged.csv', row.names=F)
# write.csv(df_paho_daily_cases, 'data/df_paho_daily_cases_fltrd.csv', row.names=F)

# df_paho_daily_cases$daily_cases  %>% cumsum %>% plot


# plot daily cases 
ggplot(df_paho_daily_cases, aes(x=decimal_date(date), y=daily_cases)) +
    geom_point(aes(col = country), size = 0.5) +
    geom_line(aes(col = country)) +
    # geom_histogram(aes(fill = country)) +
    facet_wrap(vars(code), scales = "free_y") + # country or code
    theme_light() +
    theme(legend.position = "none") + 
    labs(x='Date', y='Daily cases')

ggsave(
    # filename = 'figs/paho_cases_daily.png', dpi=330, bg='transparent',
    # filename = 'figs/paho_cases_daily_LAG.png', dpi=330, bg='transparent',
    filename = 'figs/paho_cases_daily_ftlrd.png', dpi=330, bg='transparent',
    width = 10, height = 7, units = 'in'
)







if (!interactive()) { # not run when file is sourced 



}





df_paho_long <- read.csv("preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_long_full.csv")


# outbreaks with total cases < 500  
codes_exclude = c(
    'ABW', "AIA", "ARG", "BES", "BLM", "BLZ", "BMU", "CAN", "CUW", "CYM", "GLP",
    "GUY", "HTI", "MSR", "MTQ", "USA", "VCT", "VGB", NA, "CHL", "CUB", "TCA", "URY",
    "COL", "JAM", "BHS", "SUR"
)

df_paho_subs = df_paho_long %>% filter(!code %in% codes_exclude) %>% drop_na()


# helper for date format 
m.y_func <- function(m.y) {
    m <- month(m.y, label = T, abbr = T)
    y <- year(m.y) %>% as.character %>% str_sub(3,4)
    m.y_string = paste(m, y, sep='.')
    return(m.y_string)
}
# vectorise for use in map()
## note this is not for speed but logic 
my_func_vec = Vectorize(m.y_func)

percentage_first_report <- df_paho_subs %>% 
    filter(cases > 1) %>%
    group_by(country, code) %>% 
    mutate(percentage = 100*prop.table(cases)) %>%
    filter(date == first(date))

# the first non-zero case is more than 3% of the total outbreak
added_low_cases <- percentage_first_report %>% 
    ungroup() %>% 
    mutate(
        date = floor_date(as.Date(date), 'month') - months(1),
        cases = lubridate::days_in_month(date)
    ) %>% drop_na %>% 
    mutate(
        date_frac = decimal_date(date),
        m.y = my_func_vec(date)
        ) %>%
    select(date, date_frac, m.y, country, code, cases)

# percentage_first_report %>% filter(percentage > 3) %>% select(code)


df_added_lag_cases <- bind_rows(
    added_low_cases, mutate(df_paho_subs, date = ymd(date))) %>% 
    arrange(country, date) 




df_paho_daily_cases <- map(unique(df_added_lag_cases$code), function(.ccode) {
    # going by country
    df_country <- filter(df_added_lag_cases, code == .ccode)
    # filter trailing zeros
    first_nonzero <- which(df_country$cases >= 1)[1]
    ## introduce lag, potentially
    # first_nonzero = ifelse(first_nonzero > 1, first_nonzero-1, first_nonzero)
    subdf <- df_country[first_nonzero:nrow(df_country), ]
    # convert to daily cases
    subdf_long <- pmap(subdf, function(date, date_frac, m.y, country, code, cases) {
        n_days <- lubridate::days_in_month(date)
        daily_cases <- as.integer(cases) / n_days
        daily_df <- data.frame(daily_cases = rep(daily_cases, times = n_days)) %>%
            mutate(
                date = dmy(paste(1:n_days, m.y, sep = ".")),
                country = country,
                code = code
            )
        return(daily_df)
    }) %>% bind_rows()
    return(subdf_long)
}, .progress = T) %>% bind_rows() 


# plot daily cases 
# df_daily_cases_smooth
ggplot(df_daily_cases_smooth, aes(x=decimal_date(date), y=daily_cases)) +
    geom_point(aes(col = country), size = 0.3) +
    geom_line(aes(col = country), linewidth=0.5) +
    # geom_histogram(aes(fill = country)) +
    facet_wrap(vars(code), scales = "free_y") + # country or code
    theme_light() +
    theme(legend.position = "none") + 
    labs(x='Date', y='Daily cases')

ggsave(
    # filename = 'figs/paho_cases_daily.png', dpi=330, bg='transparent',
    # filename = 'figs/paho_cases_daily_LAG.png', dpi=330, bg='transparent',
    filename = 'figs/paho_cases_daily_ftlrd_lagged.png', dpi=330, bg='transparent',
    width = 10, height = 7, units = 'in'
)


library(zoo)
head(df_paho_daily_cases)




##### this chunk is enough to smoot h
df_daily_cases_smooth <- df_paho_daily_cases %>%
    group_by(country, code) %>%
    mutate_at(
        vars("daily_cases"), 
        list(daily_cases = ~ rollmeanr(., k = 30, fill = 1))
        ) %>%
    ungroup 


# save
# write.csv(df_paho_daily_cases, 'data/df_paho_daily_cases.csv', row.names=F)
# write.csv(df_paho_daily_cases, 'data/df_paho_daily_cases_lagged.csv', row.names=F)
# write.csv(df_paho_daily_cases, 'data/df_paho_daily_cases_fltrd.csv', row.names=F)
write.csv(df_daily_cases_smooth, 'data/df_paho_daily_cases_fltrd_lagged_smooth.csv', row.names=F)

# plot daily cases 
ggplot(df_daily_cases_smooth, aes(x=decimal_date(date), y=daily_cases)) +
    geom_point(aes(col = country), size = 0.3) +
    geom_line(aes(col = country), linewidth=0.5) +
    # geom_histogram(aes(fill = country)) +
    facet_wrap(vars(code), scales = "free_y") + # country or code
    theme_light() +
    theme(legend.position = "none") + 
    labs(x='Date', y='Daily cases')

ggsave(
    # filename = 'figs/paho_cases_daily.png', dpi=330, bg='transparent',
    # filename = 'figs/paho_cases_daily_LAG.png', dpi=330, bg='transparent',
    filename = 'figs/paho_cases_daily_roll30.png', dpi=330, bg='transparent',
    width = 10, height = 7, units = 'in'
)

