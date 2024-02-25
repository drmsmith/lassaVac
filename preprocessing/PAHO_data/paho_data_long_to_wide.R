# preprocessing the raw 2014-2017 PAHO Chikungunya case data
# data downloaded from
# https://ais.paho.org/phip/viz/ed_chikungunya_amro.asp
# (PAHO official website -- chikungynya case records)
# Geogrpahic Spread of Chikungunya in the Americas
# December 2013 - December 2017
# CurvesxCountry
# New Cases of Chikungunya by month of report and country

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


# read in original PAHO data
df_paho_full <- read.csv("preprocessing/data/PAHO_data_raw_wide.csv")
# read in burden data
df_burden <- read.csv("data/df_burden_with_pop_size_2015_spillover.csv")


############################################
# CONNVERT WIDE FORMAT DATA TO LONG FORMAT #
############################################

df_paho_long <- df_paho_full %>%
    reshape2::melt(id = c("Date", "Country", "lab")) %>% # long format data
    na.omit() %>% # lots of NAs droppped
    pivot_wider(names_from = lab, values_from = value) %>% # decouple records and cases
    as.data.frame() %>%
    rename(
        date = Date, country = Country, m.y = variable,
        cases = Cases, records = "Number of Records"
    ) %>%
    filter(records >= 1) # only include cases where records exist

# records = reports recorded?
df_paho_long$records %>% unique() == 1 # sanity check that it equals 1


############################
# TIDY UP LONG FORMAT DATA # 
############################

# convert cases to integers
df_paho_long$cases <- str_replace_all(df_paho_long$cases, ",", "") # remove comma
df_paho_long$cases <- as.integer(df_paho_long$cases)
# convert for working with dates
df_paho_long$date <- mdy(df_paho_long$date)
# fractional data for plotting and param fitting
df_paho_long$date_frac <- popEpi::get.yrs(df_paho_long$date, year.length = "approx")
# get country codes (will throw error about st martin)
# Saint Martin raises error - ignore
df_paho_long$code <- countrycode::countrycode(df_paho_long$country, "country.name", "iso3c")

# finally drop $records and arrange by country and date
df_paho_long <- df_paho_long %>%
    dplyr::select(date, date_frac, m.y, country, code, cases) %>%
    arrange(country, date)


##############################
# SUBSET RELEVANT COUNTRIRES # 
##############################

# all paho countries with available data
paho_countries_all <- unique(df_paho_long$code)
# countries to subset based on burden / mobility
paho_countries_subs <- paho_countries_all[paho_countries_all %in% df_burden$code]

# list of all relevant relevant country dfs (in case easier to check/use)
ls_paho_sub <- purrr::map(
    paho_countries_subs,
    function(.code) filter(df_paho_long, code == .code)
)
names(ls_paho_sub) <- paho_countries_subs
# df of all relevant countries (same as filtered df_paho_long)
df_paho_sub <- ls_paho_sub %>% bind_rows()


####################
# DIAGNOSTIC PLOTS #
####################

if (!interactive()) { # not run when file is sourced 

    # plot one countrY
    ### all good
    # windows() # for window to pop out, vscode
    plot(ls_paho_sub$ARG$date, ls_paho_sub$ARG$cases, type = "b")


    # plot all countries with mobility data
    # windows() # for window to pop out,s vscode
    ggplot(df_paho_sub, aes(date, cases)) +
        geom_line(aes(col = country), alpha = 0.5) +
        geom_point(aes(col = country), size = 1) +
        facet_wrap(vars(country), scales = "free_y") +
        theme_light() +
        theme(legend.position = "none")

}

#################
# SAVE ALL DATA #
#################

res_dir = 'preprocessing/data/PAHO_case_data_2014-2017'
if (!dir.exists(res_dir)) { dir.create(res_dir) }

write.csv(df_paho_long, file = "preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_long_full.csv", row.names = F)
write.csv(df_paho_sub, file = "preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_long_subset.csv", row.names = F)
save(ls_paho_sub, file = "preprocessing/data/PAHO_case_data_2014-2017/ls_PAHO_long_subset.RData")


message('finished running `paho_data_long_to_wide.R`')
