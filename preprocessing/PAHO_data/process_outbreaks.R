# process long format data in which outbreak curve
# start and end dates have been marked manually
# into separate outbreak data frames (or list thereof)

library("conflicted")
library("lubridate")
library("tidyverse")
library("ggplot2")
conflicts_prefer(
    dplyr::filter(),
    .quiet=T
)


###################################
# LOAD MARKED DATA -- LONG FORMAT #
###################################

# this section uses the marked long format data
# where outbreak starts and ends are indicated
# in a column called outbreak
# in the df_PAHO_long_subset.csv
#
# therefore, the script below subsets only the data between
# the start and end of every outbreak to be fitted
#
# (there can be multiple outbreak curves to be extracted
# from data coming from the same country
# but they should not overlap)


# original data -- unedited
# df_paho_for_fits = read.csv(file='preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_long_subset_outbreaks.csv')

# data where zeros have been added for clarity
# peaks where fitting fails are removed
# and zeros are added for better fitting results
df_paho_for_fits <- read.csv(file = "preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_long_subset_outbreaks_with_zeros.csv")

df_paho_for_fits$country <- factor(df_paho_for_fits$country)
df_paho_for_fits$date <- dmy(df_paho_for_fits$date)


# tidy up data -- NAs for missing string
df_paho_for_fits$outbreak <- ifelse(df_paho_for_fits$outbreak == "", NA, df_paho_for_fits$outbreak)
# subset relevant data
start_ends <- df_paho_for_fits %>% na.omit()
# take all countries which appear 
# and will be looped over   
all_countries <- start_ends$country %>%
    unique() %>%
    as.character()

# create a list of outbreaks
# where every entry is the subset
# of the long data ranging from start to end
                    # initally loop over all countries 
l_outbreaks <- map(all_countries, function(.country) {
    all_starts <- start_ends %>% # take all start dates
        filter(country == .country & outbreak == "s") %>%
        .$date
    all_ends <- start_ends %>%   # take all end dates
        filter(country == .country & outbreak == "e") %>%
        .$date
    country_sub <- df_paho_for_fits %>% filter(country == .country)
    # now loop over all start-ends in a given country 
    outs <- map2(all_starts, all_ends, function(.s, .e) {
        # to subset the data between the start and end 
        out <- country_sub %>% filter(date >= .s & date <= .e)
    })
}) %>% unlist(recursive = F) # unnest nested lists 

# convert to df for csv
df_outbreaks <- l_outbreaks %>% bind_rows()


## if the code above throws errors (map2 in particular)
## then there must be an odd number of 's' or 'e' marks


##################
# SAVE OUTBREAKS # 
##################

# # original data
# write.csv(df_outbreaks, file='preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_outbreaks.csv', row.names=F)
# save(l_outbreaks, file='preprocessing/data/PAHO_case_data_2014-2017/ls_PAHO_outbreaks.RData')
#
# # modified data
write.csv(df_outbreaks, file = 'preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_outbreaks_manual_adj.csv', row.names = F)
save(l_outbreaks, file = 'preprocessing/data/PAHO_case_data_2014-2017/ls_PAHO_outbreaks_manual_adj.RData')


message('finished running `process_outbreaks.R`')