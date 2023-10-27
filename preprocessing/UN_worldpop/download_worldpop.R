# download and process UN adjusted worldpop data
# using custom func from utils.R

library("conflicted")
library("tidyverse")
library("countrycode")
library("raster")
library("terra")
conflicts_prefer(
    dplyr::filter(),
    .quiet=T
)


################################################
# select countries whose population to extract #
################################################

# burden estimate data from Salje et al. 
df_burden_estimates <- read.csv("preprocessing/data/df_countries_burden_estimates_cepi.csv")

# keep only countries where transmission is estimated to occur
df_burden_est_sub <- df_burden_estimates %>%
    filter(classification != "NoTransmission") %>%
    dplyr::select(
        country, who_region, classification, infections_mean,
        infections_min, infections_max
    )

# United_States_of_America name isn't mapped onto USA by countrycode()
df_burden_est_sub$country <- ifelse(df_burden_est_sub$country == "United_States_of_America",
    "USA", df_burden_est_sub$country
)

# map country names onto country codes for query
ccodes <- countrycode(df_burden_est_sub$country,
    origin = "country.name", destination = "iso3c"
) %>%
    data.frame(country = df_burden_est_sub$country, code = .)

# adding country code to burden estimate data frame
df_burden_est_sub <- left_join(ccodes, df_burden_est_sub, by = "country")



#################################################################
# download worldpop UN-adjusted data from 2016 (1km aggregated) #
#################################################################

# load downld_worldpop_UNadj1km()
source("preprocessing/UN_worldpop/utils.R")
dir.create("preprocessing/data/2015_UNadj_worlpop_data")

# will return 1 for successful download and
# iso3c country code for failed downloads
downlds <- map(df_burden_est_sub$code,
    function(.x) {
        downld_worldpop_UNadj1km(
            country = .x, year = "2015",
            local_filepath = "preprocessing/data/2015_UNadj_worlpop_data"
        )
    },
    .progress = T
) %>% unlist()

# check for failed downloads 
failed_downloads = downlds[downlds!=1]
if (length(failed_downloads) > 0) {
    message("There are incomplete or failed downloads, attempting to download again.")
    failed_downloads <- map(failed_downloads,
        function(.x) {
            downld_worldpop_UNadj1km(
                country = .x, year = "2015",
                local_filepath = "preprocessing/data/2015_UNadj_worlpop_data"
            )
        },
        .progress = T
    ) %>% unlist()
    failed_downloads <- failed_downloads[failed_downloads != 1]
    if (length(failed_downloads) > 0) {
        message("Failed to download the following twice:\n.")
        cat(paste(failed_downloads, sep = ", "))
        stop("preprocessing/UN_worldpop/worldpop.R: Please examine and fix download issues.")
    }
}

message('finished running `download_worldpop.R`')

########
# if problems persist, an alternative is to 
# download those files manually through the browser,
# using the links wich are printed in error messages
# (but ensure consistent file naming for next steps) 
