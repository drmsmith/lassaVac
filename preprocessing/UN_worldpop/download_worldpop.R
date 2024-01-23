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

# countries 
suit_country_codes <- unique(suit_per_country$country_code)

#################################################################
# download worldpop UN-adjusted data from 2016 (1km aggregated) #
#################################################################

# load downld_worldpop_UNadj1km()
source("preprocessing/UN_worldpop/utils.R")
dest_dir = 'data/2020_UNadj_worlpop_data'
# unlink(dest_dir, recursive = T) # deletes folder and contents 
if (!dir.exists(dest_dir)) dir.create(dest_dir)


# parameters 
v_country_codes = suit_country_codes # vector of strings
s_year = '2020' # string


# will return 1 for successful download and
# iso3c country code for failed downloads
downlds <- map(v_country_codes,
    function(.code) {
        d_status = downld_worldpop_UNadj1km(
            country = .code, year = s_year,
            local_filepath = dest_dir
        )
        if (is.null(d_status)) d_status = .code
        return(d_status)
    },
    .progress = T
) %>% unlist()


downlds[downlds!='1'] %>%
    countrycode(origin = 'iso3c', destination = 'country.name') %>% 
    cat(sep=', ')
# ATF, GGY, CXR, JEY, PCN, SGS
# French Southern Territories, Guernsey, 
# Christmas Island, Jersey, Pitcairn Islands, 
# South Georgia & South Sandwich Islands

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
