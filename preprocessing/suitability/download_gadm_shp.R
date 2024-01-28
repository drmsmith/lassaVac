library('maptools')
library('exactextractr')
library('sf')
library('rgdal')
library('raster')
library('geodata')
library('tidyverse')
library('ggplot2')
library('countrycode')
# sf/stars/terra functions using GDAL and PROJ

################################
#### suitability processing #### 
################################


# load and plot suitability map 
suitability <- raster('data/2016_chikv_suitability.tif')
plot(suitability)

# countries 
world_coordinates <- map_data("world") 
all_country_codes = world_coordinates$region %>% unique %>% countrycode(
    origin = 'country.name', destination = 'iso3c'
    ) %>% .[!is.na(.)] %>% unique
# Warning message:
# Some values were not matched unambiguously: 
# Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, 
# Chagos Archipelago, Grenadines, Heard Island, Kosovo, 
# Madeira Islands, Micronesia, Saba, Saint Martin, 
# Siachen Glacier, Sint Eustatius, Virgin Islands

all_country_codes %>% length

# rmfiles = list.files('data/gadm', pattern='.rds') %>% 
#     str_remove_all('_0_pk.rds') %>% str_remove_all('gadm41_')
# all_country_codes = all_country_codes[!(all_country_codes %in% rmfiles)]
# setdiff(all_country_codes, rmfiles) %>% length 




##############################
#### DOWNLOAD SHAPE FILES ####
##############################


# once downloaded, can be rerun 
# takes about 1-2 min 
suit_per_country <- map(all_country_codes, function(.code) {
    # download (to './data') and convert shape file
    country_gadm <- geodata::gadm(country = .code, level = 0, path = "data")
    if (!is.null(country_gadm)) {
        country_sf <- sf::st_as_sf(country_gadm)
        # returns list of length 1 containing
        # df with suitability ($value) and
        # fraction of pixel in country ($coverage_fraction)
        v <- exactextractr::exact_extract(suitability, country_sf)[[1]]
        # excl some edge cases and rm NAs
        v <- filter(v, coverage_fraction > 0.505) %>% drop_na()
        v$country_code <- rep(.code, times = nrow(v))
        return(v)
    } else {
        return(v = data.frame(value = NA, coverage_fraction = NA, country_code = .code))
    }
}, .progress = T) %>% bind_rows()


# check excluded countries but downloaded files 
setdiff(all_country_codes, unique(suit_per_country$country_code)) %>% countrycode(
    origin = 'iso3c', destination = 'country.name'
    ) %>% paste0(collapse=', ') %>% paste0(
        'Pixel coverage insufficient or suitability unavailable: \n', 
        ., '\n', collapse = '') %>% cat
# Pixel coverage insufficient or suitability unavailable: 
# Antarctica, Cocos (Keeling) Islands, Monaco, Maldives, 
# Marshall Islands, Norfolk Island, Vatican City


# check if there are nas in dataset 
dim(suit_per_country)
map(suit_per_country, function(.col) sum(is.na(.col))) %>% unlist

# save file in ./data
# write.csv(suit_per_country, file='data/suit_per_country.csv', row.names = F)

# delete a folder and contained files 
# unlink('data/gadm',recursive = T)



