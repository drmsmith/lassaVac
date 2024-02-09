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


###############################
#### GET MEAN SUITABILITY  ####   
#### AND POP WEIGHTED MEAN ####
###############################

# files
## population size
v_pop_files <- list.files("data/2020_UNadj_worldpop_data", pattern = ".tif") %>%
    str_remove("_2020.tif") %>%
    toupper()
## gadm shape files
v_shape_files <- list.files("data/gadm", pattern = ".rds") %>%
    str_remove("gadm41_") %>%
    str_remove("_0_pk.rds") %>%
    toupper()
## shared country codes 
# (some shape files or population files are missing)
v_country_codes <- intersect(v_pop_files, v_shape_files) # 220


# load and suitability map 
suitability <- raster('data/prediction_map_2500CRSMasked.tif')
terra_suit <- terra::rast(suitability)

# cols = colorRampPalette(c('#8ca8d4', '#fbeaa5', '#e78279'))(10)
# plot(suitability, col=cols)

# compute means 
# both mean and population weighted mean
df_suit_means <- map(v_country_codes, function(.ccode) {
    # read in population density and lvl 0 shape file
    pop_fpath <- paste0(
        "data/2020_UNadj_worldpop_data/", tolower(.ccode), "_2020.tif",
        collapse = ""
    )
    shape_fpath <- paste0(
        "data/gadm/gadm41_", toupper(.ccode), "_0_pk.rds",
        collapse = ""
    )
    popdata <- raster(pop_fpath)
    country_sf <- sf::st_as_sf(readRDS(shape_fpath))

    # match resolution of pop data to suitability and align them
    pop_resampled <- resample(popdata, suitability)
    # calculate population-weighted mean
    mean_pop_weighted <- exact_extract(
        terra_suit, country_sf, "weighted_mean",
        weights = pop_resampled,
        default_weight = 0 # NAs in pop_resampled default to 0
        # as i have no other smart way to drop them....
    )[[1]]
    # calculate regular mean
    mean_suit <- exact_extract(
        terra_suit, country_sf, "mean"
    )[[1]]
    # total population 
    pop_size <- exact_extract(
        popdata, country_sf, "sum"
    )[[1]]
    #### note both means are weighted by pixel coverage!!!
    df_suits <- data.frame(
        country_code = .ccode,
        mean_suit = mean_suit,
        mean_pop_weighted = mean_pop_weighted, 
        pop_size = pop_size
    )
}, .progress = TRUE) %>% bind_rows()



# check and save files 
head(df_suit_means)
dim(df_suit_means)
# write.csv(df_suit_means, file='data/df_suit_means_pop_weighted_pop_size.csv', row.names=F)




