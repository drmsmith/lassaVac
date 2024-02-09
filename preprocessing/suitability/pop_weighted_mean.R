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

data_year = 2019
path_start = paste0("data/", data_year, "_UNadj_worldpop_data")
path_end = paste0("_", data_year, ".tif")

# files
## population size
# 2020_UNadj_worldpop_data
v_pop_files <- list.files(path_start, pattern = ".tif") %>%
    str_remove(path_end) %>%
    toupper()
## gadm shape files
v_shape_files <- list.files("data/gadm", pattern = ".rds") %>%
    str_remove("gadm41_") %>%
    str_remove("_0_pk.rds") %>%
    toupper()
## shared country codes 
# (some shape files or population files are missing)
# v_country_codes <- base::intersect(v_pop_files, v_shape_files) # 220
v_country_codes <- map(v_country_codes, function(.ccode) {
    # read in population density and lvl 0 shape file
    pop_fpath <- paste0(
        path_start, "/", tolower(.ccode), path_end,
        collapse = ""
    )
    shape_fpath <- paste0(
        "data/gadm/gadm41_", toupper(.ccode), "_0_pk.rds",
        collapse = ""
    )
    data.frame(
        code = .ccode,
        pop_exists = file.exists(pop_fpath),
        shape_exists = file.exists(shape_fpath)
    )
}) %>% bind_rows %>% # %>% filter(pop_exists == F | shape_exists == F)
     filter(pop_exists == T & shape_exists == T) %>% .$code

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
        path_start, "/", tolower(.ccode), path_end,
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
write.csv(df_suit_means, file='data/2019ppp_df_suit_means_pop_weighted_pop_size.csv', row.names=F)


df_suit_means_old = read.csv('data/df_suit_means_pop_weighted_pop_size.csv')
df_suit_means_old %>% head
df_suit_means %>% head


df_suit_means %>% 
    left_join(
    rename(df_suit_means_old, pop_size_20 = pop_size) %>% select(country_code, pop_size_20), 
    by = join_by(country_code == country_code)
    ) %>%
    mutate(
        pop_diff = pop_size - pop_size_20,
        perc_diff = ((pop_size - pop_size_20)*100) / (pop_size)
    ) %>% dplyr::select(country_code, pop_size, pop_size_20, pop_diff, perc_diff) %>% 
    # head
    filter(country_code=='ITA')




# ls_shapefiles <- purrr::map(
#     list.files("data/gadm", pattern = ".rds", full.names = T), 
#     function(.x) sf::st_as_sf(readRDS(.x)), .progress = T
#     )
# sf_shapefiles = purrr::reduce(ls_shapefiles, sf:::rbind.sf)


# ls_raster = map(
#         list.files("data/2019_UNadj_worldpop_data", pattern = ".tif", full.names = T), 
#         terra::rast
# )
# pops_merged = terra::merge(terra::sprc(ls_raster))

# writeRaster(pops_merged, file='data/2019ppp_UNadj_worldpop.tif')

# typeof(pops_merged)






########################
# USING 2019 TIF FILE # 
########################

# # load and suitability map 
# suitability <- raster('data/prediction_map_2500CRSMasked.tif')
# terra_suit <- terra::rast(suitability)

# pd_2019 <- raster('data/human_population_density_2019.tif')
# # match resolutions 
# pd_2019_resampled <- resample(pd_2019, suitability)


# v_shape_files_codes <- list.files("data/gadm", pattern = ".rds") %>%
#     str_remove("gadm41_") %>%
#     str_remove("_0_pk.rds") %>%
#     toupper()



# # compute means 
# # both mean and population weighted mean
# df_suit_means <- map(v_shape_files_codes, function(.ccode) {
#     # read in population density and lvl 0 shape file
#     shape_fpath <- paste0(
#         "data/gadm/gadm41_", toupper(.ccode), "_0_pk.rds",
#         collapse = ""
#     )
#     country_sf <- sf::st_as_sf(readRDS(shape_fpath))

#     # calculate population-weighted mean
#     mean_pop_weighted <- exact_extract(
#         terra_suit, country_sf, "weighted_mean",
#         weights = pd_2019_resampled,
#         default_weight = 0 # NAs in pop_resampled default to 0
#         # as i have no other smart way to drop them....
#     )[[1]]
#     # calculate regular mean
#     mean_suit <- exact_extract(
#         terra_suit, country_sf, "mean"
#     )[[1]]
#     # total population
#     pop_size <- exact_extract(
#         pd_2019, country_sf,
#         "weighted_sum", weights = "area"
#     )[[1]]
#     #### note both means are weighted by pixel coverage!!!
#     df_suits <- data.frame(
#         country_code = .ccode,
#         mean_suit = mean_suit,
#         mean_pop_weighted = mean_pop_weighted,
#         pop_size = pop_size
#     )
# }, .progress = TRUE) %>% bind_rows()

# head(df_suit_means)
# dim(df_suit_means)
# # write.csv(df_suit_means, file='data/2019_df_suit_means_pop_weighted_pop_size.csv', row.names=F)

# df_suit_means_old = read.csv('data/df_suit_means_pop_weighted_pop_size.csv')

# dim(df_suit_means_old)

# df_suit_means %>% 
#     filter(country_code %in% df_suit_means_old$country_code) %>%
#     mutate(
#         perc_diff = ((pop_size - df_suit_means_old$pop_size)*100) / (pop_size), 
#         pop_adjust = pop_size/1e6, 
#         pop_2020 = df_suit_means_old$pop_size
#     ) %>% dplyr::select(country_code, pop_size,pop_adjust, pop_2020, perc_diff) %>% 
#     # head
#     filter(country_code=='ITA')


# df_suit_means_old$pop_size[df_suit_means_old$country_code=='BGR']



# pd_2019_resampled


##########################
# based on Junwen's code #
##########################


library('tidyverse')

# data from 
# https://population.un.org/wpp/Download/Standard/CSV/
# see indicators for column units 
	# Indicator reference (CSV, 4 KB) --> column info
    # 1950-2100, medium (ZIP, 7.77 MB) --> this file 
un_demo_ind_csv <- read.csv('preprocessing/data/WPP2022_Demographic_Indicators_Medium/WPP2022_Demographic_Indicators_Medium.csv')

dim(un_demo_ind_csv)
# colnames(un_demo_ind_csv) %>% cat(sep=', ')

# SortOrder, LocID, Notes, ISO3_code, ISO2_code, SDMX_code, 
# LocTypeID, LocTypeName, ParentID, Location, VarID, Variant, 
# Time, TPopulation1Jan, TPopulation1July, TPopulationMale1July, 
# TPopulationFemale1July, PopDensity, PopSexRatio, MedianAgePop, 
# NatChange, NatChangeRT, PopChange, PopGrowthRate, DoublingTime, 
# Births, Births1519, CBR, TFR, NRR, MAC, SRB, Deaths, DeathsMale, 
# DeathsFemale, CDR, LEx, LExMale, LExFemale, LE15, LE15Male, 
# LE15Female, LE65, LE65Male, LE65Female, LE80, LE80Male, LE80Female, 
# InfantDeaths, IMR, LBsurvivingAge1, Under5Deaths, Q5, Q0040, Q0040Male, 
# Q0040Female, Q0060, Q0060Male, Q0060Female, Q1550, Q1550Male, Q1550Female, 
# Q1560, Q1560Male, Q1560Female, NetMigrations, CNMR


un_demo_fltrd <- un_demo_ind_csv %>% 
    filter(ISO3_code != '' & Time == 2019) %>% 
            # code      year    pop in thousands                pop density per sq km
    select(ISO3_code, Time, TPopulation1Jan, TPopulation1July, PopDensity) %>% 
    arrange(ISO3_code)

head(un_demo_fltrd)

un_demo_fltrd$TPopulation1Jan[un_demo_fltrd$ISO3_code=='BGR']



# Include package
library("tidyverse")
library("maptools")
library("rgdal")
library("raster")
library("exactextractr")
library("terra")
library("sf")
library("geodata")


# Suitability 2023 across the world
sd_suit <- raster(file.path('data', "prediction_map_2500CRSMasked.tif"))
# Population density 
rst_hpdr <- raster(file.path('data',"human_population_density_2019.tif"))
# Countries over the world
shp_country <- maptools::readShapePoly(file.path('data', "world-administrative-boundaries.shp"))


# Resample pop density to cover the area in suitability
# this takes a few min to run 
t_start <- Sys.time()
rst_suit_hpdr <- resample(rst_hpdr, sd_suit)
t_end <- Sys.time()
print(difftime(t_end, t_start, units = 'mins'))


# Extract suitability data by country
df_suit <- exact_extract(
    sd_suit, shp_country, "mean", append_cols=c('name', 'iso3')
    )



# Extract pop_density by country
dat_pd_by_country <- exact_extract(
    rst_suit_hpdr, shp_country, 'sum', append_cols=c('name', 'iso3')
    )


# Extract pop_density weighted suitability by country
pop_weighted_mean <- exact_extract(
        sd_suit, shp_country, "weighted_mean",
        weights = rst_suit_hpdr,
        default_weight = 0, # NAs in pop_resampled default to 0
        # as i have no other smart way to drop them....
        append_cols=c('name', 'iso3')
    )


df_suits_pop_dens <- left_join(
    df_suit, dat_pd_by_country, by = join_by(name==name, iso3==iso3)
    ) %>% 
    left_join(pop_weighted_mean, by = join_by(name==name, iso3==iso3)) %>% 
    rename(
        suit_mean=mean, pop_density_sum=sum, suit_pd_weightd=weighted_mean, code=iso3
        ) %>% 
    arrange(code) 

# cols:  code, suit_mean, pop_density_sum, suit_pd_weightd

# Summarize suitability for each country
df_country <- df_suits_pop_dens %>% 
  group_by(code) %>% 
  summarize(
    mean_suit = mean(suit_mean, na.rm = T),
    pop_density_sum = sum(pop_density_sum, na.rm=T), 
    suit_pd_weightd = mean(suit_pd_weightd, na.rm=T)
    )


colnames(df_country)
dim(df_country)
head(df_country, n=30) %>% print(n=30)

# write.csv(df_country, 'data/df_suit_pd_weighted.csv', row.names=F)

df_burden <- read.csv('data/2019ppp_df_suit_means_pop_wght_pop_size_who_p_spillover.csv')

df_country$id = "pop_dens_data"
df_burden$id = "pop_count_data"

colnames(df_country)


#df_all =
 left_join(
    df_country, 
    df_burden[,c("country", "code", "pop_size", "mean_suit", "mean_pop_weighted", "annual_incidence", "id")],
    by = join_by(code==code)) %>% head

df_long_suits = bind_rows(
    df_country[,c("code","mean_suit","id")], 
    df_burden[,c("code","mean_suit","id")]) %>% 
    reshape2::melt(id=c('code', 'id'), value.name = 'mean_suit') %>% 
    mutate(code = factor(code)) %>%
    arrange(code) %>% 
    group_by(code) %>% filter( n() > 1 ) %>% 
    dplyr::select(code, id, mean_suit)





df_long_weighted_suits = bind_rows(
    df_country[,c("code","suit_pd_weightd", "id")], 
    df_burden[,c("code","mean_pop_weighted", "id")]) %>% 
    reshape2::melt(id=c('code', 'id'), value.name = 'mean_suit', variable.name='mean_type') %>% 
    mutate(code = factor(code)) %>% drop_na %>%
    arrange(code) %>% 
    group_by(code) %>% filter( n() > 1 ) %>% 
    dplyr::select(code, mean_type, mean_suit)

head(df_long_weighted_suits)




ggplot(df_long_suits, aes(x=mean_suit, y=code)) +
    geom_point(aes(color=id), alpha=0.5) + 
    # facet_wrap(vars(id)) + 
    theme_light(base_size=14) + 
    # guides(color='none') + 
    scale_y_discrete(limits=rev) + 
    labs(x='Mean suitability extracted', y='', title='Suitability means') 

ggsave(filename='figs/suit_means_compare.png', dpi=330, width=3200, height=3600, units='px')

ggplot(df_long_weighted_suits, aes(x=mean_suit, y=code)) +
    geom_point(aes(color=mean_type), alpha=0.5) + 
    # facet_wrap(vars(id)) + 
    theme_light(base_size=14) + 
    # guides(color='none') + 
    scale_y_discrete(limits=rev) + 
    labs(x='Mean suitability extracted', y='', title='Weighed suitability means') 

ggsave(filename='figs/suit_means_wghtd_compare.png', dpi=330, width=3200, height=3600, units='px')




######################
# COMPARE TO UN DATA # 
######################

head(un_demo_fltrd)

df_diffs = left_join(
    df_burden, un_demo_fltrd, 
    by = join_by(code == ISO3_code)) %>% 
    mutate(
        pop_jan = TPopulation1Jan * 1e3, 
        pop_jul = TPopulation1July * 1e3, 
        perc_diff_jan = 100*(pop_size-pop_jan) / pop_size, 
        perc_diff_jul = 100*(pop_size-pop_jul) / pop_size
        ) 

df_diffs %>% dplyr::select(code, pop_size, perc_diff_jan, perc_diff_jul) %>% 
    summarise(
        mean_perc_diff_jan = mean(perc_diff_jan),
        max_diff_jan = max(perc_diff_jan), 
        mean_perc_diff_jul = mean(perc_diff_jul),
        max_diff_jul = max(perc_diff_jul)
        )

df_diffs %>% 
    dplyr::select(code, perc_diff_jan, perc_diff_jul) %>%
    reshape2::melt(
        id='code', value.name='percent_diff', variable.name='dataset'
        ) %>% arrange(code) %>% 
    ggplot(aes(x=percent_diff, y=code, label=code)) +
    geom_point(aes(color=dataset), alpha=0.7) + 
    theme_light(base_size = 14) + 
    scale_y_discrete(limits=rev) + 
    geom_vline(
        xintercept = 0, alpha=0.7,
        color='black', linewidth=1.5) + 
    labs(x='Percent difference between datasets', y='')

ggsave(filename='figs/pop_data_diffs.png', dpi=330, width=3200, height=3600, units='px')


