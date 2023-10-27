# extracting relevant worldpop data 
library("conflicted")
library('tidyverse')
library('raster')
conflicts_prefer(
    dplyr::filter(),
    .quiet=T
)

# burden estimate data from Salje et al. 
df_burden_estimates <- read.csv("preprocessing/data/df_countries_burden_estimates_cepi.csv")

###############################################################
# sum across raster to get country-wide population estimates #
###############################################################

# get paths of tiff files 
data_dir = 'preprocessing/data/2015_UNadj_worlpop_data/'
tif_paths <- list.files(data_dir, '.tif', full.names = T) 

# summing and 
# extracting iso3c country code from filename  
# runtime: several seconds 
worldpops <- map(tif_paths,
    function(.x) {
        r <- raster::raster(.x)
        rs <- raster::cellStats(r, "sum")       # change year
        code <- str_remove_all(.x, '_2015.tif') # _ppp_2016_UNadj
        code <- str_remove_all(code, data_dir)
        code <- toupper(code)
        return(data.frame(code=code, total_pop_size=rs))
    },
    .progress = T
) %>% bind_rows


# add pop sizes to estim burden data
df_burden_with_pop_size <- right_join(df_burden_est_sub, worldpops, by = "code")
df_burden_with_pop_size$total_pop_size <- as.double(df_burden_with_pop_size$total_pop_size)


# remember to change year if needed
write.csv(df_burden_with_pop_size,
    row.names = F,
    file = "preprocessing/data/df_burden_with_pop_size_2015.csv"
)

message('finished running `process_worldpop.R`')