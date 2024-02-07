library('tidyverse')
library('RColorBrewer')
library('maptools')
library('ggplot2')
library('viridis')
library('rgeos')
library('rworldmap')
library('maps')
library('igraph')
library('countrycode')
library('geosphere')
library('exactextractr')
library('sf')
library('rgdal')
library('raster')
library('geodata')
# sf/stars/terra functions using GDAL and PROJ


# read in df with suitability means from pop_weighted_mean.R
df_suit_means <- read.csv('data/df_suit_means_pop_weighted_pop_size.csv')

 
# check how population weighted is different to mean 
(df_suit_means$mean_suit - df_suit_means$mean_pop_weighted) %>%
    hist(., breaks = 20)

# create dummies for plotting 
df_diag <- df_suit_means %>%
    mutate(diff = mean_suit - mean_pop_weighted) %>%
    mutate(change = mean_suit < mean_pop_weighted)
 
# plot showing difference between 
# mean suit and pop weighted mean suit 
# and relative size of pop weighted mean suit 
ggplot(data=df_diag, mapping=aes(x=1, y=diff, label=country_code)) + 
    geom_text(
        aes(
            x=1, y=diff, color=change, size=mean_pop_weighted
            ), 
        position=position_jitter(), alpha=0.7,
        fontface='bold'
        ) + 
    # geom_jitter(aes(x=1, y=diff, col=change)) + 
    theme_light() + 
    theme(axis.text.x = element_blank()) +
    labs(x='', y='Mean - pop_weighted_mean') + 
    guides(color='none', size=guide_legend(title='Pop. weighted mean'))
    # facet_wrap(~change)



########################
#### PLOT SPILLOVER ####
########################

# first, put all shape files together 
# read in and iteratively grow into single shp file 
shapefiles <-  list.files("data/gadm", pattern = ".rds", full.names = T)
ls_shapefiles <- purrr::map(shapefiles, function(.x) sf::st_as_sf(readRDS(.x)), .progress = T)
sf_shapefiles = purrr::reduce(ls_shapefiles, sf:::rbind.sf)


sf_shapefiles <- read_sf('data/gadm_merged_shp/sf_shapefiles_suit.shp', quiet = TRUE)
# add suitability values as a var 
sf_shapefiles = merge(sf_shapefiles, df_suit_means, by.y='country_code', by.x='GID_0') 
# sf_shapefiles = mutate(sf_shapefiles, mean_suit=df_suit_means$mean_suit[df_suit_means$country_code == GID_0]) 

# save
# write_sf(sf_shapefiles, 'data/sf_shapefiles_suit.shp', quiet = TRUE)


# (!!!) very heavy plotting
# big map plot one by one 
# CHANGE MEAN_SUIT TO MEAN_POP_WEIGHTED 
ggplot(sf_shapefiles) +
    geom_sf(aes(fill = mean_suit), color = "darkgrey", size = 0.1) +
    # scale_fill_viridis() +
    scale_fill_gradient2(
        low = "#547dbf",
        mid = "#fbeaa5",
        high = "#db4437", # can optionally use muted()
        midpoint = 0.5,
        name = 'Mean suitability'
        # name = 'Population-weighted\nmean suitability'
        ) +
    # scale_fill_distiller(palette = "RdYlBu", direction=-1) +
    theme_light() +
    coord_sf()

# make sure to save because viewing this will be impossible 
# ggsave(filename = 'figs/map_mean_suit.png', dpi=500, bg='transparent')
# ggsave(filename = 'figs/map_mean_pop_weighted.png', dpi=500, bg='transparent')




