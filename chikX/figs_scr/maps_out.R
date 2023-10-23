# burden estimates 
df_burden = read.csv('LassaX/data_chik/df_burden_with_pop_size_2015_spillover.csv')


# load mobility matrix for country codes with mobility data 
mat_mob_p = read.csv("LassaX/data_chik/mat_mob_prob.csv")
all_codes = colnames(mat_mob_p)
rownames(mat_mob_p) = all_codes

# update burden df because no mobility information available for 
# five regions (see mobility_provessing.R)
# French Guiana, French Polynesia, New Caledonia, Puerto Rico, South Sudan
df_burden = df_burden[df_burden$code %in% all_codes,]





library('rgeos')
library('rworldmap')
library('maps')
library('igraph')
library('countrycode')
library('geosphere')


# arrows v1 
map("usa")
map.axes()
igraph:::igraph.Arrows(-120, 40, -90, 45, curve=0.3, sh.col="blue")


# get world centroids 
# get world map
wmap <- getMap(resolution="high")
# get centroids
centroids <- gCentroid(wmap, byid=TRUE)

# get a data.frame with centroids
df <- as.data.frame(centroids)
df$code = countrycode(rownames(df), 'country.name', 'iso3c')
df = df[df$code %in% all_codes,]
df$code %>% .[duplicated(.)]
df = df[rownames(df)!='Northern Cyprus',]
df$country = rownames(df)
df[df$code=='CYP',]

all_pairs <- cbind(t(combn(df$x, 2)), t(combn(df$y, 2))) %>% 
    as.data.frame()
colnames(all_pairs) <- c("long1","long2","lat1","lat2")


plot(wmap)
points(df)
points(df, col='red', cex=0.4, pch=20)
#igraph:::igraph.Arrows(-120, 40, -90, 45, curve=0.3, sh.col="blue",open = F)
for(i in 1:nrow(all_pairs)){
    plot_my_connection(all_pairs$long1[i], all_pairs$lat1[i], all_pairs$long2[i], all_pairs$lat2[i], col="skyblue", lwd=1)
}





src_dest = data.frame(country=c('Oman','Pakistan','India'), ti=c(1,388,654))
lag(src_dest)


# will throw a few warnings - ignore
trace_map = function(df_src_dest, centroids=df){
    # create df country+centroid coords
    n_rows = nrow(df_src_dest) 
    scrdest_coords = map2(lag(df_src_dest$country)[2:n_rows], 
                          df_src_dest$country[2:n_rows], function(.x,.y){
        row = cbind(centroids[centroids$country == .x,1:2], 
                    centroids[centroids$country == .y,1:2])
    }) %>% bind_rows %>% unname
    
    colnames(scrdest_coords) <- c("long1","lat1","long2","lat2")
    #scrdest_coords$col = rev(brewer.pal(8, 'RdYlBu')[1:nrow(scrdest_coords)])
    
    
    df[df$country=='Oman',]
    
    # wolrd map data
    wrld = map_data('world')
    world= wrld[wrld$region!='Antarctica',] 

    # plot map with arrows starting at origin and tracing 
    gg = ggplot() + 
        geom_map(data=world, map=world, 
                 aes(map_id=region, long, lat), 
                 fill="gray85", color="#7f7f7f", size=0.25) +
        theme_void() + coord_fixed() + 
        theme(panel.background = element_rect(fill = 'gray95')) + 
        geom_map(data=df_src_dest, map=world, aes(map_id=country, fill=ti),  
                 color="#7f7f7f", size=0.25, alpha=0.75) +
        scale_fill_gradientn(colours = brewer.pal(8, 'RdYlBu')) +
        pmap(scrdest_coords, function(long1, lat1, long2, lat2){#, col){
            geom_curve(
                aes(x = long1, y = lat1, xend = long2, yend = lat2),
                arrow = arrow(
                    length = unit(0.03, "npc"), 
                    type="open" # arrow head (open/closed)
                ), colour = 'black', size = 0.7, angle = 90) # 90 or 0 // colour=col
        })
    return(gg)
}

trace_map(src_dest) %>% ggsave('LassaX/spred_vis/spread_vis.png', plot=.,
                               width = 12, height = 5.5, units='in')



purrr::map(spread_sums, function(.x){
    # create df 
    .src_dest = .x %>% data.frame(code=names(.), ti=(730-.)) %>% 
        mutate(country = countrycode::countrycode(code, 'iso3c', 'country.name')) %>%
        select(country, ti, code)
    .src_dest$country = ifelse(.src_dest$country == 'United States', 
                               'United States of America', .src_dest$country)
    if (nrow(.src_dest) > 1){
        plt_title = paste(.src_dest$code, collapse='-')
        trace_map(.src_dest) %>% 
            ggsave(
                paste('LassaX/spred_vis/', plt_title,'.png', sep=''),
                plot=., width = 12, height = 5.5, units='in')
    }
}, .progress = T)

# usa problem 
# 46 myanmar
src_dest = spread_sums[[46]] %>% data.frame(code=names(.), ti=(730-.)) %>% 
    mutate(country = countrycode::countrycode(code, 'iso3c', 'country.name')) %>%
    select(country, ti, code)

plt_title = paste(src_dest$code, collapse='-')
trace_map(src_dest) #%>% ggsave('LassaX/spred_vis/spread_vis.png', plot=.,
                     #          width = 12, height = 5.5, units='in')
df$country=='United States of America'

library('magick')

imgs <- list.files('LassaX/spred_vis/', full.names = TRUE, pattern='.png')
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated, path = "LassaX/spred_vis/all_outbreaks.gif")



plot(wolrd)
points(df)
points(df, col='red', cex=0.4, pch=20)
#igraph:::igraph.Arrows(-120, 40, -90, 45, curve=0.3, sh.col="blue",open = F)
for(i in 1:nrow(scrdest_coords)){
    plot_my_connection(scrdest_coords$long1[i], scrdest_coords$lat1[i], 
                       scrdest_coords$long2[i], scrdest_coords$lat2[i], col="skyblue", lwd=1)
}
## need to run plot my conneciton function below  



library('tidyverse')
library(RColorBrewer)
library(maptools)
library(ggplot2)

wrld = map_data('world')
world= wrld[wrld$region!='Antarctica',]  

df_col = merge(df, select(df_burden, p_spillover, code), by='code')

df_burden$

df_col$brk <- cut(df_col$infections_mean, 
               breaks=c(0, sort(df_col$p_spillover)), 
               labels=as.character(
                   df_col[order(df_col$p_spillover),]$country),
               include.lowest=TRUE)




pal <- colorRampPalette(brewer.pal(12, 'Spectral'))(length(df_col$p_spillover))
palSz <- 7 # not sure what you really want/need for this range


ggplot() + 
    geom_map(data=world, map=world, 
             aes(map_id=region, long, lat), 
             fill="gray90", color="#7f7f7f", size=0.25) +
    geom_map(data=df_col, map=world, aes(map_id=country, fill=p_spillover),  
             color="white", size=0.25) +
    theme_light() +
    scale_fill_gradientn(colours = brewer.pal(5, 'Reds'))
    # scale_fill_gradient2(low = pal[1],
    #                      mid = pal[palSz/2],
    #                      high = pal[palSz],
    #                      midpoint = (max(df_col$p_spillover) + 
    #                                      min(df_col$p_spillover)) / 2,
    #                      name="P(spillover)")




ghg <- ts(c(12.3, 14.7, 17.3, 13.2, 8.5, 7.7, 6.4, 3.2, 19.8, 
22.3, 24.7, 15.6, 17.4), start=0, end=24, frequency=0.5)
## Have a look at the emission development.
plot(ghg)
## Calculate what has been emitted that day
## Assuming that emissions develop linearly between
## measurements
flux::auc(time(ghg), ghg)
cumsum(ghg) %>% sum

# A function to plot connections
plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
    inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), 
                            n=50, addStartEnd=TRUE, breakAtDateLine=F)             
    inter=data.frame(inter)
    diff_of_lon=abs(dep_lon) + abs(arr_lon)
    if(diff_of_lon > 180){
        lines(subset(inter, lon>=0), ...)
        lines(subset(inter, lon<0), ...)
    }else{
        lines(inter, ...)
    }
}




# load mobility matrix for country codes with mobility data 
mat_mob_p = read.csv("LassaX/data_chik/mat_mob_prob.csv")
all_codes = colnames(mat_mob_p)
rownames(mat_mob_p) = all_codes

mat_mob_p[is.na(mat_mob_p)] = 0
mat_mob_p = (mat_mob_p - min(mat_mob_p)) / (max(mat_mob_p) - min(mat_mob_p))
mat_mob_p$primary = all_codes

mmat_mob_long = mat_mob_p %>% 
    pivot_longer(cols = !primary, 
                 names_to = "secondary", 
                 values_to = "travels") %>%
    mutate(id=paste(primary,secondary,sep='-'))

all_pairs

all_pairs <- cbind(t(combn(df$x, 2)), 
                   t(combn(df$y, 2)), 
                   t(combn(df$code, 2))) %>% 
    as.data.frame()
colnames(all_pairs) <- c("long1","long2","lat1","lat2",
                         'primary', 'secondary')
all_pairs$id = paste(all_pairs$primary,all_pairs$secondary,sep='-')



all_pairs = merge(all_pairs,
                  mmat_mob_long[,c('id','travels')], 
                  by='id', all.x=T)

# only nonzeros 
all_pairs = all_pairs[all_pairs$travels > 0,]

# log transform 
all_pairs$travels = log(all_pairs$travels)
all_pairs$travels = (all_pairs$travels - min(all_pairs$travels)) / (max(all_pairs$travels) - min(all_pairs$travels)) 

hist(all_pairs$travels, 10)



library('plotly')

# an otherwise lovely interactive map which 
# does not allow me to show the travel as n_trips 
# relative to country because the lines are all the same color

geo <- list(
    scope = 'world',
    #projection = list(type = 'azimuthal equal area'),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    countrycolor = toRGB("gray80")
)

fig <- plot_geo(locationmode = 'world', color = I("red"))
fig <- fig %>% add_markers(
    data = df, x = ~x, y = ~y, #text = ~airport,
    alpha = 0.5#,size = ~cnt, hoverinfo = "text",
)
fig <- fig %>% add_segments(
    data = all_pairs,
    x = ~long1, xend = ~long2,
    y = ~lat1, yend = ~lat2,
    alpha = 0.2, size = I(1),#color=~travels,
    hoverinfo = "text"
)
fig <- fig %>% layout(
    title = 'Travel data 2015',
    geo = geo, showlegend = FALSE, height=800
)

fig
