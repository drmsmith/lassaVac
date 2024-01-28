library('conflicted')
library('tidyverse')
library('RColorBrewer')
conflicts_prefer(
    dplyr::filter(), 
    .quiet=T
)



########################
# UTILS FOR CONVERSION # 
########################
n_days_year <- function(year) {
    year <- as.character(year)
    ndays <- paste0("01-", 1:12, "-", year) %>% # paste year 
        dmy() %>%                               # convert to date
        map(days_in_month) %>%                  # counts all days
        unlist() %>% sum()                      # sum
    return(ndays)
}



#########################
# NUMBER OF DAILY TRIPS # 
#########################


# read in estimates with annual infection numbers
# suitability for now 
df_suit_means <- read.csv('data/df_suit_means_pop_weighted_pop_size.csv')



# mobility matrix = pairwise number of trips 
mat_mobility_2015 <- read.csv('preprocessing/data/mat_mobility_2015.csv')
rownames(mat_mobility_2015) = mat_mobility_2015[,1]

# drop the rownames column 
mat_mob_raw = mat_mobility_2015[,2:ncol(mat_mobility_2015)] %>% as.matrix


#### ANNUAL TRIPS PER PERSON 

data_year = 2015 # !!!! INDICATE THE YEAR OF DATA SET 
n_days = n_days_year(data_year)


mat_mob_daily_trips = mat_mob_raw / n_days
rownames(mat_mob_daily_trips) = mat_mobility_2015[,1]


# replace NA with 0 
mat_mob_daily_trips[is.na(mat_mob_daily_trips)] = 0


#### ADJUSTED DAILY TRIPS PER PERSON (2015)

### since 96% of vals <0.01 and 98.8% <0.05, use fudge factor to increase probability 
# to make quicker (i.e. more computationally feasible) stochastic simulations 
# FUGDE_FACT = 5
# mat_mob_daily_trips = (mat_mob_daily_trips*FUGDE_FACT)/365.25 
# mat_mob_daily_trips = as.matrix(mat_mob_daily_trips)


# this is the mobility matrix which is fed into the 
# spread model 
write.csv(mat_mob_daily_trips, 
          file='data/df_mat_mob_n_daily_trips.csv', row.names = F)




if (!interactive()) { # not run when file is sourced 
    # cepi_cols = read.csv('methods/cepi_color_scheme.csv')
    # create legend and colors 
    n = 10
    # cols = colorRampPalette(brewer.pal(8,"Blues"))(n)
    cols = colorRampPalette(c('#8ca8d4', '#fbeaa5', '#e78279'))(n)
    lgnd = seq(from=min(mat_mob_daily_trips, na.rm = T),
            to=max(mat_mob_daily_trips, na.rm = T), 
            length.out = n+1)
    lgnd = format(lgnd, scientific=F, digits = 2)
    lgnd = paste('(', lgnd[1:n], ' - ', lgnd[2:(n+1)], ']', sep='')

    # plot -- save with custom params manually
    # because the plot is too large to preview in R
    par(mar = c(1, 0, 1, 1))
    mat_mob_daily_trips %>% as.matrix %>% 
        heatmap(.,Colv = NA, Rowv = NA, revC = T, col=cols)
    legend(x='right', legend=lgnd, fill=cols, 
        title='Daily \ntrips', cex=0.9, box.col='white')
}




