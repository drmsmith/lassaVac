


# preview results 
# frequency of number of spreads to other places 
purrr::map(list_gravity_spread, function(.x) {
    spread_sums = apply(.x, 1, function(row) sum(row!=0)) 
    spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% length 
} ) %>% unlist %>% table
#    hist(breaks=15, main='', xlab='n chain countries') # %>% table


all_out_len = purrr::map(list_gravity_spread, function(.x) {
    spread_sums = apply(.x, 1, function(row) sum(row!=0)) 
    spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% length 
} ) %>% unlist %>% table# %>% hist(breaks=15)




spread_sums = purrr::map(list_gravity_spread, function(.x) {
    spread_sums = apply(.x, 1, function(row) sum(row!=0)) 
    spread_sums[spread_sums!=0] %>% sort(decreasing = T) 
} )

map(seq_along(spread_sums), function(i) {
    .sim = spread_sums[[i]] %>% sort(decreasing = T)
    data.frame(t_i = (365*2 - unname(.sim)), country = names(.sim), n_sim = i) 
}) %>% bind_rows %>% 
    ggplot(aes(t_i, country)) + 
    geom_point(aes(color=t_i), alpha=0.75) +
    facet_wrap(~n_sim, nrow = 5) + theme_light()


purrr::map(list_gravity_spread, function(.x) {
    spread_sums = apply(.x, 1, function(row) sum(row!=0)) 
    spread_sums[spread_sums!=0] %>% sort(decreasing = T) %>% length 
} ) %>% unlist %>% 
    hist(breaks=50, main='', xlab='n chain countries') # %>% table


# which countries appear most frequently  
countries_spread = spread_sums %>% unlist %>% names(.) %>% table %>% sort(decreasing = T)
countries_spread
length(countries_spread)
sum(countries_spread)
# 58 unique countries 
# total 228 outbreaks to simulate 
countrycode::countrycode(names(countries_spread), 'iso3c', 'country.name')


# plot the frequency of countries
plot(countries_spread, ylim=c(0,30), srt=270, xlab='')
text(countries_spread+3, labels=names(countries_spread), srt=75, col='red')
