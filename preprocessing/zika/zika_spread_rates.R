library(tidyverse)


zika_phyl_meta = read.table('preprocessing/data/zika/nextstrain_zika_metadata.tsv', header=T, sep='\t') # _2014_2016

dim(zika_phyl_meta)
colnames(zika_phyl_meta)
head(zika_phyl_meta)

df_filter_by_date = zika_phyl_meta %>% 
    filter(date > '2014-06-01' & date < '2016-12-31') %>% 
    arrange(date) 

df_uniq_countries = df_filter_by_date %>% 
    select(country, region) %>% 
    unique %>% arrange(region)

dim(df_uniq_countries)
cat(df_uniq_countries$country, sep='\n')

# There are 37 countries between mid 2014 and end 2016
# aim is to get p(introduction) = f*suitability^k
# to yield simulations with these kinds of numbers 

# table(df_uniq_countries$region) %>% print
# Africa          1
# Asia            6
# North America   16
# Oceania         3
# South America   9
# Southeast Asia  2


# df_countries = zika_phyl_meta[,c('country', 'region')] %>% unique %>% arrange(region)
# dim(df_countries)

colnames(df_filter_by_date)

# scaleFUN <- function(x) sprintf("%.0f", x)
# scale_x_continuous(labels=scaleFUN)
#### or 
# scale_y_continuous(
#   labels = scales::number_format(accuracy = 0.01,
#                                  decimal.mark = ','))

zika_phyl_meta %>%
    filter(date > "2014-06-01" & date < "2016-12-31") %>%
    group_by(country) %>%
    arrange(date) %>%
    summarise(date = first(date)) %>%
    add_count(country) %>%
    arrange(date) %>%
    mutate(
        cumul_nspread = cumsum(n),
        country = factor(country, levels = country),
        date_frac = lubridate::decimal_date(as.Date(date)),
        date_norm = date_frac - min(date_frac)
    ) %>% arrange(date) 



df_filter_by_date %>%
    group_by(country) %>%
    summarise(date = first(date)) %>%
    arrange(date) %>%
    mutate(
        country = factor(country, levels = country),
        date_frac = lubridate::decimal_date(as.Date(date))
    ) %>% print(n=35)


    ggplot(aes(date_frac, country)) +
    geom_point(aes(col = country)) +
    guides(color = "none") +
    labs(x = "Year", y = "") +
    theme_light(base_size = 18) +
    scale_color_manual(
        values = make_cepi_base_col_scheme(
            c("#547dbf", "#db4437", "#682860"), 
            length(unique((df_filter_by_date$country)))
            )
        # name = ''
    )

df_filter_by_date %>%
    group_by(country) %>%
    summarise(date = first(date)) %>%
    arrange(date) %>%
    mutate(
        country = factor(country, levels = country),
        date_frac = lubridate::decimal_date(as.Date(date))
    ) %>%
    ggplot(aes(
        date_frac, country,labels=country, 
        fontface='bold', size=22/.pt
        )) +
    geom_text(aes(label=country, col = country)) +
    guides(color = "none", size='none') +
    labs(x = "Year", y = "") +
    theme_light(base_size = 22) +
    scale_color_manual(
        values = make_cepi_base_col_scheme(
            c("#547dbf", "#db4437", "#682860"), 
            length(unique((df_filter_by_date$country)))
            )
    ) + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
    xlim(2013.9, 2017)

ggsave(
    filename = 'figs/zika_spread.png', 
    dpi=330, width=2500, height=2000, unit='px', bg='transparent'
)




make_cepi_base_col_scheme <- function(.base_cols, .out) {
    colscheme <- colorRampPalette(.base_cols)(.out) %>%
        unlist() %>%
        unname()
    return(colscheme)
}




### spread against baseline 

spread_cumul_timing <- df_full_summary %>% group_by(simulation) %>% 
    add_count(simulation, code) %>% 
    mutate(
        cumul_nspread = cumsum(n),
        simulation = factor(simulation) 
    )
    # select(
    #     simulation, country, code, region_code, region_name, 
    #     outbreak_start_day, n, cumul_nspread, 
    #     total_infections_all_years, years_1_2) #%>% head


### colour by region code 
df_colrs_region = data.frame(
    region_color = c('#E78279', '#FFC354', '#8C5B00', '#8CA8D4', '#9A6F94', '#BD5E8D'), 
    region_code = sort(unique(spread_cumul_timing_col_region$region_col))
    )


spread_cumul_timing_col_region <- spread_cumul_timing %>% 
    left_join( # create dummy var by last region 
        summarise(spread_cumul_timing, region_col = last(region_code)),
        by=join_by(simulation==simulation)) %>%
    select(simulation, code, region_code, outbreak_start_day, n, cumul_nspread, region_col) %>%
    left_join(df_colrs_region, by=join_by(region_code==region_code))






df_zika_cumul <- zika_phyl_meta %>% 
    filter(date > '2014-06-01' & date < '2016-12-31') %>% 
    group_by(country) %>%
    summarise(date = first(date)) %>%
    arrange(date) %>%
    add_count(country) %>%
    mutate(
        cumul_nspread = cumsum(n), 
        country = factor(country, levels = country),
        date_frac = lubridate::decimal_date(as.Date(date)), 
        date_norm = date_frac - min(date_frac)
    )

min(df_zika_cumul$date)


df_cepi_cols <- read.csv('methods/misc/cepi_color_scheme.csv')
df_cepi_cols$light_1


cepi_prim_cols <- c(
    "#547dbf", "#ffa500", "#db4437", "#9d0f55", "#682860", "#0080A0", "#F9DF79"
)

n_cols = 100
vec_colrs = sample(colorRampPalette(df_cepi_cols$light_2)(n_cols), n_cols)


vec_colrs_region = c(df_cepi_cols$base_col[c(2,1,3,4)], "gray33", "#78251E")
vec_colrs_region = c("#ffa500", "#004658", "#547dbf", "#F9DF79", "#db4437", "#9d0f55")

# cat(vec_colrs_region, sep='\n')


ggplot(
    spread_cumul_timing_col_region, # spread_cumul_timing, 
    aes(x=outbreak_start_day/365, y=cumul_nspread)) + 
    # geom_line(aes(color=simulation), alpha=0.5) + 
    geom_line(aes(
        group=simulation, color=region_col
        ), linewidth = 1, alpha=0.45) + 
    scale_color_manual(values = vec_colrs_region, name='WHO region') + 
    # scale_color_manual(values = rep('darkgray', times=100)) + 
    # geom_line(color='darkgray', alpha=0.5) + 
    geom_line(
        data=df_zika_cumul, aes(x=date_norm, y=cumul_nspread), 
        linewidth=2, color='black'
        ) + 
    # guides(color='none') + 
    xlim(0,2) + ylim(0, 72) + 
    labs(
        x='Time (years)', # x='Simulation time (years)', 
        y='Number of countries\nexperiencing outbreaks'
        ) + 
    theme_light(base_size=16)


ggsave(
    filename = 'figs/zika_spread_tune.png',
    width = 3500, height = 2500, units = "px",
    dpi = 400,
    bg = "transparent"
)


