library(tidyverse)
library(ggplot2)

##############################
##### FOR ONE SIMULATION #####
##############################

# check spread 
outbr_dur_country = apply(m_spread, 1, sum) %>% .[.!=0] %>% sort(decreasing = T) #%>% length

length(outbr_dur_country)
outbr_dur_country
# barplot(outbr_dur_country, las=2, ylab='Duration / days')

# v_daily_infectious_ppl
# df_res_curve
# df_res_summary


df_ressum = map(ls_outbreaks, 'df_res_summary') %>% bind_rows

df_rescur = map(ls_outbreaks, 'df_res_curve') %>% bind_rows


plot(sort(df_ressum$prop_pop_affected, decreasing = T))

dim(df_ressum)

ggplot(df_ressum, aes(x=timing, y=country)) +
    geom_point(aes(col=region_name, size=IncCumul_U_final))


# COUNTRY
ggplot(df_rescur, aes(time_years, daily_infections_sim)) + 
    geom_point(aes(color=region_code), size=1, alpha=0.2) +
    facet_wrap(~country, scales = 'free_y') + 
    theme_bw()

# Time taken: about 2 hours 20 mins 


# hist of suitability 
ggplot(df_burden, aes(x=mean_pop_weighted)) + 
    geom_histogram(aes(fill=region_code), alpha=0.75) + 
    labs(
        x='Population-weighted mean suitability',
        y='Count'
    )

colnames(df_burden)






# plot an individual simulation 


all_daily_infs <- map(all_files, function(dirname) {
    res <- readRDS(dirname)
    map(res, function(.ls_sim) .ls_sim$v_daily_new_infections) %>% bind_rows()
}, .progress = T) #%>% bind_rows()

all_daily_infs[[1]]

d_inf = readRDS('res/importation_model_par/simulation_5.RDS')

df_all_sims = map(d_inf, 'df_res_curve') %>% bind_rows

dim(df_all_sims)
colnames(df_all_sims)



# country
# code
# region_name
# region_code
# pop_size
# simulation
# time_days
# time_years
# timing
# IncCumul_U_final

sim1 = df_all_sims_long %>% filter(simulation==1)

dim(sim1)


ggplot(sim1, aes(x=time_years, y=daily_infections_sim)) + 
    geom_point(aes(col=timing), size=1, alpha=0.5) + 
    facet_wrap(vars(country), scale='free_y') + 
    scale_color_gradient2(
        low = "#ffa500",
        mid = "gray",
        high = "#0080A0", # can optionally use muted()
        midpoint = 180
    ) +
    theme_light() + 
    labs(x='Time / years', y='Daily infections') 


df_all_sims$timing %>% unique
