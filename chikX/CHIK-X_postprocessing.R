library('tidyverse')
library('cowplot')

# y=\frac{a\cdot2}{\left(\left(e^{\left(x-h\right)\cdot s_{l}}\right)+\left(e^{\left(-x+h\right)\cdot s_{r}}\right)\right)}

all_files = list.files('LassaX/chik_res/', 'list_diseaseX_i_outputSet_2') %>%
    str_sort(numeric = TRUE) %>% .[1:20]


# extract final df with summary res 
all_params = map(all_files, function(dirname){
    res = get(load(paste('LassaX/chik_res/', dirname, sep='')))
    map(res, function(.x) .x$health_econ) %>% bind_rows
}) %>% bind_rows

# id=c('country','code','timing','infect0','simulation','vacc_alloc','vacc_strategy')


# TIME 
ggplot(all_params, aes(time_years, cases_sim)) + 
    geom_point(aes(color=timing), size=1, alpha=0.2) +  # size=timing
    facet_wrap(~simulation, scales = 'free_y', nrow = 4) + 
    theme_bw()

# COUNTRY
ggplot(all_params, aes(time_years, cases_sim)) + 
    geom_point(aes(color=code), size=1, alpha=0.2) +  # size=timing
    facet_wrap(~simulation, scales = 'free_y', nrow = 4) + 
    theme_bw()




# the ugliest plot to have graced this planet yet
# ggplot(all_params, aes(time_years, cases_sim)) + 
#     geom_point(aes(color=timing), size=1, alpha=0.2) +  # size=timing
#     facet_grid(vacc_strategy~country, scales = 'free_y') + theme_bw()
# 


all_params$simulation %>% unique %>% sort %>% identical(1:100)

res = get(load(paste('LassaX/chik_res/', all_files[2], sep='')))
res=list_diseaseX_i
# map(res, function(.x) .x$health_econ) %>% bind_rows
# map(res, function(.x) .x$params) %>% bind_rows

res 
df_res2 = map(res, function(.x) .x$health_econ) %>% bind_rowsfr
head(df_res2)

df_timing = df_res2[,c('timing', 'code')] %>% unique 
df_timing$timing = df_timing$timing/365.25


df_res2$code %>% unique %>% 
    countrycode::countrycode(origin = 'iso3c', destination = 'country.name')
    
ggplot(df_res2, aes(time_years, cases_sim)) + 
    geom_point(aes(color=code), size=1, alpha=0.2) + 
    geom_vline(data=df_timing, aes(xintercept = timing, color=code), linetype='dashed') + 
    facet_wrap(~code, scales = 'free_y', nrow = 4) + 
    guides(color=guide_legend(override.aes = list(linetype = 0))) + 
    labs(x='Time (years)', y='Simulated Daily Cases', title='Simulation 3') +
    theme_light()
    # theme_classic() + background_grid()   
    # theme_minimal_grid()
    # theme_bw()


df_res2[df_res2$country=='Oman','timing'] %>% unique
df_res2[df_res2$country=='India','timing'] %>% unique
df_res2[df_res2$country=='Pakistan','timing'] %>% unique


 
ti = res[[29]]$health_econ$timing
dt = res[[29]]$params$dt
xs = res[[29]]$params$xs
ys = res[[29]]$params$outbreak_res
xs_before = seq(from=0, to=ti/365, length.out=ti)
new_t = c(xs_before,
          xs + (ti/365))
new_ys = c(rep(0, times=ti), ys)


res[[29]]$health_econ
res[[29]]$params

all_params[all_params$simulation==5, ]

data.frame(time=xs, cases=ys) %>% 
    filter(cases>0) %>% 
    mutate(cases=cases + rnorm(length(cases), mean(cases), mean(cases)*0.05)) %>%
    mutate(cases = ifelse(cases > 0, cases, 0)) %>%
    plot

plot(res_out, pch=20, cex=0.5)
    
plot(xs, ys)

length(new_t)
length(new_ys)
plot(new_t, new_ys)
points(xs, ys)


plot(xs, ys)

res = map(list.files('LassaX/chik_res/'), function(dirname){
    res = get(load(paste('LassaX/chik_res/', dirname, sep='')))
})  

res[[10]]

# plot in base r 
map(10:12, function(i){
    walk(res[[i]], function(.x) {
        title = paste(.x$health_econ$country[1], i, sep='-')
        jpeg(paste("LassaX/chik_res/plots/", title,rnorm(1,1,1),'.png', sep=''),
             width = 600, height = 600)
        plot(.x$params$xs, .x$params$outbreak_res,
             main=title,
             xlab='Years', ylab='incidence per capita')
        dev.off()
    })
}, .progress=T)



plot(res[[1]]$params$xs, res[[1]]$params$outbreak_res)

str(res[[1]])

sum(res[[1]]$params$outbreak_res == Inf)

ys = res[[10]][[1]]$params$outbreak_res 
xs = res[[10]][[1]]$params$xs

mean(ys)
rnorm(length(ys), mean(ys), sd(ys))

plot(xs, ys+rnorm(length(ys), mean(ys), mean(ys)*0.1), pch=20,cex=0.5)
plot(xs, ys, pch=20,cex=0.5)

sapply(res[[13]], length)

sample_params = sample_n(shape_params_inc_per_capita, 1)
new_s_l = rnorm(1, sample_params$s_l, sample_params$s_l_SE)
new_s_r = rnorm(1, sample_params$s_r, sample_params$s_r_SE)
# negative params not accepted 
new_s_l = ifelse(new_s_l < 0, new_s_l - sample_params$s_l, new_s_l)
new_s_r = ifelse(new_s_r < 0, abs(new_s_r) - sample_params$s_r, new_s_r)

xs = seq(from=0,to=1,length.out=365)
outbreak_res = shin_curve(xs=xs, amplitude=0.2, 
                          h_transl=0.5, 
                          s_l=new_s_l, s_r=new_s_r, add_noise = T)
plot(outbreak_res, pch=20, cex=0.5)


for (i in 1:20) {
    outbreak = outbreak_res+rnorm(length(outbreak_res), 
                                  mean(outbreak_res), mean(outbreak_res)*0.05)
    plot(outbreak, pch=20, cex=0.5, ylim=c(0,0.5))
}