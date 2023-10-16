library('lubridate')
library('tidyverse')
library('ggplot2')

### TODO ####
# add total outbreak sum(outbreak)
# compare to AOC


############################
#       LOAD OUTBREAKS     # 
############################


l_outbreaks = get(load(file='LassaX/data_chik/curve_fitting/paho_case_data/paho_outbreaks.RData'))
df_outbreaks = read.csv(file='LassaX/data_chik/curve_fitting/paho_case_data/paho_outbreaks.csv') 
df_outbreaks$country = factor(df_outbreaks$country)
df_outbreaks$date = ymd(df_outbreaks$date)


shin_model = function(df=df,
                      ampl,
                      squeeze_l, 
                      squeeze_r) { 
    xs = df$xdates
    ys = df$ycases
    # amplitude=max(ys)
    h_transl=xs[which(ys == max(ys))]
    ys_sim =  ampl*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
    return(ys_sim)
}


# map(l_outbreaks, function(.outbreak){
#     plot_outbreak(.outbreak)})
out = map(l_outbreaks, function(.outbreak){
    
    ycases = .outbreak$cases
    xdates = .outbreak$date_frac
    df_dates_cases = data.frame(xdates = xdates, ycases = ycases)
    
    t_min = min(xdates)
    t_max = max(xdates)
    amplitude=max(ycases)
    h_transl=xdates[which(ycases == max(ycases))]
    params = NULL
    shin_curve = NULL
    df_plot=NULL
    aoc_tot=NULL
    gplot=NULL
    possibleError  = tryCatch(
        {fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, ampl, 
                                                         squeeze_l, squeeze_r),
                       start = list(ampl = amplitude, squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        }, 
        error=function(e) e)
    
    if(inherits(possibleError, "error")) {
        print(possibleError)
    } else {
        fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, ampl, 
                                                         squeeze_l, squeeze_r),
                      start = list(ampl = amplitude, squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        params = summary(fit_nls)$parameters
        
        shin_curve = function(xs=xdates,
                              h_transl=xdates[which(ycases == max(ycases))],
                              amplitude=params[1],
                              squeeze_l=params[2],
                              squeeze_r=params[3]){
            ys =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
            return(ys)
        }
        
        ypred=shin_curve(
            xs = xdates,
            h_transl = h_transl,
            amplitude = params[1],
            squeeze_l = params[2],
            squeeze_r = params[3])
        
        df_plot = data.frame(xdates,ycases,ypred) %>% reshape2::melt(id='xdates')
        
        gplot = ggplot(df_plot, aes(xdates, value, col=variable)) + 
            geom_point(alpha=0.4) + geom_line(alpha=0.4)+ 
            theme_bw() + ylim (0, 1.1 * max(ycases)) + 
            labs(x='Date', y='Cases', title=.outbreak$country[1])
        
        aoc_tot = integrate(shin_curve, lower = t_min, upper = t_max)
    }
    return(list(params=params, amplitude=amplitude, peak_time=h_transl, 
                t_min=t_min, t_max=t_max,
                df_plot=df_plot, aoc=aoc_tot, total_cases=sum(ycases),
                gplot=gplot))
})


map(seq_along(out), function(.x) ggsave(filename=paste(
    'LassaX/data_chik/pred_figs/',.x,'.png',sep=''),
                             plot=out[[.x]]$gplot, height=3, width=5, units='in'))


map(out, function(.x) .x$aoc$value) %>% unlist %>% hist(breaks=10)


params_out = map(out, function(.x) data.frame(squeeze_l=.x$params[1,1],
                                 squeeze_r=.x$params[2,1])) %>% bind_rows() 
params_out

out[[1]]$params





#############################
#       ESTIMATE SHAPES     # 
#############################

#########
# CASES #
#########

out = map(l_outbreaks, function(.outbreak){
    
    ycases = .outbreak$cases
    xdates = .outbreak$date_frac
    df_dates_cases = data.frame(xdates = xdates, ycases = ycases)
    
    t_min = min(xdates)
    t_max = max(xdates)
    amplitude=max(ycases)
    h_transl=xdates[which(ycases == max(ycases))]
    params = NULL
    shin_curve = NULL
    df_plot=NULL
    aoc_tot=NULL
    gplot=NULL
    possibleError  = tryCatch(
        {fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, ampl, 
                                                          squeeze_l, squeeze_r),
                       start = list(ampl = amplitude, squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        }, 
        error=function(e) e)
    
    if(inherits(possibleError, "error")) {
        print(possibleError)
    } else {
        fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, ampl, 
                                                         squeeze_l, squeeze_r),
                      start = list(ampl = amplitude, squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        
        shin_curve = function(xs=xdates,
                              h_transl=xdates[which(ycases == max(ycases))],
                              amplitude=params[1],
                              squeeze_l=params[2],
                              squeeze_r=params[3]){
            ys =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
            return(ys)
        }
        
        ypred=shin_curve(
            xs = xdates,
            amplitude=params[1],
            squeeze_l=params[2],
            squeeze_r=params[3])
        
        df_plot = data.frame(xdates,ycases,ypred) %>% reshape2::melt(id='xdates')
        
        gplot = ggplot(df_plot, aes(xdates, value, col=variable)) + 
            geom_point(alpha=0.4) + geom_line(alpha=0.4)+ 
            theme_bw() + ylim (0, 1.1 * max(ycases)) + 
            labs(x='Date', y='Cases', title=.outbreak$country[1])
        
        aoc_tot = integrate(shin_curve, lower = t_min, upper = t_max)
    }
    if (!is.null(params)) {
        return(list(df_out=data.frame(
            country=as.character(.outbreak$country[1]), code=as.character(.outbreak$code[1]),
            total_size = sum(.outbreak$cases),
            t_min=t_min, t_max=t_max, peak_time=h_transl, 
            amplitude=params[1,1], amplitude_SE = params[1,2], 
            s_l=params[2,1], s_l_SE=params[2,2], s_r=params[3,1], s_r_SE=params[3,2],
            aoc=aoc_tot$value),
            df_plot=df_plot, gplot=gplot))
    } else { return(list(df_out=NULL, df_plot=df_plot, gplot=gplot))}

})


params_cases = map(out, function(.x) if(!is.null(.x[[1]])) .x[[1]]) %>% bind_rows() 

params_out = params_cases[
    params_cases$s_l_SE < 1e4 & params_cases$s_r_SE < 1e4, ] 
 
# check AOC vs sum(cases)
(100*( params_out$total_size - params_out$aoc ) / params_out$total_size) %>%
    format(., digits=3) %>% as.numeric %>% hist(main='', xlab='% error')
params_out$aoc %>% hist(main='', xlab='% error')
params_out$total_size %>% hist(main='', xlab='% error')

write.csv(params_cases, 'LassaX/data_chik/shape_params_cases_2.csv', row.names = F)
#write.csv(params_cases, 'LassaX/data_chik/shape_params_cases.csv', row.names = F)


######### benchmarking!!! 

params_inc_per_capita['country']

benchmark(
    "df[]" = {
        params_inc_per_capita['country']
    },
    "df$" = {
        params_inc_per_capita$country
    },
    "list" = {
        out[[1]]$df_out$country
    },
    replications = 10000,
    columns = c("test",
                "replications",
                "elapsed",
                "relative",
                "user.self",
                "sys.self"))



mat = matrix(NA, ncol = 3, nrow=3)

rownames(mat) = c('a', 'b', 'c')
colnames(mat) = c('a1', 'b1', 'c1')
mat_l = map(1:3, function(.x) mat[.x, ])
names(mat_l) = c('a', 'b', 'c')

benchmark(
    "mat['a']" = {
        mat['a', ]
    },
    "mat_l$a" = {
        mat_l$a
    },
    replications = 100000,
    columns = c("test",
                "replications",
                "elapsed",
                "relative",
                "user.self",
                "sys.self"))
library(reshape2)
all_outs = sapply(out, function(.out){
    if (!is.null(.out$df_plot)){
        ypred_sum = sum(.out$df_plot$value[.out$df_plot$variable=='ypred'])
        ycases_sum = sum(.out$df_plot$value[.out$df_plot$variable=='ycases'])
        aoc = .out$df_out$aoc
        rtrn = c(ypred_sum,ycases_sum,aoc) %>% format(scientific=F, digits=4)
        names(rtrn) = c('ypred_sum','ycases_sum','aoc')
        return(rtrn)} else c(NA, NA, NA) 
}) %>% t 
all_outs_long = all_outs %>% melt %>%
    mutate(dataset=as.factor(Var1), type=as.factor(Var2)) %>% 
    select(dataset,type,value)

ggplot(all_outs_long, aes(type, value)) + geom_point(aes(color=type)) + 
    facet_wrap(~dataset, scales = 'free_y') + 
    theme_bw() + theme(legend.position = 'bottom')
