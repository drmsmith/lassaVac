library('lubridate')
library('tidyverse')
library('ggplot2')

ls_paho_per_capita = get(load("data_thief/ls_PAHO_per_capita.RData"))
df_paho_per_capita = read.csv(file="data_thief/PAHO_long_per_capita.csv")
df_paho_per_capita$country = factor(df_paho_per_capita$country)
df_paho_per_capita$date = as.Date(df_paho_per_capita$date)

plot_outbreak = function(df=outbreak, type='cases', vlines=NULL){
    p = ggplot(df, aes(date_frac, get(type))) + 
        geom_point(size=1) + geom_line() + theme_bw() + 
        labs(x='Date', y=str_replace_all(tools::toTitleCase(type),'_',' '),
             title=df$country[1])
    if (!is.null(vlines)) p = p + 
            sapply(vlines, 
                   function(xint) geom_vline(aes(xintercept=date_frac[xint]), 
                                             linetype='dashed'))
    return(p)
}

process_peaks = function(outbreak){
    # [peak_size - peak_loc - start - end]
    peaks = pracma::findpeaks(outbreak$cases, threshold = 0.2*max(outbreak$cases),# nups = 1, 
                              minpeakdistance = 2, sortstr = T)
    peaks
    nums = peaks[,3:4] %>% sort %>% .[duplicated(.)]
    inds = if (dim(peaks)[1] > 1) {
        peaks[(peaks[, 3] %in% nums |
                   peaks[, 4] %in% nums), 3:4] %>% range + c(-2,2)
    } else {
        peaks[3:4] + c(-2,2)
    }
    inds = ifelse(inds < 1, 1, inds)
    inds = ifelse(inds > nrow(outbreak), nrow(outbreak), inds)
    # o_start = outbreak %>% filter(date_frac < peak_time & cases==0) %>% 
    #     select(date_frac) %>% first %>% unlist
    # o_end = outbreak %>% filter(date_frac > peak_time & cases==0) %>% 
    #     select(date_frac) %>% last %>% unlist
    # inds = which(outbreak$date_frac %in% c(o_start, o_end))
    return(list(peaks=peaks, inds=inds))
}



shin_model = function(df=df_dates_cases,
                      squeeze_l=21, 
                      squeeze_r=9) { 
    xs = df$xdates
    ys = df$ycases
    amplitude=max(ys)
    h_transl=xs[which(ys == max(ys))]
    ys_sim =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
    return(ys_sim)
}


shin_curve = function(xs=xdates,
                      amplitude=max(ycases),
                      h_transl=xdates[which(ycases == max(ycases))],
                      squeeze_l=params[1],
                      squeeze_r=params[2]){
    ys =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
    return(ys)
}


#####################
# processing peaks # 
####################

# these vars are needed to course the functions 
outbreak = ls_paho_per_capita[[3]]
plot_outbreak()#type='inc_per_capita')
    

# [peak_size - peak_loc - start - end]
peaks = pracma::findpeaks(outbreak$cases, threshold = 0.3*max(outbreak$cases))
peaks


peak_amplitude = peaks[1]
peak_time = outbreak$date_frac[peaks[2]]
inds = peaks[1,3:4]


o_start = outbreak %>% filter(date_frac < peak_time & cases==0) %>% 
    select(date_frac) %>% first %>% unlist
o_end = outbreak %>% filter(date_frac > peak_time & cases==0) %>% 
    select(date_frac) %>% last %>% unlist
inds = which(outbreak$date_frac %in% c(o_start, o_end))

ycases = outbreak$cases[inds[1]:inds[2]]
xdates = outbreak$date_frac[inds[1]:inds[2]]
df_dates_cases = data.frame(xdates = xdates, ycases = ycases)
plot(df_dates_cases)

description = list(amplitude=peak_amplitude,h_transl=peak_time)





# fit 
fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, squeeze_l, squeeze_r),
              start = list(squeeze_l = 1, squeeze_r = 1))
params = summary(fit_nls)$parameters


# plot predicted points
df_plot = data.frame(xdates,ycases, ypred= shin_curve(
        xs = xdates,
        amplitude = peak_amplitude,
        h_transl = peak_time,
        squeeze_l = params[1],
        squeeze_r = params[2])) %>% reshape2::melt(id='xdates')
        
gplot = ggplot(df_plot, aes(xdates, value, col=variable)) + 
    geom_point(alpha=0.4) + geom_line(alpha=0.4)+ 
    theme_bw() + ylim (0, 1.2 * max(ycases)) + labs(x='Date', y='Cases')
gplot
# plot data
points(xdates, ycases,
       ylim = c(0, 1.2 * max(ycases)))


# changed defaults shin_curve to the suitable param values
aoc_tot = integrate(
    shin_curve,
    lower = min(df_dates_cases$xdates),
    upper = max(df_dates_cases$xdates)
)
aoc_tot

    




plots = map(seq_along(ls_paho_per_capita), function(i){
    outbreak = ls_paho_per_capita[[i]]
    #plot_outbreak()#type='inc_per_capita')
    
    peaks_out = process_peaks(outbreak = outbreak)
    peaks = peaks_out$peaks
    inds = peaks_out$inds
    
    peak_amplitude = peaks[1]
    peak_time = outbreak$date_frac[peaks[2]]
    
    ycases = outbreak$cases[inds[1]:inds[2]]
    xdates = outbreak$date_frac[inds[1]:inds[2]]
    df_dates_cases = data.frame(xdates = xdates, ycases = ycases)
    p = plot_outbreak(df = outbreak, vlines = inds)
    return(p)
})











#######################
#   LOAD MARKED DATA  #
#######################


df_paho_for_fits = read.csv(file="LassaX/data_chik/PAHO_long_per_capita_outbreaks.csv")
df_paho_for_fits$country = factor(df_paho_for_fits$country)
df_paho_for_fits$date = dmy(df_paho_for_fits$date)

head(df_paho_for_fits)
df_paho_for_fits$outbreak = ifelse(df_paho_for_fits$outbreak=='', NA, df_paho_for_fits$outbreak)

start_ends = df_paho_for_fits %>% na.omit() 
all_countries = start_ends$country %>% unique %>% as.character


l_outbreaks = map(all_countries, function(.country){
    all_starts = start_ends %>% filter(country==.country & outbreak=='s') %>% .$date
    all_ends = start_ends %>% filter(country==.country & outbreak=='e') %>% .$date
    country_sub = df_paho_for_fits %>% filter(country==.country)
    outs = map2(all_starts, all_ends, function(.s, .e){
        out = country_sub %>% filter(date >= .s & date <= .e)
        }) #%>% bind_rows()
    }) %>% unlist(recursive = F) 

df_outbreaks = l_outbreaks %>% bind_rows

# save(l_outbreaks, file='LassaX/data_chik/paho_outbreaks.RData')
# write.csv(df_outbreaks, file='LassaX/data_chik/paho_outbreaks.csv', row.names=F) 




############################
#       LOAD OUTBREAKS     # 
############################


l_outbreaks = get(load(file='LassaX/data_chik/curve_fitting/paho_case_data/ppaho_outbreaks.RData'))
df_outbreaks = read.csv(file='LassaX/data_chik/curve_fitting/paho_case_data/paho_outbreaks.csv') 
df_outbreaks$country = factor(df_outbreaks$country)
df_outbreaks$date = ymd(df_outbreaks$date)


shin_model = function(df=df_dates_cases,
                      squeeze_l=21, 
                      squeeze_r=9) { 
    xs = df$xdates
    ys = df$ycases
    amplitude=max(ys)
    h_transl=xs[which(ys == max(ys))]
    ys_sim =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
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
        {fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, squeeze_l, squeeze_r),
                       start = list(squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        }, 
        error=function(e) e)
    
    if(inherits(possibleError, "error")) {
        print(possibleError)
    } else {
        fit_nls = nls(df_dates_cases$ycases ~ shin_model(
            df_dates_cases, squeeze_l, squeeze_r
        ), start = list(squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        
        shin_curve = function(xs=xdates,
                              amplitude=max(ycases),
                              h_transl=xdates[which(ycases == max(ycases))],
                              squeeze_l=params[1],
                              squeeze_r=params[2]){
            ys =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
            return(ys)
        }
        
        ypred=shin_curve(
            xs = xdates,
            amplitude = amplitude,
            h_transl = h_transl,
            squeeze_l = params[1],
            squeeze_r = params[2])
        
        df_plot = data.frame(xdates,ycases,ypred) %>% reshape2::melt(id='xdates')
        
        gplot = ggplot(df_plot, aes(xdates, value, col=variable)) + 
            geom_point(alpha=0.4) + geom_line(alpha=0.4)+ 
            theme_bw() + ylim (0, 1.1 * max(ycases)) + 
            labs(x='Date', y='Cases', title=.outbreak$country[1])
        
        aoc_tot = integrate(shin_curve, lower = t_min, upper = t_max)
    }
    return(list(params=params, amplitude=amplitude, peak_time=h_transl, 
                t_min=t_min, t_max=t_max,
                df_plot=df_plot, aoc=aoc_tot, gplot=gplot))
})


map(seq_along(out), function(.x) ggsave(filename=paste('LassaX/data_chik/pred_figs/',.x,'.png',sep=''),
                             plot=out[[.x]]$gplot, height=3, width=5, units='in'))


map(out, function(.x) .x$aoc)


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
        {fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, squeeze_l, squeeze_r),
                       start = list(squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        }, 
        error=function(e) e)
    
    if(inherits(possibleError, "error")) {
        print(possibleError)
    } else {
        fit_nls = nls(df_dates_cases$ycases ~ shin_model(
            df_dates_cases, squeeze_l, squeeze_r
        ), start = list(squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        
        shin_curve = function(xs=xdates,
                              amplitude=max(ycases),
                              h_transl=xdates[which(ycases == max(ycases))],
                              squeeze_l=params[1],
                              squeeze_r=params[2]){
            ys =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
            return(ys)
        }
        
        ypred=shin_curve(
            xs = xdates,
            amplitude = amplitude,
            h_transl = h_transl,
            squeeze_l = params[1],
            squeeze_r = params[2])
        
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
            t_min=t_min, t_max=t_max, peak_time=h_transl, amplitude=amplitude,
            s_l=params[1,1], s_l_SE=params[1,2], s_r=params[2,1], s_r_SE=params[2,2],
            aoc=aoc_tot$value),
            df_plot=df_plot, gplot=gplot))
    } else { return(list(df_out=NULL, df_plot=df_plot, gplot=gplot))}

})


params_cases = map(out, function(.x) if(!is.null(.x[[1]])) .x[[1]]) %>% bind_rows() 



###################
# INC PER CAPITA #
##################


out = map(l_outbreaks, function(.outbreak){
    
    ycases = .outbreak$inc_per_capita
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
        {fit_nls = nls(df_dates_cases$ycases ~ shin_model(df_dates_cases, squeeze_l, squeeze_r),
                       start = list(squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        }, 
        error=function(e) e)
    
    if(inherits(possibleError, "error")) {
        print(possibleError)
    } else {
        fit_nls = nls(df_dates_cases$ycases ~ shin_model(
            df_dates_cases, squeeze_l, squeeze_r
        ), start = list(squeeze_l = 10, squeeze_r = 10))
        params = summary(fit_nls)$parameters
        
        shin_curve = function(xs=xdates,
                              amplitude=max(ycases),
                              h_transl=xdates[which(ycases == max(ycases))],
                              squeeze_l=params[1],
                              squeeze_r=params[2]){
            ys =  amplitude*2 / ( exp((xs-h_transl)*squeeze_l) + exp((-xs+h_transl)*squeeze_r) )
            return(ys)
        }
        
        ypred=shin_curve(
            xs = xdates,
            amplitude = amplitude,
            h_transl = h_transl,
            squeeze_l = params[1],
            squeeze_r = params[2])
        
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
            t_min=t_min, t_max=t_max, peak_time=h_transl, amplitude=amplitude,
            s_l=params[1,1], s_l_SE=params[1,2], s_r=params[2,1], s_r_SE=params[2,2],
            aoc=aoc_tot$value),
            df_plot=df_plot, gplot=gplot))
    } else { return(list(df_out=NULL, df_plot=df_plot, gplot=gplot))}
    
})


params_inc_per_capita = map(out, function(.x) if(!is.null(.x[[1]])) .x[[1]]) %>% bind_rows() 


floor(params_cases[,c(7,9)]) == floor(params_inc_per_capita[,c(7,9)])


# the shape parameters are more or less the same 
floor(params_cases[12,7:8])
floor(params_inc_per_capita[12,7:8])


write.csv(params_cases, 'LassaX/data_chik/shape_params_cases.csv', row.names = F)
write.csv(params_inc_per_capita, 'LassaX/data_chik/shape_params_inc_per_capita.csv', row.names = F)



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
