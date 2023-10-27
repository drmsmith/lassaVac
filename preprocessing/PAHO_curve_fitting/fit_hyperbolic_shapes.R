# fit hyperbolic sinh curve to outbreak data
# using (questionable but simple) non-linear
# least squares (nls)

library('conflicted')
library('lubridate')
library('tidyverse')
library('ggplot2')
conflicts_prefer(
    dplyr::filter(),
    .quiet=T
)


############################
#       LOAD OUTBREAKS     #
############################
# outbreaks for fitting produced above
# using the long-format data with marked
# outbreak start-end dates
# in a column called 'outbreak'


# original data
# l_outbreaks = get(load(
#     file='preprocessing/data/PAHO_case_data_2014-2017/ls_PAHO_outbreaks.RData'))
# df_outbreaks = read.csv(file='/preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_outbreaks.csv')


# data where
# peaks where fitting fails are removed
# and zeros are added at the start/end
# for better curve shapes which could be
# used for the outbreak simulations
l_outbreaks = get(load(
    file='preprocessing/data/PAHO_case_data_2014-2017/ls_PAHO_outbreaks_manual_adj.RData'))
df_outbreaks = read.csv(file='preprocessing/data/PAHO_case_data_2014-2017/df_PAHO_outbreaks_manual_adj.csv')
df_outbreaks$country = factor(df_outbreaks$country)
df_outbreaks$date = ymd(df_outbreaks$date)



#######################
# HYPERBOLIC FUNCTION #
#######################

# function used for fitting
# hyperbolic model parameterised as curve with
# h_transl = horizontal translation of peak
# amplitude = maximum reported cases
# s_l and s_r = x axis transformations
# which determine the shape of the left and right
# tails respectively
# df_dates_cases = two column matrix with cols = xdates and ycases
    # xdates and ycases (time as float and reported cases, presumably int)
sinh_model = function(df=df_dates_cases,
                      s_l=21,
                      s_r=9) {
    xs = df$xdates
    ys = df$ycases
    amplitude=max(ys)
    h_transl=xs[which(ys == max(ys))]
    ys_sim =  amplitude*2 / ( exp((xs-h_transl)*s_l) + exp((-xs+h_transl)*s_r) )
    return(ys_sim)
}


#############################
#       ESTIMATE SHAPES     #
#############################

#########
# CASES #
#########

# model fitting happens below
out = map(l_outbreaks, function(.outbreak){
    # extract cases and date as fraction
    ycases = .outbreak$cases
    xdates = .outbreak$date_frac
    # dummy df fed into model function
    df_dates_cases = data.frame(xdates = xdates, ycases = ycases)

    # get useful params like amplitude and duration of outbreak
    t_min = min(xdates)
    t_max = max(xdates)
    amplitude=max(ycases)
    h_transl=xdates[which(ycases == max(ycases))]

    # initialise returned params to NULL
    params = NULL
    sinh_curve = NULL
    df_plot=NULL
    aoc_tot=NULL
    gplot=NULL
    # using tryCatch to avoid stopping the loop if the fitting fails
    possibleError  = tryCatch(
        {fit_nls = nls(df_dates_cases$ycases ~ sinh_model(df_dates_cases, s_l, s_r),
                       start = list(s_l = 10, s_r = 10))
        params = summary(fit_nls)$parameters
        },
        error=function(e) e)

    # if an error is thrown while fitting
    # print error and problematic country code
    # paraguay always throws an error as the fits fail
    if(inherits(possibleError, "error")) {
        cat('Error when fitting to', .outbreak$code[1], ': \n')
        cat(paste(possibleError))
    } else {
        # repeat fitting here because I don't know
        # how to avoid doing this twice :(
        fit_nls = nls(df_dates_cases$ycases ~ sinh_model(
            df_dates_cases, s_l, s_r
        ), start = list(s_l = 10, s_r = 10))
        params = summary(fit_nls)$parameters

        # this function is defined inside the loop
        # as it is the curve (not the model)
        # and it is used for
        # (1) plotting the model "predictions"
        # (2) integrating to get the AOC
        # (which is why the arguments need to be
        # initialised to the parameter estimates)
        sinh_curve = function(xs=xdates,
                              amplitude=max(ycases),
                              h_transl=xdates[which(ycases == max(ycases))],
                              s_l=params[1], # initialise parameters to estimates
                              s_r=params[2]){
            ys =  amplitude*2 / ( exp((xs-h_transl)*s_l) + exp((-xs+h_transl)*s_r) )
            return(ys)
        }

        # the model predictions
        ypred=sinh_curve(
            xs = xdates,
            amplitude = amplitude,
            h_transl = h_transl,
            s_l = params[1],
            s_r = params[2])

        # reshape params into long format for ggplot
        df_plot = data.frame(date=xdates,paho=ycases,model=ypred) %>% reshape2::melt(id='date')

        # plot PAHO data (ycases) and predictions (ypred)
        gplot = ggplot(df_plot, aes(date, value, col=variable)) +
            geom_point(alpha=0.4) + geom_line(alpha=0.4)+
            theme_bw() + ylim (0, 1.1 * max(df_plot$value)) +
            labs(x='Date', y='New cases (monthly)', title=.outbreak$country[1]) +
            guides(color = guide_legend(title='Data'))

        # get area under curve
        aoc_tot = integrate(sinh_curve, lower = t_min, upper = t_max)
    }
    if (!is.null(params)) {
        return(list(
            df_out = data.frame(
                country = as.character(.outbreak$country[1]), code = as.character(.outbreak$code[1]),
                t_min = t_min, t_max = t_max, peak_time = h_transl, amplitude = amplitude,
                s_l = params[1, 1], s_l_SE = params[1, 2], s_r = params[2, 1], s_r_SE = params[2, 2],
                aoc = aoc_tot$value
            ),
            df_plot = df_plot, gplot = gplot
        ))
    } else {
        return(list(df_out = NULL, df_plot = df_plot, gplot = gplot))
    }

})


if (!interactive()) { # not run when file is sourced 
    # plot predicted (by model) against data
    # UPDATE FILE PATH TO CORRECT PATH if needed
    dir.create('preprocessing/PAHO_curve_fitting/curves_man_adj/')
    dest_dir = 'preprocessing/PAHO_curve_fitting/curves_man_adj/'
    walk(seq_along(out), function(.n_curve) {
        if (!is.null(out[[.n_curve]]$gplot)) { # if plot exists
            ggsave( # save plot named as number
                filename = paste( # file name = index_code.png
                    dest_dir, .n_curve, "_", out[[.n_curve]]$df_out["code"], ".png",
                    sep = ""
                ),
                plot = out[[.n_curve]]$gplot, height = 3, width = 5, units = "in"
            )
        }
    }, .progress = T)
}


# get all params as a neat table (drop NULLs)
params_cases = map(out, function(.x) if(!is.null(.x[[1]])) .x[[1]]) %>% bind_rows()


# write.csv(params_cases, 'data/shape_params_PAHO_cases.csv', row.names = F)
write.csv(params_cases, 'data/df_shape_params_PAHO_cases_adj.csv', row.names = F)


message('finished running `fit_hyperbolic_shapes.R`')

########################
# INCIDENCE PER CAPITA #
#######################

# to perform the fitting on 
# incidence per capita 
# run the code above but with 
# an incidence per capita column 
# for ycases in df_dates_cases
# obtained by dividing cases
# by 2016 UN-adjusted worldpop data
# therefore, load correct long-format csv with
# outbreak start-ends, run the processing above
# to get the appropriate l_outbreaks

