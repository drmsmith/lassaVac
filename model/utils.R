library('magick', quietly = T, warn.conflicts = F)
library('tidyverse')


# paho case data
df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
paho_codes <- df_paho_outbreak_sizes$code


# return the vector of the number of daily infectious individuals 
# numeric of length v_daily_infections 
calc_daily_infections <- function(
    v_daily_infections,         # numeric vector 
    infectiousness_duration=7   # integer 
    ){
    n_lags = infectiousness_duration-1 # sum today and infectiousness_duration-1 days 
    df_lags = map( # returns list of lagged vectors 
        1:n_lags, 
        function(.lag_dur) lag(v_daily_infections, n=.lag_dur)
        )
    # change names to suppress error messages 
    names(df_lags) = paste0('lag_', 1:n_lags)
    df_lags = bind_cols(df_lags) # convert to df 
    df_lags = as.data.frame(list(daily=v_daily_infections, df_lags)) # combine together
    # calc. daily infectious individuals based on duration of infectiousness 
    daily_infectious_ppl = apply(df_lags, 1, function(.row) sum(.row, na.rm=T))
    return(daily_infectious_ppl)
}

# test 
# catchment0_daily_infections = c(1,1,3,4,10,15,16,17,20,25,19,16,13,9,4,1,0,0,0,0,0,0,0,0)
# df_roll = data.frame(
#     day_n = catchment0_daily_infections,
#     min_1 = lag(catchment0_daily_infections, n=1),#, default = 0),
#     min_2 = lag(catchment0_daily_infections, n=2),#, default = 0),
#     min_3 = lag(catchment0_daily_infections, n=3),#, default = 0),
#     min_4 = lag(catchment0_daily_infections, n=4),#, default = 0),
#     min_5 = lag(catchment0_daily_infections, n=5),#, default = 0), 
#     min_6 = lag(catchment0_daily_infections, n=6)#, default = 0)
#     )
# daily_infectious_ppl = apply(df_roll, 1, function(.row) sum(.row, na.rm=T))
# identical(calc_daily_infections(catchment0_daily_infections, 7),daily_infectious_ppl)




generate_outbreak <- function(
    sim_day=NULL,           # current day of simulation time 
    pop_size=NULL,          # total population size of source country
    fix_pop_affected=0.5,   # numeric for proportion of population, everything else = random 
    infectiousness_duration=7 # days
    ){
    # select paho curve
    curve_code <- sample(paho_codes, size = 1)
    curve_code <- "GUF" ################################################################################
    paho_curve <- df_paho_daily_cases$daily_cases[df_paho_daily_cases$code == "PAN"]
    # outbreak size = total infections 
    outbreak_size <- sum(paho_curve)
    # choose a random % population affected (20-60%) 
    if (is.numeric(fix_pop_affected)) { # FIX POP AFFECTED AT THE BEGINNING
        pop_affected <- 0.5 * pop_size ########################################################
    } else { 
        pop_affected <- runif(1, 0.2, 0.6) * pop_size
    }
    # factor to scale up daily infections accordingly
    curve_factor <- pop_affected / outbreak_size
    # new vector of daily infections
    v_daily_infections <- round(paho_curve * curve_factor)
    # vector of daily infectious individuals for travelling 
    v_daily_infectious_ppl <- calc_daily_infections(v_daily_infections, infectiousness_duration)
    ls_outbreak = list(
        v_daily_new_infections = v_daily_infections,
        v_daily_infectious_ppl = v_daily_infectious_ppl, 
        outbreak_info = list(
            outbreak_start = sim_day, 
            outbreak_size = sum(v_daily_infections),
            pop_affected_param = pop_affected,
            pop_size = pop_size,
            paho_curve_code = curve_code
        )
    )
    return(ls_outbreak)

}




# make gifs of saved plots 
# gifs saved and returned by default 
gif_maker = function(
    vec_img_paths,  # vector of full paths to desired images in correct order
    file_name = 'plot_gif', # how to name the gif
    dest_dir = getwd(),     # directory to save gif in 
    .fps=2) {               # frames per second (multiple of 100)
    # read all images into a list  
    img_list <- lapply(vec_img_paths, image_read)
    # join images 
    img_joined <- image_join(img_list)
    # animate at 2 frames per second by default
    img_animated <- image_animate(img_joined, fps = .fps)
    ## save to disk
    img_path = paste0(dest_dir, '/', file_name, '.gif')
    image_write(image = img_animated, path = img_path)
    gc()
    return(img_animated)
}


