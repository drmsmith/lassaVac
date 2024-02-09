library('magick', quietly = T, warn.conflicts = F)
library('tidyverse')
library('conflicted')
library('jsonlite')
conflicts_prefer(
    dplyr::filter(),
    stats::lag(),
    .quiet = T
)



# # paho case data
# df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
# df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
# paho_codes <- df_paho_outbreak_sizes$code


# return the vector of the number of daily infectious individuals 
# numeric of length v_daily_infections
calc_daily_infections <- function(
    v_daily_infections, # numeric vector
    data_files,
    infectiousness_duration = 7, # integer
    .outbreak_cutoff_days = 365 * 2
    ) {
    df_paho_daily_cases <- data_files$df_paho_daily_cases
    df_paho_outbreak_sizes <- data_files$df_paho_outbreak_sizes
    paho_codes <- df_paho_outbreak_sizes$code

    n_lags <- infectiousness_duration - 1 # sum today and infectiousness_duration-1 days
    df_lags <- map( # returns list of lagged vectors
        1:n_lags,
        function(.lag_dur) dplyr::lag(v_daily_infections, n = .lag_dur)
    )
    # change names to suppress error messages
    names(df_lags) <- paste0("lag_", 1:n_lags)
    df_lags <- bind_cols(df_lags) # convert to df
    df_lags <- as.data.frame(list(daily = v_daily_infections, df_lags)) # combine together
    # calc. daily infectious individuals based on duration of infectiousness
    daily_infectious_ppl <- apply(df_lags, 1, function(.row) sum(.row, na.rm = T))
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


# generate_outbreak_by_pop_size <- function(
#     sim_day = NULL, # current day of simulation time
#     df_country_data = NULL, # total population size of source country
#     sim_i = NULL, # simulation number/index
#     fix_pop_affected = NULL, # numeric for proportion of population, everything else = random
#     infectiousness_duration = 7 # days
#     ) {
#     pop_size <- df_country_data$pop_size
#     # select paho curve
#     curve_code <- sample(paho_codes, size = 1)
#     paho_curve <- df_paho_daily_cases$daily_cases[df_paho_daily_cases$code == curve_code]
#     # outbreak size = total infections
#     outbreak_size <- sum(paho_curve)
#     # choose a random % population affected (20-60%)
#     if (!is.null(fix_pop_affected) & is.numeric(fix_pop_affected)) { 
#         # pop affected is percentage times population size
#         pop_affected <- fix_pop_affected * pop_size
#     } else {
#         pop_affected <- runif(1, 0.2, 0.6) * pop_size
#     }
#     # factor to scale up daily infections accordingly
#     curve_factor <- pop_affected / outbreak_size
#     # new vector of daily infections
#     v_daily_infections <- round(paho_curve * curve_factor)
#     # vector of daily infectious individuals for travelling
#     v_daily_infectious_ppl <- calc_daily_infections(v_daily_infections, infectiousness_duration)

#     # long format df with all daily infections 
#     # and additional info for plotting / summary  
#     df_res_curve <- data.frame(daily_infections_sim = v_daily_infections) %>%
#         mutate(
#             country = df_country_data$country,
#             code = df_country_data$code,
#             region_name = df_country_data$region_name,
#             region_code = df_country_data$region_code,
#             pop_size = df_country_data$pop_size,
#             simulation = sim_i,
#             time_days = (1:length(v_daily_infections)) + (sim_day - 1),
#             time_years = time_days / 365.25,
#             timing = sim_day,
#             IncCumul_U_final = sum(v_daily_infections)
#         )

#     # one-line df with all params relating to sim 
#     # and additional info e.g. totals for plotting / summary 
#     df_res_summary <- df_res_curve[1, c(
#         'country', 'code', 'region_name', 'region_code',
#         'pop_size', 'simulation', 'timing', 'IncCumul_U_final'
#         )] %>%
#         mutate(
#             duration = length(v_daily_infections) / 365.25,
#             paho_curve_code = curve_code,
#             prop_pop_affected = sum(v_daily_infections) / df_country_data$pop_size 
#         )
#     # return lst of 
#     # vector of daily infectious people + the two dfs
#     ls_outbreak <- list(
#         v_daily_new_infections = v_daily_infectious_ppl,
#         df_res_curve = df_res_curve,
#         df_res_summary = df_res_summary
#     )

#     return(ls_outbreak)
# }



# creates a unique id based on date-time
# returns string "YYMMDD_HHMMSS"
get_dt_id <- function() {
    id <- as.character(Sys.time()) %>%
        substring(3, 19) %>%
        str_replace_all(":", "") %>%
        str_replace_all("-", "") %>%
        str_replace_all(" ", "_")
    return(id)
}


write_log_json <- function(
    simulation_hyperparameters,   # hyper params given at start 
    dest_dir = NULL        # where to save log
    ) {
    # save to dest_dir
    id <- get_dt_id()
    simulation_hyperparameters$id = id 
    params_json <- toJSON(simulation_hyperparameters)
    fname = paste0('sim_params_', id, '.json')
    destpath = file.path(dest_dir, fname)
    write(toJSON(params_json), destpath)
}



### TO ADJUST RANGE FOR RANDOM GENERATION GO TO 
# pop_affected <- runif(1, 0.2, 0.6) * annual_incidence
generate_outbreak_by_annual_incidence <- function(
    sim_day = NULL, # current day of simulation time
    df_country_data = NULL, # total population size of source country
    sim_i = NULL, # simulation number/index
    prop_adj = 1, # numeric for proportion of population, everything else = random
    infectiousness_duration = 7, # days
    data_files
    ) {
    df_paho_daily_cases <- data_files$df_paho_daily_cases
    df_paho_outbreak_sizes <- data_files$df_paho_outbreak_sizes
    paho_codes <- df_paho_outbreak_sizes$code

    pop_size <- df_country_data$pop_size
    annual_incidence <- df_country_data$annual_incidence
    # select paho curve
    curve_code <- sample(paho_codes, size = 1)
    paho_curve <- df_paho_daily_cases$daily_cases[df_paho_daily_cases$code == curve_code]
    # outbreak size = total infections (for PAHO data)
    outbreak_size <- sum(paho_curve)
    # adjust simulated outbreak size 
    # pop affected is percentage times population size
    pop_affected <- prop_adj * annual_incidence
    # factor to scale up daily infections accordingly
    curve_factor <- pop_affected / outbreak_size
    # new vector of daily infections
    v_daily_infections <- round(paho_curve * curve_factor)
    # vector of daily infectious individuals for travelling
    v_daily_infectious_ppl <- calc_daily_infections(
        v_daily_infections=v_daily_infections, 
        infectiousness_duration=infectiousness_duration,
        data_files=data_files
        )


    # long format df with all daily infections 
    # and additional info for plotting / summary  
    df_res_curve <- data.frame(daily_infections_sim = v_daily_infections) %>%
        mutate(
            country = df_country_data$country,
            code = df_country_data$code,
            region_name = df_country_data$region_name,
            region_code = df_country_data$region_code,
            pop_size = df_country_data$pop_size,
            annual_incidence = df_country_data$annual_incidence,
            simulation = sim_i,
            time_days = (1:length(v_daily_infections)) + (sim_day - 1),
            time_years = time_days / 365.25,
            timing = sim_day,
            IncCumul_U_final = sum(v_daily_infections)
        )

    # one-line df with all params relating to sim 
    # and additional info e.g. totals for plotting / summary 
    df_res_summary <- df_res_curve[1, c(
        'country', 'code', 'region_name', 'region_code',
        'pop_size', 'annual_incidence', 'simulation', 'timing', 'IncCumul_U_final'
        )] %>%
        mutate(
            duration = length(v_daily_infections) / 365.25,
            paho_curve_code = curve_code,
            prop_pop_affected = sum(v_daily_infections) / df_country_data$pop_size 
        )
    # return lst of 
    # vector of daily infectious people + the two dfs
    ls_outbreak <- list(
        v_daily_new_infections = v_daily_infectious_ppl,
        df_res_curve = df_res_curve,
        df_res_summary = df_res_summary
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


