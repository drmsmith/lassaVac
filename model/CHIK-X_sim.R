library('tidyverse')
conflicts_prefer(
    dplyr::filter(),
    .quiet = T
)

source('model/utils.R')


# define simulation parameters
set.seed(31124)
# unlink(res_dir)
res_dir = './res/importation_model_par'     # save results in this dir 
n_simulations = 100                      # number of simulations
duration_spread <- 365*2                # simulation time span
infectiousness_duration <- 7    # infected individual can infect days

# these factors transform 
# pop-weighted suitability
# f * suit ^ k 
fact_f = 0.1
fact_k = 4.5

# initialise results dir 
if (!dir.exists(res_dir)) {dir.create(res_dir, recursive = T)}


# suitability (instead of estd. infections)
# suitability info for 220 countries
df_burden <- read.csv("data/df_suit_means_pop_wght_pop_size_who_regions.csv")

# mobility data (daily trips between src and dest)
# mobility info for 188 countries
mat_mob_daily_trips <- read.csv("data/df_mat_mob_n_daily_trips.csv")
all_codes <- colnames(mat_mob_daily_trips)
rownames(mat_mob_daily_trips) <- all_codes

# ensure mobility data and suitability data are matched 
# since there are fewer countries on the mobility data set 
# 220 vs 188 (suit vs mobility)
# final number is 184 for some reason
df_burden <- filter(df_burden, country_code %in% all_codes) %>% drop_na()


#########################
# transform suitability #
#########################
df_burden$mean_pop_wght_transfrm = fact_f * df_burden$mean_pop_weighted ^ fact_k

# paho case data
df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
paho_codes <- df_paho_outbreak_sizes$code


#########################
#### PARALLELISATION ####
#########################

library('doParallel')
library('foreach')
library('progress')

f <- function(iterator){
  pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
  count <- 0
  function(...) {
    count <<- count + length(list(...)) - 1
    setTxtProgressBar(pb, count)
    flush.console()
    list(...) # this can feed into .combine option of foreach
  }
}


totalCores = detectCores()
# cl <- makeCluster(totalCores[1]-1, type='SOCK')
cl <- parallel::makeCluster(4, type='PSOCK')
registerDoParallel(cl)
clusterEvalQ(cl,  library('tidyverse'))
clusterEvalQ(cl,  source('model/utils.R'))
# clusterEvalQ(cl, set.seed(31124))




###############################
# start simulation, init cond #
###############################


start.time.total <- Sys.time()

# for (sim_i in 1:n_simulations) {
foreach(sim_i = icount(n_simulations), .combine = f(n_simulations)) %dopar% {
    cat(paste0("Simulation ", sim_i, "\n", collapse = ""))
    start.time <- Sys.time()


    # matrix of spread across catchments
    # (to be updated in loop with spread model)
    m_spread <- matrix(0, nrow = nrow(df_burden), ncol = duration_spread)
    colnames(m_spread) <- 1:duration_spread
    rownames(m_spread) <- df_burden$country_code

    # starting catchment
    # identify first catchment, add to vector of infected catchments, update spread matrix
    catchment0 <- sample(df_burden$country_code, size = 1, prob = df_burden$p_spillover)
    catchment0_pop_size <- df_burden$pop_size[df_burden$country_code == catchment0]

    # initialise vec_catchments_infected
    vec_catchments_infected <- catchment0
    # fill rest of row with 1 (assumption is that catchments do not return to susceptible)
    m_spread[catchment0, ] <- 1

    # subset relevant data
    df_catchment0 <- df_burden[
        df_burden$country_code == catchment0,
        c("country_name", "country_code", "region_name", "region_code", "pop_size")
    ]


    # generate outbreak data
    outbreak_0 <- generate_outbreak(
        sim_day = 0,
        df_country_data = df_catchment0,
        fix_pop_affected = NULL,
        infectiousness_duration = infectiousness_duration,
        sim_i = sim_i
    )

    # list$'country code' = ls_outbreak_info
    ls_outbreaks <- setNames(list(outbreak_0), catchment0)


    # go through outbreak and update matrices day-by-day
    for (day_i in 1:duration_spread) {
        # cat(paste0("Day ", day_i, "\n", collapse = ""))
        # for each catchment currently infected
        for (catchment_infect_j in vec_catchments_infected) {
            outbrk <- ls_outbreaks[[catchment_infect_j]] # get outbreak data and info

            # n_infectious people on day_i from source country
            n_infectious <- outbrk$v_daily_new_infections[day_i - outbrk$df_res_summary$timing]
            # population size, source country
            pop_size <- outbrk$df_res_summary$pop_size
            # proportion of infectious population
            prop_infectious <- n_infectious / pop_size
            # all catchments not currently infected
            uninfected_ccodes <- df_burden$country_code[!df_burden$country_code %in% vec_catchments_infected]
            # filter out ccodes with no travel between countries to save comp time
            no_travel <- all_codes[mat_mob_daily_trips[catchment_infect_j, ] == 0]
            # leave countries not yet infected but where there is travel between source and dest
            uninfected_travel_ccodes <- uninfected_ccodes[!uninfected_ccodes %in% no_travel]

            # evaluate probability of spread and outbreak to all catchments not currently infected
            for (catchment_suscept_k in uninfected_travel_ccodes) {
                # p infectious moving a -> b
                n_travellers <- mat_mob_daily_trips[catchment_infect_j, catchment_suscept_k]
                n_infectious_travellers <- n_travellers * prop_infectious
                prop_infectious_traveller <- n_infectious_travellers / n_travellers

                ##### CALCULATE P INFECTIOUS PERSON TRAVELLING #####
                ####################################################
                # probability of an infected person travelling
                travel_infect_i_j <- rbinom(1, 1, prop_infectious_traveller)

                # probability of outbreak starting = suitability
                ####################################################### CHANGE BACK TO POP WEIGHTED
                p_outbreak <- df_burden$mean_pop_wght_transfrm[df_burden$country_code == catchment_suscept_k]
                outbreak_i_j <- rbinom(1, 1, p_outbreak)


                # if infected travels AND starts outbreak
                # update infected catchments and corresponding m_spread matrix
                if ((travel_infect_i_j == 1) & (outbreak_i_j == 1)) {
                    # add to infected countries
                    vec_catchments_infected <- append(
                        vec_catchments_infected,
                        catchment_suscept_k
                    )
                    # indicate start date
                    m_spread[catchment_suscept_k, day_i:ncol(m_spread)] <- 1

                    # subset relevant data
                    df_suscept_country <- df_burden[
                        df_burden$country_code == catchment_suscept_k,
                        c("country_name", "country_code", "region_name", "region_code", "pop_size")
                    ]

                    # generate outbreak data
                    new_outbreak <- generate_outbreak(
                        sim_day = day_i, df_country_data = df_suscept_country,
                        fix_pop_affected = NULL, infectiousness_duration = infectiousness_duration,
                        sim_i = sim_i
                    )
                    # append as 'country code' = ls_outbreak_info
                    ls_outbreaks <- append(
                        ls_outbreaks, setNames(list(new_outbreak), catchment_suscept_k)
                    )
                }
            }
        }
    }



    filename <- paste0(res_dir, "/simulation_", sim_i, ".RDS", collapse = "")
    saveRDS(ls_outbreaks, file = filename)

    end.time <- Sys.time()
    time.taken <- round(end.time - start.time, 2)
    print(time.taken)
}


stopCluster(cl)

cat(paste('Simulations saved in "', res_dir, '".\n', sep = ""))
end.time.total <- Sys.time()
time.taken <- round(end.time.total - start.time.total, 2)
print(time.taken)


# str(ls_outbreaks)
