library('tidyverse')
library('RcppRoll')

source('model/utils.R')

# define simulation parameters
set.seed(31124)
res_dir = './res/new_model'     # save results in this dir 
n_simulations = 1               # number of simulations
duration_spread <- 365          # simulation time span
infectiousness_duration <- 7    # infected individual can infect days

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

# paho case data
df_paho_daily_cases <- read.csv("data/df_paho_daily_cases.csv")
df_paho_outbreak_sizes <- read.csv("data/df_paho_outbreak_sizes.csv")
paho_codes <- df_paho_outbreak_sizes$code


###############################
# start simulation, init cond #
###############################


start.time <- Sys.time()


# matrix of spread across catchments
# (to be updated in loop with spread model)
m_spread <- matrix(0, nrow = nrow(df_burden), ncol = duration_spread)
colnames(m_spread) <- 1:duration_spread
rownames(m_spread) <- df_burden$country_code

# starting catchment
# identify first catchment, add to vector of infected catchments, update spread matrix
catchment0 <- sample(df_burden$country_code, size = 1, prob = df_burden$p_spillover)
catchment0 <- "IDN" #################################################################################
catchment0_pop_size <- df_burden$pop_size[df_burden$country_code == catchment0]

# initialise vec_catchments_infected
vec_catchments_infected <- catchment0
# fill rest of row with 1 (assumption is that catchments do not return to susceptible)
m_spread[catchment0, ] <- 1

# generate outbreak data
outbreak_0 <- generate_outbreak(
    sim_day = 0, 
    pop_size = catchment0_pop_size, 
    fix_pop_affected = 0.5, 
    infectiousness_duration=infectiousness_duration
)
# list$'country code' = ls_outbreak_info
ls_outbreaks <- setNames(list(outbreak_0), catchment0)


# go through outbreak and update matrices day-by-day
for (day_i in 1:duration_spread) {
    cat(paste0('Day ', day_i, '\n', collapse=''))
    # for each catchment currently infected
    for (catchment_infect_j in vec_catchments_infected) {
        # print(paste0("on day ", date_i, ", evaluating potential spread from ", catchment_infect_j))
        outbrk <- ls_outbreaks[[catchment_infect_j]] # get outbreak data and info
        # n_infectious people on day_i from source country
        n_infectious <- outbrk$v_daily_new_infections[day_i - outbrk$outbreak_info$outbreak_start]
        # population size, source country
        pop_size <- outbrk$outbreak_info$pop_size
        # proportion of infectious population
        prop_infectious <- n_infectious / pop_size
        # evaluate probability of spread to all catchments not currently infected
        uninfected_ccodes = df_burden$country_code[!df_burden$country_code %in% vec_catchments_infected]
        # filter out ccodes with no travel between countries to save comp time 
        no_travel = all_codes[mat_mob_daily_trips[catchment_infect_j, ] == 0]
        # leave countries not yet infected but where there is travel between source and dest 
        uninfected_travel_ccodes = uninfected_ccodes[!uninfected_ccodes %in% no_travel]
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
            p_outbreak <- df_burden$mean_pop_weighted[df_burden$country_code == catchment_suscept_k]
            outbreak_i_j <- rbinom(1, 1, p_outbreak)

            # print(paste('Travel ', travel_infect_i_j, ', Outbreak ',  outbreak_i_j, sep=''))
            # print(paste(n_travellers))

            
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
                new_pop_size <- df_burden$pop_size[df_burden$country_code == catchment_suscept_k]
                # generate outbreak data
                new_outbreak <- generate_outbreak(
                    sim_day = day_i, pop_size = new_pop_size, 
                    fix_pop_affected = 0.5, infectiousness_duration = infectiousness_duration
                )
                # append as 'country code' = ls_outbreak_info
                ls_outbreaks <- append(
                    ls_outbreaks, setNames(list(new_outbreak), catchment_suscept_k)
                    )
            }
        }
    }
}



filename = paste0(res_dir, '/test_sim_p_outbr_suit.RDS', collapse='')
saveRDS(ls_outbreaks, file=filename)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)

cat(paste('Simulations saved in "', res_dir, '".\n',  sep=''))
print(time.taken)



outbr_dur_country = apply(m_spread, 1, sum) %>% .[.!=0] %>% sort(decreasing = T) #%>% length

barplot(outbr_dur_country, las=2, ylab='Duration / days')
