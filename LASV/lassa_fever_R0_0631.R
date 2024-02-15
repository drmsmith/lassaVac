# Lassa fever stochastic branching process.
# Extra lines removed and close statement added to prevent "closing unused connection" message.
# Number of spillovers per day also recorded.
# R0 = 0.0631
# Different number of spillovers per year for each catchment is possible.
#     Just need to supply a different number for each year in the input file.
# Different number of spillovers per simulation used for each catchment.
# max_spillovers = 113,000 (i.e. more than maximum in incidence samples, so that there is no cap).

rm(list=ls())

suppressPackageStartupMessages(library(zoo))
library(data.table)
#library(progress) # needed for progress bar, which has been disabled
library(parallel)
#library(tidyverse) # unavailable on Condor, so individual packages installed separately
library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)

#######################################
########## CATCHMENT ##################
#######################################

generate_outbreak <- function(R0, cap) {
  outbreak <- list()
  to_resolve <- c(1)
  previous_used_index <- 1
  n_infected_arr <- rpois(cap, R0)
  is_runaway <- FALSE
  while (length(to_resolve) > 0) {
    index <- to_resolve[[1]]
    to_resolve <- to_resolve[-1]

    n_infected <- n_infected_arr[index]
    if (previous_used_index + n_infected >= cap) {
      is_runaway <- TRUE
      n_children <- 0
    } else {
      n_children <- n_infected
    }

    new_children <- numeric(0)
    if (n_children > 0) {
      new_children <- previous_used_index + 1:n_children
      previous_used_index <- previous_used_index + n_children
    }
    outbreak[[index]] <- new_children

    to_resolve <- c(to_resolve, new_children)
  }
  return(list(outbreak = outbreak, is_runaway = is_runaway))
}

outbreak_to_unsorted_times <- function(time_params, outbreak, first_infection) {
  n_nodes <- length(outbreak)
  times_list <- numeric(n_nodes)
  times_list[1] <- first_infection
  stack <- c(1)

  incubation_times <- rgamma(n_nodes, shape = time_params$shape_incubation, scale = time_params$scale_incubation)
  infection_deltas <- rgamma(n_nodes, shape = time_params$shape_death, scale = time_params$scale_death)
  uniform_delay_multipliers <- runif(n_nodes)

  while (length(stack) > 0) {
    index <- stack[length(stack)]
    stack <- stack[-length(stack)]
    children <- outbreak[[index]]

    if (length(children) > 0) {
      start_infectious <- times_list[index] + incubation_times[index]
      times_list[children] <- start_infectious + infection_deltas[index] * uniform_delay_multipliers[children]
      stack <- c(stack, children)
    }
  }
  return(times_list)
}

get_totals <- function(arr, times) {
  if (is.null(arr)) {
    return(NULL)
  }
  below_time_arr <- arr[arr <= max(times)]
  hist_data <- hist(
    below_time_arr,
    breaks = times,
    plot = FALSE
  )
  totals <- cumsum(hist_data$counts)
  return(totals)
}

get_catchment <- function(
	sim,
    n_years,
    spillovers,
	max_spillovers,
    time_params,
    R0,
    outbreak_cap,
    start_time = 0,
    sampling_rate = 1) {
  duration <- 365
  sample_times <- seq(from = start_time, to = start_time + duration * n_years, by = sampling_rate)
  infection_times <- c()
  outbreaks <- list()
  outbreak_lengths <- vector("integer")
  outbreak_idx <- 1
  runaway_times <- c()

  all_first_infected <- c()
  for (year in 1:n_years) {
	n_out <- spillovers[[year+(n_years*(sim-1))]]
	if (!is.null(max_spillovers)) {
		n_outbreaks <- min(n_out, max_spillovers)
	} else {
		n_outbreaks <- n_out
	}
	outbreak_lengths <- c(outbreak_lengths, n_outbreaks)
    first_infected_times <- sort(
      rbeta(n_outbreaks, time_params$shape1_infec, time_params$shape2_infec) * duration
        + (year - 1) * duration
    )
    all_first_infected <- c(all_first_infected, first_infected_times)
  }
  for (first_infected_time in all_first_infected) {
    o <- generate_outbreak(R0, outbreak_cap)
    outbreak <- o$outbreak
    outbreaks[[outbreak_idx]] <- outbreak
    is_runaway <- o$is_runaway
    if (is_runaway) {
      runaway_times <- c(runaway_times, first_infected_time)
    }
    outbreak_unsorted_time_of_infection <- outbreak_to_unsorted_times(time_params, outbreak, first_infected_time)
    infection_times <- c(infection_times, outbreak_unsorted_time_of_infection)
    outbreak_lengths[outbreak_idx] <- length(outbreak_unsorted_time_of_infection)
    outbreak_idx <- outbreak_idx + 1
  }

  unpruned_infection_times <- infection_times
  spillover_infection_times <- all_first_infected

  return(list(
    times = sample_times,
    unpruned = get_totals(unpruned_infection_times, sample_times),
	spillover = get_totals(spillover_infection_times, sample_times),
    runaways = get_totals(runaway_times, times = sample_times)
  ))
}



#######################################
########## LASSA FEVER ################
#######################################

csv_to_catchment_data <- function(file_path, n_years, sims_per_catchment) {
  df <- data.table::fread(file_path, check.names = TRUE)
  column_names <- names(df)

  data <- list()
  j <- 1
  for (i in seq_len(nrow(df))) {
    catchment_row <- df[i, ]
    if (all(!is.na(catchment_row[,7:((n_years*sims_per_catchment)+6)]))) {
      data[[j]] <- list(
        identifier = catchment_row$GID_1,
        name = catchment_row$NAME_1,
        n_spillovers = round(catchment_row[,7:((n_years*sims_per_catchment)+6)]),
        start_year = catchment_row$Year
      )
      j <- j + 1
    }
  }
  return(data)
}


lassa_fever <- function(
    input_file,
    output_file,
    params,
    sims_per_catchment,
    n_catchments = NULL,
    processes = 1,
    max_spillovers = NULL) {
  input_data <- csv_to_catchment_data(input_file,
                                      n_years = params$n_years,
                                      sims_per_catchment)

  if (is.null(n_catchments)) {
    new_data <- input_data
  } else {
    new_data <- input_data[1:n_catchments]
  }

  output_data <- list()
  for (dat in new_data) {

	spillovers <- dat$n_spillovers
    starts <- (dat$start_year:10 - 1) * 365
    sims_out <- mclapply(
      1:sims_per_catchment, function(i) {
        cat <- get_catchment(
		  sim = i,
          n_years = params$n_years,
          spillovers = spillovers,
		  max_spillovers = max_spillovers,
          time_params = params$time_params,
          R0 = params$R0,
          outbreak_cap = params$outbreak_cap
        )
      }
    )
    out <- list()
    j <- 1
    run_names <- character()
    for (i in sims_out) {
      run_name <- paste0("run", j)
      run_names <- c(run_names, run_name)
      for (name in names(i)) {
        if (name %in% names(out)) {
          current_attr <- out[[name]]
        } else {
          current_attr <- list()
        }
        current_attr[[run_name]] <- i[[name]]
        out[[name]] <- current_attr
      }
      j <- j + 1
    }
    transform_data <- function(data, column_names) {
      df <- do.call(rbind, lapply(names(data), function(name) {
        n_reps <- length(data[[name]][[column_names[1]]])
        t <- data.frame(
          identifier = rep(dat$identifier, each = n_reps),
          name = rep(dat$name, each = n_reps),
          measure = rep(name, each = n_reps)
        )
        t[column_names] <- data[[name]][column_names]
        return(t)
      }))
    }
    df <- transform_data(out, run_names)
    output_data <- c(output_data, list(df))
    #pb$tick()
  }
  combined_df <- do.call(rbind, output_data)
  f <- file(output_file, "w")

  write.csv(combined_df, file = output_file, row.names = FALSE, quote = FALSE)
  close(f)
}



#######################################
########## R0 = 0.0631 ################
#######################################

time_params <- list(
  shape1_infec = 9.532623,
  shape2_infec = 6.438107,
  shape_incubation = 11.1191707,
  scale_incubation = 0.9224181,
  shape_death = 1.862467,
  scale_death = 6.0729
)

params <- list(
  n_years = 10,
  time_params = time_params,
  R0 = 0.0631,
  outbreak_cap = 50
)

lassa_fever(
  input_file = "input.csv",
  output_file = "output.csv",
  params = params,
  sims_per_catchment = 1,
  processes = 1,
  max_spillovers = 113000
)
