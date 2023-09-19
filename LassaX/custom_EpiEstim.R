library(EpiEstim)

###############################################################################
### MODIFY EPI ESTIM TO RETURN DAILY PARAMETER ESTIMATES FOR RT (FROM WHICH ###
### QUANTILES ARE DRAWN AND RETURNED AS MAIN RESULTS)                       ###
###############################################################################

custom_estimate_R_func <- function(incid,
                            si_sample,
                            method = c(
                              "non_parametric_si", "parametric_si",
                              "uncertain_si", "si_from_data", "si_from_sample"
                            ),
                            config) {
  
  #########################################################
  # Calculates the cumulative incidence over time steps   #
  #########################################################
  
  calc_incidence_per_time_step <- function(incid, t_start, t_end) {
    nb_time_periods <- length(t_start)
    incidence_per_time_step <- vnapply(seq_len(nb_time_periods), function(i) 
      sum(incid[seq(t_start[i], t_end[i]), c("local", "imported")]))
    return(incidence_per_time_step)
  }
  
  #########################################################
  # Calculates the parameters of the Gamma posterior      #
  # distribution from the discrete SI distribution        #
  #########################################################
  
  posterior_from_si_distr <- function(incid, si_distr, a_prior, b_prior,
                                      t_start, t_end) {
    nb_time_periods <- length(t_start)
    lambda <- overall_infectivity(incid, si_distr)
    final_mean_si <- sum(si_distr * (seq(0, length(si_distr) -
                                           1)))
    a_posterior <- vector()
    b_posterior <- vector()
    a_posterior <- vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      a_prior + sum(incid[seq(t_start[t], t_end[t]), "local"]) 
      ## only counting local cases on the "numerator"
    }
    else {
      NA
    })
    b_posterior <- vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      1 / (1 / b_prior + sum(lambda[seq(t_start[t], t_end[t])]))
    }
    else {
      NA
    })
    return(list(a_posterior, b_posterior))
  }
  
  #########################################################
  # Samples from the Gamma posterior distribution for a   #
  # given mean SI and std SI                              #
  #########################################################
  
  sample_from_posterior <- function(sample_size, incid, mean_si, std_si,
                                    si_distr = NULL,
                                    a_prior, b_prior, t_start, t_end) {
    nb_time_periods <- length(t_start)
    
    if (is.null(si_distr)) {
      si_distr <- discr_si(seq(0, T - 1), mean_si, std_si)
    }
    
    final_mean_si <- sum(si_distr * (seq(0, length(si_distr) -
                                           1)))
    lambda <- overall_infectivity(incid, si_distr)
    a_posterior <- vector()
    b_posterior <- vector()
    a_posterior <- vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      a_prior + sum(incid[seq(t_start[t], t_end[t]), "local"]) 
      ## only counting local cases on the "numerator"
    }
    else {
      NA
    })
    b_posterior <- vnapply(seq_len(nb_time_periods), function(t) if (t_end[t] >
                                                                     final_mean_si) {
      1 / (1 / b_prior + sum(lambda[seq(t_start[t], t_end[t])], na.rm = TRUE))
    }
    else {
      NA
    })
    sample_r_posterior <- vapply(seq_len(nb_time_periods), function(t) 
      if (!is.na(a_posterior[t])) {
        rgamma(sample_size,
               shape = unlist(a_posterior[t]),
               scale = unlist(b_posterior[t])
        )
      }
      else {
        rep(NA, sample_size)
      }, numeric(sample_size))
    if (sample_size == 1L) {
      sample_r_posterior <- matrix(sample_r_posterior, nrow = 1)
    }
    return(list(sample_r_posterior, si_distr))
  }
  
  method <- match.arg(method)
  incid <- process_I(incid)
  T <- nrow(incid)
  
  a_prior <- (config$mean_prior / config$std_prior)^2
  b_prior <- config$std_prior^2 / config$mean_prior
  
  check_times(config$t_start, config$t_end, T)
  nb_time_periods <- length(config$t_start)
  
  if (method == "si_from_sample") {
    if (is.null(config$n2)) {
      stop("method si_from_sample requires to specify the config$n2 argument.")
    }
    si_sample <- process_si_sample(si_sample)
  }
  
  min_nb_cases_per_time_period <- ceiling(1 / config$cv_posterior^2 - a_prior)
  incidence_per_time_step <- calc_incidence_per_time_step(
    incid, config$t_start,
    config$t_end
  )
  if (incidence_per_time_step[1] < min_nb_cases_per_time_period) {
    warning("You're estimating R too early in the epidemic to get the desired
            posterior CV.")
  }
  
  if (method == "non_parametric_si") {
    si_uncertainty <- "N"
    parametric_si <- "N"
  }
  if (method == "parametric_si") {
    si_uncertainty <- "N"
    parametric_si <- "Y"
  }
  if (method == "uncertain_si") {
    si_uncertainty <- "Y"
    parametric_si <- "Y"
  }
  if (method %in% c("si_from_data", "si_from_sample")) {
    si_uncertainty <- "Y"
    parametric_si <- "N"
  }
  if (si_uncertainty == "Y") {
    if (parametric_si == "Y") {
      mean_si_sample <- rep(-1, config$n1)
      std_si_sample <- rep(-1, config$n1)
      for (k in seq_len(config$n1)) {
        while (mean_si_sample[k] < config$min_mean_si || mean_si_sample[k] >
               config$max_mean_si) {
          mean_si_sample[k] <- rnorm(1,
                                     mean = config$mean_si,
                                     sd = config$std_mean_si
          )
        }
        while (std_si_sample[k] < config$min_std_si || std_si_sample[k] >
               config$max_std_si) {
          std_si_sample[k] <- rnorm(1, mean = config$std_si,
                                    sd = config$std_std_si)
        }
      }
      temp <- lapply(seq_len(config$n1), function(k) sample_from_posterior(config$n2,
                                                                           incid,
                                                                           mean_si_sample[k],
                                                                           std_si_sample[k],
                                                                           si_distr = NULL,
                                                                           a_prior,
                                                                           b_prior, config$t_start, config$t_end
      ))
      config$si_distr <- cbind(
        t(vapply(seq_len(config$n1), function(k) (temp[[k]])[[2]], numeric(T))),
        rep(0, config$n1)
      )
      r_sample <- matrix(NA, config$n2 * config$n1, nb_time_periods)
      for (k in seq_len(config$n1)) {
        r_sample[seq((k - 1) * config$n2 + 1, k * config$n2), 
                 which(config$t_end > mean_si_sample[k])] <- (temp[[k]])[[1]][, 
                                                                              which(config$t_end > mean_si_sample[k])]
      }
      mean_posterior <- colMeans(r_sample, na.rm = TRUE)
      std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
      quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                        0.025,
                                        na.rm = TRUE
      )
      quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                       0.05,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.25,
                                       na.rm = TRUE
      )
      median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
      quantile_0.75_posterior <- apply(r_sample, 2, quantile,
                                       0.75,
                                       na.rm = TRUE
      )
      quantile_0.95_posterior <- apply(r_sample, 2, quantile,
                                       0.95,
                                       na.rm = TRUE
      )
      quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                        0.975,
                                        na.rm = TRUE
      )
    }
    else {
      config$n1 <- dim(si_sample)[2]
      mean_si_sample <- rep(-1, config$n1)
      std_si_sample <- rep(-1, config$n1)
      for (k in seq_len(config$n1)) {
        mean_si_sample[k] <- sum((seq_len(dim(si_sample)[1]) - 1) * 
                                   si_sample[, k])
        std_si_sample[k] <- sqrt(sum(si_sample[, k] * 
                                       ((seq_len(dim(si_sample)[1]) - 1) - 
                                          mean_si_sample[k])^2))
      }
      temp <- lapply(seq_len(config$n1), function(k) sample_from_posterior(config$n2,
                                                                           incid,
                                                                           mean_si = NULL, std_si = NULL, si_sample[, k], a_prior,
                                                                           b_prior, config$t_start, config$t_end
      ))
      config$si_distr <- cbind(
        t(vapply(seq_len(config$n1), function(k) (temp[[k]])[[2]], 
                 numeric(nrow(si_sample)))),
        rep(0, config$n1)
      )
      r_sample <- matrix(NA, config$n2 * config$n1, nb_time_periods)
      for (k in seq_len(config$n1)) {
        r_sample[seq((k - 1) * config$n2 + 1,k * config$n2), which(config$t_end >
                                                                     mean_si_sample[k])] <- (temp[[k]])[[1]][, which(config$t_end >
                                                                                                                       mean_si_sample[k])]
      }
      mean_posterior <- colMeans(r_sample, na.rm = TRUE)
      std_posterior <- apply(r_sample, 2, sd, na.rm = TRUE)
      quantile_0.025_posterior <- apply(r_sample, 2, quantile,
                                        0.025,
                                        na.rm = TRUE
      )
      quantile_0.05_posterior <- apply(r_sample, 2, quantile,
                                       0.05,
                                       na.rm = TRUE
      )
      quantile_0.25_posterior <- apply(r_sample, 2, quantile,
                                       0.25,
                                       na.rm = TRUE
      )
      median_posterior <- apply(r_sample, 2, median, na.rm = TRUE)
      quantile_0.75_posterior <- apply(r_sample, 2, quantile,
                                       0.75,
                                       na.rm = TRUE
      )
      quantile_0.95_posterior <- apply(r_sample, 2, quantile,
                                       0.95,
                                       na.rm = TRUE
      )
      quantile_0.975_posterior <- apply(r_sample, 2, quantile,
                                        0.975,
                                        na.rm = TRUE
      )
    }
  } else {
    # CertainSI
    if (parametric_si == "Y") {
      config$si_distr <- discr_si(seq(0,T - 1), config$mean_si, config$std_si)
    }
    if (length(config$si_distr) < T + 1) {
      config$si_distr[seq(length(config$si_distr) + 1,T + 1)] <- 0
    }
    final_mean_si <- sum(config$si_distr * (seq(0,length(config$si_distr) -
                                                  1)))
    Finalstd_si <- sqrt(sum(config$si_distr * (seq(0,length(config$si_distr) -
                                                     1))^2) - final_mean_si^2)
    post <- posterior_from_si_distr(
      incid, config$si_distr, a_prior, b_prior,
      config$t_start, config$t_end
    )
    a_posterior <- unlist(post[[1]])
    b_posterior <- unlist(post[[2]])
    mean_posterior <- a_posterior * b_posterior
    std_posterior <- sqrt(a_posterior) * b_posterior
    quantile_0.025_posterior <- qgamma(0.025,
                                       shape = a_posterior,
                                       scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.05_posterior <- qgamma(0.05,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.25_posterior <- qgamma(0.25,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    median_posterior <- qgamma(0.5,
                               shape = a_posterior,
                               scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.75_posterior <- qgamma(0.75,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.95_posterior <- qgamma(0.95,
                                      shape = a_posterior,
                                      scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
    quantile_0.975_posterior <- qgamma(0.975,
                                       shape = a_posterior,
                                       scale = b_posterior, lower.tail = TRUE, log.p = FALSE
    )
  }
  
  results <- list(R = as.data.frame(cbind(
    config$t_start, config$t_end, mean_posterior,
    std_posterior, quantile_0.025_posterior, quantile_0.05_posterior,
    quantile_0.25_posterior, median_posterior, quantile_0.75_posterior,
    quantile_0.95_posterior, quantile_0.975_posterior
  )))
  
  non_na_rows <- !is.na(results$R$mean_posterior)
  results$R <- results$R[non_na_rows, ]
  
  names(results$R) <- c(
    "t_start", "t_end", "Mean(R)", "Std(R)",
    "Quantile.0.025(R)", "Quantile.0.05(R)", "Quantile.0.25(R)",
    "Median(R)", "Quantile.0.75(R)", "Quantile.0.95(R)",
    "Quantile.0.975(R)"
  )
  results$method <- method
  results$si_distr <- config$si_distr
  if (is.matrix(results$si_distr)) {
    colnames(results$si_distr) <- paste0("t", seq(0,ncol(results$si_distr) - 1))
  } else {
    names(results$si_distr) <- paste0("t", seq(0,length(results$si_distr) - 1))
  }
  if (si_uncertainty == "Y") {
    results$SI.Moments <- as.data.frame(cbind(
      mean_si_sample,
      std_si_sample
    ))
  } else {
    results$SI.Moments <- as.data.frame(cbind(
      final_mean_si,
      Finalstd_si
    ))
  }
  names(results$SI.Moments) <- c("Mean", "Std")
  
  if (!is.null(incid$dates)) {
    results$dates <- check_dates(incid)
  } else {
    results$dates <- seq_len(T)
  }
  results$I <- rowSums(incid[, c("local", "imported")])
  results$I_local <- incid$local
  results$I_imported <- incid$imported
  
  print(paste0("si_uncertainty = ", si_uncertainty, ", and parametric_si = ", parametric_si))
  
  results$a_posterior <- a_posterior
  results$b_posterior <- b_posterior
  
  # results$test <- rgamma(1000,
  #                        shape = a_posterior,
  #                        scale = b_posterior)
  
  class(results) <- "estimate_R"
  return(results)
}

#################################################################
### ASSIGN THIS UPDATED FUNCTION TO BE CALLED WITHIN EPIESTIM ###
#################################################################

environment(custom_estimate_R_func) <- asNamespace('EpiEstim')
assignInNamespace("estimate_R_func", custom_estimate_R_func, ns = "EpiEstim")


