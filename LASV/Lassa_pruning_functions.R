
Lassa_pruning_function <- function(inputdata, efficacy, R0) {
  
  #create non-cumulative versions of unpruned, spillover and h2h cases
  tempdata <- c(0,catchmentdata$unpruned[1:(nrow(catchmentdata)-1)])
  catchmentdata$unpruned_nc <- catchmentdata$unpruned - tempdata
  tempdata2 <- c(0,catchmentdata$spillover[1:(nrow(catchmentdata)-1)])
  catchmentdata$spillover_nc <- catchmentdata$spillover - tempdata2
  catchmentdata$h2h_nc <- catchmentdata$unpruned_nc - catchmentdata$spillover_nc
  
  #create vector of vac_Frac_prev (preventive vaccination coverages) for all years
  catchment_vac_Frac_prev <- c(catchmentdata$vacFrac_prev_0[1], catchmentdata$vacFrac_prev_1[1], catchmentdata$vacFrac_prev_2[1], 0,0,0,0,0,0,0)
  catchment_vac_Frac_prev[is.na(catchment_vac_Frac_prev)] <- 0
  
  for(year in 1:10) {
  
  #extract data for this year
  catchmentdata_year <- filter(catchmentdata, catchmentdata$realyear == year)
  
  if (year == 1) {
    
    #apply preventive vaccination
    catchmentdata_year$cases_averted_prev <- catchmentdata_year$unpruned_nc * catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_averted_prevH <- catchmentdata_year$h2h_nc * catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_vacc_prev <- catchmentdata_year$unpruned_nc * catchment_vac_Frac_prev[year]
	
	#onward transmissions
	catchmentdata_year$averted <- catchmentdata_year$cases_averted_prev
	catchmentdata_year$total_averted <- catchmentdata_year$cases_averted_prev
	proportion_protected <- catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_onward_prevH_tmp1 <- catchmentdata_year$total_averted * R0 * (1 - proportion_protected)
	catchmentdata_year$cases_onward_prevH_tmp2 <- round(catchmentdata_year$cases_onward_prevH_tmp1)
	catchmentdata_year$cases_onward_prevH <- catchmentdata_year$cases_onward_prevH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_prevH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_prevH_tmp1[j] <- catchmentdata_year$cases_onward_prevH_tmp1[j] * R0 * (1 - proportion_protected)
			catchmentdata_year$cases_onward_prevH_tmp2[j] <- round(catchmentdata_year$cases_onward_prevH_tmp1[j])
			catchmentdata_year$cases_onward_prevH[j] <- catchmentdata_year$cases_onward_prevH[j] + catchmentdata_year$cases_onward_prevH_tmp1[j]
		}
	}
    
    #calculate remaining cases after preventive vaccination and onward transmissions removed
	catchmentdata_year$total_avertedH <- catchmentdata_year$cases_averted_prevH
    catchmentdata_year$remainingcasesU1 <- pmax(0, (catchmentdata_year$unpruned_nc - catchmentdata_year$total_averted - catchmentdata_year$cases_onward_prevH))
	catchmentdata_year$remainingcasesH1 <- pmax(0, (catchmentdata_year$h2h_nc - catchmentdata_year$total_avertedH - catchmentdata_year$cases_onward_prevH))
	
    #apply reactive vaccination if threshold of 50 cases over 28 day period is exceeded
	previous_values <- rep(0,27) # previous values for rolling sum (zero in year 1)
	padded_data <- c(previous_values, catchmentdata_year$remainingcasesU1)
	rolling_sum <- rollsum(padded_data, 28, align = "left")
    threshold_index <- which(rolling_sum >= 50)
    if (length(threshold_index)==0) {
      reactive_start <- nrow(catchmentdata_year)
    } else {
      reactive_start <- threshold_index[1]
    }
    catchmentdata_year$vacFrac_reac[1:reactive_start] <- 0
    catchmentdata_year$cases_averted_reac <- catchmentdata_year$unpruned_nc * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_averted_reacH <- catchmentdata_year$h2h_nc * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_vacc_reac <- catchmentdata_year$unpruned_nc * catchmentdata_year$vacFrac_reac
	
	#re-calculate onward transmissions
	catchmentdata_year$averted <- catchmentdata_year$cases_averted_prev + catchmentdata_year$cases_averted_reac
	catchmentdata_year$total_averted <- catchmentdata_year$cases_averted_prev + catchmentdata_year$cases_averted_reac
	proportion_protected <- (catchment_vac_Frac_prev[year] * efficacy) + (catchmentdata_year$vacFrac_reac * efficacy)
	catchmentdata_year$cases_onward_total_tmp1 <- catchmentdata_year$total_averted * R0 * (1 - proportion_protected)
	catchmentdata_year$cases_onward_total_tmp2 <- round(catchmentdata_year$cases_onward_total_tmp1)
	catchmentdata_year$cases_onward_total <- catchmentdata_year$cases_onward_total_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_total_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_total_tmp1[j] <- catchmentdata_year$cases_onward_total_tmp1[j] * R0 * (1 - proportion_protected[j])
			catchmentdata_year$cases_onward_total_tmp2[j] <- round(catchmentdata_year$cases_onward_total_tmp1[j])
			catchmentdata_year$cases_onward_total[j] <- catchmentdata_year$cases_onward_total[j] + catchmentdata_year$cases_onward_total_tmp1[j]
		}
	}
	
	#re-calculate remaining cases after preventive and reactive vaccination and onward transmissions removed
	catchmentdata_year$total_avertedH <- catchmentdata_year$cases_averted_prevH + catchmentdata_year$cases_averted_reacH
    catchmentdata_year$remainingcasesU2 <- pmax(0, (catchmentdata_year$unpruned_nc - catchmentdata_year$total_averted - catchmentdata_year$cases_onward_total))
	catchmentdata_year$remainingcasesH2 <- pmax(0, (catchmentdata_year$h2h_nc - catchmentdata_year$total_avertedH - catchmentdata_year$cases_onward_total))
    
	#calculate pruned, vaccinated and total_vaccinated (N.B. averted = directly protected and hence immune, vaccinated = vaccinated but not necessarily protected)
    catchmentdata_year$Newpruned_nc <- catchmentdata_year$remainingcasesU2
	catchmentdata_year$vaccinated <- catchmentdata_year$cases_vacc_prev + catchmentdata_year$cases_vacc_reac
	catchmentdata_year$total_vaccinated <- catchmentdata_year$cases_vacc_prev + catchmentdata_year$cases_vacc_reac
	
    #save total coverage for immunity pruning next year
	if (reactive_start == nrow(catchmentdata_year)) {
		total_coverage_endyear <- catchment_vac_Frac_prev[year]
	} else {
		total_coverage_endyear <- catchment_vac_Frac_prev[year] + catchmentdata_year$vacFrac_reac[nrow(catchmentdata_year)]
	}
	
	#save last 27 pruned values for rolling sum next year
	previous_values_endyear <- catchmentdata_year$Newpruned_nc[339:365]
	
    #save results
    Lassa_results <<- tibble(scenario = catchmentdata_year$Scenario, country = catchmentdata_year$COUNTRY, catchmentID = catchmentdata_year$identifier,
							 run = catchmentdata_year$Run, realyear = year, unpruned = catchmentdata_year$unpruned, unpruned_nc = catchmentdata_year$unpruned_nc,
							 spillover = catchmentdata_year$spillover, spillover_nc = catchmentdata_year$spillover_nc,
							 h2h_nc = catchmentdata_year$h2h_nc, pruned_nc = catchmentdata_year$Newpruned_nc,
							 averted = catchmentdata_year$averted, total_averted = catchmentdata_year$total_averted,
							 vaccinated = catchmentdata_year$vaccinated, total_vaccinated = catchmentdata_year$total_vaccinated,
							 averted_imm = rep(0,365), averted_reac = catchmentdata_year$cases_averted_reac,
							 averted_prev = catchmentdata_year$cases_averted_prev, averted_onward = catchmentdata_year$cases_onward_total)
  
  } else {
        
    #All years following Year 1
	
	#coverage for immunity pruning
	Newcoverage_endyear <- total_coverage_endyear * efficacy
	Newcoverage_endyear_vacc <- total_coverage_endyear
	
    #apply immunity
    catchmentdata_year$cases_averted_immunity <-  catchmentdata_year$unpruned_nc * Newcoverage_endyear
	catchmentdata_year$cases_averted_immunityH <-  catchmentdata_year$h2h_nc * Newcoverage_endyear
	catchmentdata_year$cases_vacc_immunity <-  catchmentdata_year$unpruned_nc * Newcoverage_endyear_vacc
	
    #apply preventive vaccination
    catchmentdata_year$cases_averted_prev <- catchmentdata_year$unpruned_nc * catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_averted_prevH <- catchmentdata_year$h2h_nc * catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_vacc_prev <- catchmentdata_year$unpruned_nc * catchment_vac_Frac_prev[year]
	
	#onward transmissions
	catchmentdata_year$averted <- catchmentdata_year$cases_averted_prev
	catchmentdata_year$total_averted <- catchmentdata_year$cases_averted_immunity + catchmentdata_year$cases_averted_prev
	proportion_protected <- Newcoverage_endyear + (catchment_vac_Frac_prev[year] * efficacy)
	catchmentdata_year$cases_onward_prevH_tmp1 <- catchmentdata_year$total_averted * R0 * (1 - proportion_protected)
	catchmentdata_year$cases_onward_prevH_tmp2 <- round(catchmentdata_year$cases_onward_prevH_tmp1)
	catchmentdata_year$cases_onward_prevH <- catchmentdata_year$cases_onward_prevH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_prevH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_prevH_tmp1[j] <- catchmentdata_year$cases_onward_prevH_tmp1[j] * R0 * (1 - proportion_protected)
			catchmentdata_year$cases_onward_prevH_tmp2[j] <- round(catchmentdata_year$cases_onward_prevH_tmp1[j])
			catchmentdata_year$cases_onward_prevH[j] <- catchmentdata_year$cases_onward_prevH[j] + catchmentdata_year$cases_onward_prevH_tmp1[j]
		}
	}
    
    #calculate remaining cases after immunity, preventive vaccination and onward transmissions removed
	catchmentdata_year$total_avertedH <- catchmentdata_year$cases_averted_immunityH + catchmentdata_year$cases_averted_prevH
    catchmentdata_year$remainingcasesU1 <- pmax(0, (catchmentdata_year$unpruned_nc - catchmentdata_year$total_averted - catchmentdata_year$cases_onward_prevH))
	catchmentdata_year$remainingcasesH1 <- pmax(0, (catchmentdata_year$h2h_nc - catchmentdata_year$total_avertedH - catchmentdata_year$cases_onward_prevH))
	
    #apply reactive vaccination if threshold of 50 cases over 28 day period is exceeded
	previous_values <- previous_values_endyear # previous values for rolling sum (last 27 pruned values from previous year)
	padded_data <- c(previous_values, catchmentdata_year$remainingcasesU1)
	rolling_sum <- rollsum(padded_data, 28, align = "left")
    threshold_index <- which(rolling_sum >= 50)
    if (length(threshold_index)==0) {
      reactive_start <- nrow(catchmentdata_year)
    } else {
      reactive_start <- threshold_index[1]
    }
    catchmentdata_year$vacFrac_reac[1:reactive_start] <- 0
	catchmentdata_year$cases_averted_reac <- catchmentdata_year$unpruned_nc * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_averted_reacH <- catchmentdata_year$h2h_nc * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_vacc_reac <- catchmentdata_year$unpruned_nc * catchmentdata_year$vacFrac_reac
	
	#re-calculate onward transmissions
	catchmentdata_year$averted <- catchmentdata_year$cases_averted_prev + catchmentdata_year$cases_averted_reac
	catchmentdata_year$total_averted <- catchmentdata_year$cases_averted_immunity + catchmentdata_year$cases_averted_prev + catchmentdata_year$cases_averted_reac
	proportion_protected <- Newcoverage_endyear + (catchment_vac_Frac_prev[year] * efficacy) + (catchmentdata_year$vacFrac_reac * efficacy)
	catchmentdata_year$cases_onward_total_tmp1 <- catchmentdata_year$total_averted * R0 * (1 - proportion_protected)
	catchmentdata_year$cases_onward_total_tmp2 <- round(catchmentdata_year$cases_onward_total_tmp1)
	catchmentdata_year$cases_onward_total <- catchmentdata_year$cases_onward_total_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_total_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_total_tmp1[j] <- catchmentdata_year$cases_onward_total_tmp1[j] * R0 * (1 - proportion_protected[j])
			catchmentdata_year$cases_onward_total_tmp2[j] <- round(catchmentdata_year$cases_onward_total_tmp1[j])
			catchmentdata_year$cases_onward_total[j] <- catchmentdata_year$cases_onward_total[j] + catchmentdata_year$cases_onward_total_tmp1[j]
		}
	}
	
	#re-calculate remaining cases after immunity, preventive and reactive vaccination and onward transmissions removed
	catchmentdata_year$total_avertedH <- catchmentdata_year$cases_averted_immunityH + catchmentdata_year$cases_averted_prevH + catchmentdata_year$cases_averted_reacH
    catchmentdata_year$remainingcasesU2 <- pmax(0, (catchmentdata_year$unpruned_nc - catchmentdata_year$total_averted - catchmentdata_year$cases_onward_total))
	catchmentdata_year$remainingcasesH2 <- pmax(0, (catchmentdata_year$h2h_nc - catchmentdata_year$total_avertedH - catchmentdata_year$cases_onward_total))
    
	#calculate pruned, vaccinated and total_vaccinated (N.B. averted = directly protected and hence immune, vaccinated = vaccinated but not necessarily protected)
    catchmentdata_year$Newpruned_nc <- catchmentdata_year$remainingcasesU2
	catchmentdata_year$vaccinated <- catchmentdata_year$cases_vacc_prev + catchmentdata_year$cases_vacc_reac
	catchmentdata_year$total_vaccinated <- catchmentdata_year$cases_vacc_immunity + catchmentdata_year$cases_vacc_prev + catchmentdata_year$cases_vacc_reac
	
	#save total coverage for immunity pruning next year
	if (reactive_start == nrow(catchmentdata_year)) {
		total_coverage_endyear <- total_coverage_endyear + catchment_vac_Frac_prev[year]
	} else {
		total_coverage_endyear <- total_coverage_endyear + catchment_vac_Frac_prev[year] + catchmentdata_year$vacFrac_reac[nrow(catchmentdata_year)]
	}
	
	#save last 27 pruned values for rolling sum next year
	previous_values_endyear <- catchmentdata_year$Newpruned_nc[339:365]
	
    #save and combine results with other years
    Lassa_results2 <<-  tibble(scenario = catchmentdata_year$Scenario, country = catchmentdata_year$COUNTRY, catchmentID = catchmentdata_year$identifier,
							   run = catchmentdata_year$Run, realyear = year, unpruned = catchmentdata_year$unpruned, unpruned_nc = catchmentdata_year$unpruned_nc,
							   spillover = catchmentdata_year$spillover, spillover_nc = catchmentdata_year$spillover_nc,
							   h2h_nc = catchmentdata_year$h2h_nc, pruned_nc = catchmentdata_year$Newpruned_nc,
							   averted = catchmentdata_year$averted, total_averted = catchmentdata_year$total_averted,
							   vaccinated = catchmentdata_year$vaccinated, total_vaccinated = catchmentdata_year$total_vaccinated,
							   averted_imm = catchmentdata_year$cases_averted_immunity, averted_reac = catchmentdata_year$cases_averted_reac,
							   averted_prev = catchmentdata_year$cases_averted_prev, averted_onward = catchmentdata_year$cases_onward_total)
    Lassa_results <<- bind_rows(Lassa_results, Lassa_results2)
    
  } } }
