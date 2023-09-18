
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
	catchmentdata_year$cases_onward_prevH_tmp1 <- catchmentdata_year$cases_averted_prev * R0
	catchmentdata_year$cases_onward_prevH_tmp2 <- round(catchmentdata_year$cases_onward_prevH_tmp1)
	catchmentdata_year$cases_onward_prevH <- catchmentdata_year$cases_onward_prevH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_prevH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_prevH_tmp1[j] <- catchmentdata_year$cases_onward_prevH_tmp1[j] * R0
			catchmentdata_year$cases_onward_prevH_tmp2[j] <- round(catchmentdata_year$cases_onward_prevH_tmp1[j])
			catchmentdata_year$cases_onward_prevH[j] <- catchmentdata_year$cases_onward_prevH[j] + catchmentdata_year$cases_onward_prevH_tmp1[j]
		}
	}
    
    #calculate remaining cases after preventive vaccination and onward transmissions removed
    catchmentdata_year$remainingcasesU1 <- pmax(0, (catchmentdata_year$unpruned_nc - catchmentdata_year$cases_averted_prev - catchmentdata_year$cases_onward_prevH))
	catchmentdata_year$remainingcasesH1 <- pmax(0, (catchmentdata_year$h2h_nc - catchmentdata_year$cases_averted_prevH - catchmentdata_year$cases_onward_prevH))
	
    #apply reactive vaccination on remaining cases based on threshold of 50 cases over 28 day period
    daily_threshold_rate <- 50/28
    threshold_index <- which(catchmentdata_year$remainingcasesU1 >= daily_threshold_rate)
    if (length(threshold_index)==0) {
      reactive_start <- nrow(catchmentdata_year)
    } else {
      reactive_start <- threshold_index[1] + 27
    }
    catchmentdata_year$vacFrac_reac[1:reactive_start] <- 0
    catchmentdata_year$cases_averted_reac <- catchmentdata_year$remainingcasesU1 * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_averted_reacH <- catchmentdata_year$remainingcasesH1 * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_vacc_reac <- catchmentdata_year$remainingcasesU1 * catchmentdata_year$vacFrac_reac
	
	#onward transmissions
	catchmentdata_year$cases_onward_reacH_tmp1 <- catchmentdata_year$cases_averted_reac * R0
	catchmentdata_year$cases_onward_reacH_tmp2 <- round(catchmentdata_year$cases_onward_reacH_tmp1)
	catchmentdata_year$cases_onward_reacH <- catchmentdata_year$cases_onward_reacH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_reacH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_reacH_tmp1[j] <- catchmentdata_year$cases_onward_reacH_tmp1[j] * R0
			catchmentdata_year$cases_onward_reacH_tmp2[j] <- round(catchmentdata_year$cases_onward_reacH_tmp1[j])
			catchmentdata_year$cases_onward_reacH[j] <- catchmentdata_year$cases_onward_reacH[j] + catchmentdata_year$cases_onward_reacH_tmp1[j]
		}
	}
	
	#calculate remaining cases after reactive vaccination and onward transmissions removed
    catchmentdata_year$remainingcasesU2 <- pmax(0, (catchmentdata_year$remainingcasesU1 - catchmentdata_year$cases_averted_reac - catchmentdata_year$cases_onward_reacH))
	catchmentdata_year$remainingcasesH2 <- pmax(0, (catchmentdata_year$remainingcasesH1 - catchmentdata_year$cases_averted_reacH - catchmentdata_year$cases_onward_reacH))
    
	#calculate pruned, averted, total_averted, vaccinated and total_vaccinated (N.B. averted = directly protected and hence immune, vaccinated = vaccinated but not necessarily protected)
    catchmentdata_year$Newpruned_nc <- catchmentdata_year$remainingcasesU2
	catchmentdata_year$averted <- catchmentdata_year$cases_averted_reac + catchmentdata_year$cases_averted_prev
	catchmentdata_year$total_averted <- catchmentdata_year$cases_averted_reac + catchmentdata_year$cases_averted_prev
	catchmentdata_year$vaccinated <- catchmentdata_year$cases_vacc_reac + catchmentdata_year$cases_vacc_prev
	catchmentdata_year$total_vaccinated <- catchmentdata_year$cases_vacc_reac + catchmentdata_year$cases_vacc_prev
	
    #save total averted for immunity pruning next year
	total_averted_endyear <- cumsum(catchmentdata_year$total_averted)[nrow(catchmentdata_year)]
	total_vacc_endyear <- cumsum(catchmentdata_year$total_vaccinated)[nrow(catchmentdata_year)]
	
    #save results
    Lassa_results <<- tibble(scenario = catchmentdata_year$Scenario, country = catchmentdata_year$COUNTRY, catchmentID = catchmentdata_year$identifier,
							 run = catchmentdata_year$Run, realyear = year, unpruned = catchmentdata_year$unpruned, unpruned_nc = catchmentdata_year$unpruned_nc,
							 spillover = catchmentdata_year$spillover, spillover_nc = catchmentdata_year$spillover_nc,
							 h2h_nc = catchmentdata_year$h2h_nc, pruned_nc = catchmentdata_year$Newpruned_nc,
							 averted = catchmentdata_year$averted, total_averted = catchmentdata_year$total_averted,
							 vaccinated = catchmentdata_year$vaccinated, total_vaccinated = catchmentdata_year$total_vaccinated)
  
  } else {
        
    #All years following Year 1
	
	#calculate combined coverage for immunity pruning
	total_unpruned_thisyear <- cumsum(catchmentdata_year$unpruned_nc)[nrow(catchmentdata_year)]
    Newcoverage_endyear <- total_averted_endyear/total_unpruned_thisyear
	Newcoverage_endyear_vacc <- total_vacc_endyear/total_unpruned_thisyear
	
    #apply immunity 
    catchmentdata_year$cases_averted_immunity <-  catchmentdata_year$unpruned_nc * Newcoverage_endyear
	catchmentdata_year$cases_averted_immunityH <-  catchmentdata_year$h2h_nc * Newcoverage_endyear
	catchmentdata_year$cases_vacc_immunity <-  catchmentdata_year$unpruned_nc * Newcoverage_endyear_vacc
	
	#onward transmissions
	catchmentdata_year$cases_onward_immunityH_tmp1 <- catchmentdata_year$cases_averted_immunity * R0
	catchmentdata_year$cases_onward_immunityH_tmp2 <- round(catchmentdata_year$cases_onward_immunityH_tmp1)
	catchmentdata_year$cases_onward_immunityH <- catchmentdata_year$cases_onward_immunityH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_immunityH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_immunityH_tmp1[j] <- catchmentdata_year$cases_onward_immunityH_tmp1[j] * R0
			catchmentdata_year$cases_onward_immunityH_tmp2[j] <- round(catchmentdata_year$cases_onward_immunityH_tmp1[j])
			catchmentdata_year$cases_onward_immunityH[j] <- catchmentdata_year$cases_onward_immunityH[j] + catchmentdata_year$cases_onward_immunityH_tmp1[j]
		}
	}
	
	#calculate remaining cases after immunity pruning and onward transmissions removed
    catchmentdata_year$cases_after_immunityU0 <- pmax(0, (catchmentdata_year$unpruned_nc - catchmentdata_year$cases_averted_immunity - catchmentdata_year$cases_onward_immunityH))
	catchmentdata_year$cases_after_immunityH0 <- pmax(0, (catchmentdata_year$h2h_nc - catchmentdata_year$cases_averted_immunityH - catchmentdata_year$cases_onward_immunityH))
	
    #apply preventive vaccination
    catchmentdata_year$cases_averted_prev <- catchmentdata_year$cases_after_immunityU0 * catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_averted_prevH <- catchmentdata_year$cases_after_immunityH0 * catchment_vac_Frac_prev[year] * efficacy
	catchmentdata_year$cases_vacc_prev <- catchmentdata_year$cases_after_immunityU0 * catchment_vac_Frac_prev[year]
	
	#onward transmissions
	catchmentdata_year$cases_onward_prevH_tmp1 <- catchmentdata_year$cases_averted_prev * R0
	catchmentdata_year$cases_onward_prevH_tmp2 <- round(catchmentdata_year$cases_onward_prevH_tmp1)
	catchmentdata_year$cases_onward_prevH <- catchmentdata_year$cases_onward_prevH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_prevH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_prevH_tmp1[j] <- catchmentdata_year$cases_onward_prevH_tmp1[j] * R0
			catchmentdata_year$cases_onward_prevH_tmp2[j] <- round(catchmentdata_year$cases_onward_prevH_tmp1[j])
			catchmentdata_year$cases_onward_prevH[j] <- catchmentdata_year$cases_onward_prevH[j] + catchmentdata_year$cases_onward_prevH_tmp1[j]
		}
	}
    
    #calculate remaining cases after prev vaccination and onward transmissions removed
	catchmentdata_year$remainingcasesU1 <- pmax(0, (catchmentdata_year$cases_after_immunityU0 - catchmentdata_year$cases_averted_prev - catchmentdata_year$cases_onward_prevH))
	catchmentdata_year$remainingcasesH1 <- pmax(0, (catchmentdata_year$cases_after_immunityH0 - catchmentdata_year$cases_averted_prevH - catchmentdata_year$cases_onward_prevH))
	
    #apply reactive vaccination on remaining cases based on threshold of 50 cases over 28 day period
    daily_threshold_rate <- 50/28
    threshold_index <- which(catchmentdata_year$remainingcasesU1 >= daily_threshold_rate)
    if (length(threshold_index)==0) {
      reactive_start <- nrow(catchmentdata_year)
    } else {
      reactive_start <- threshold_index[1] + 27
    }
    catchmentdata_year$vacFrac_reac[1:reactive_start] <- 0
	catchmentdata_year$cases_averted_reac <- catchmentdata_year$remainingcasesU1 * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_averted_reacH <- catchmentdata_year$remainingcasesH1 * catchmentdata_year$vacFrac_reac * efficacy
	catchmentdata_year$cases_vacc_reac <- catchmentdata_year$remainingcasesU1 * catchmentdata_year$vacFrac_reac
	
	#onward transmissions
	catchmentdata_year$cases_onward_reacH_tmp1 <- catchmentdata_year$cases_averted_reac * R0
	catchmentdata_year$cases_onward_reacH_tmp2 <- round(catchmentdata_year$cases_onward_reacH_tmp1)
	catchmentdata_year$cases_onward_reacH <- catchmentdata_year$cases_onward_reacH_tmp1
	for (j in (1:nrow(catchmentdata_year))) {
		while (catchmentdata_year$cases_onward_reacH_tmp2[j] >= 1) {
			catchmentdata_year$cases_onward_reacH_tmp1[j] <- catchmentdata_year$cases_onward_reacH_tmp1[j] * R0
			catchmentdata_year$cases_onward_reacH_tmp2[j] <- round(catchmentdata_year$cases_onward_reacH_tmp1[j])
			catchmentdata_year$cases_onward_reacH[j] <- catchmentdata_year$cases_onward_reacH[j] + catchmentdata_year$cases_onward_reacH_tmp1[j]
		}
	}
	
	#calculate remaining cases after reactive vaccination and onward transmissions removed
    catchmentdata_year$remainingcasesU2 <- pmax(0, (catchmentdata_year$remainingcasesU1 - catchmentdata_year$cases_averted_reac - catchmentdata_year$cases_onward_reacH))
	catchmentdata_year$remainingcasesH2 <- pmax(0, (catchmentdata_year$remainingcasesH1 - catchmentdata_year$cases_averted_reacH - catchmentdata_year$cases_onward_reacH))
    
	#calculate pruned, averted, total_averted, vaccinated and total_vaccinated (N.B. averted = directly protected and hence immune, vaccinated = vaccinated but not necessarily protected)
    catchmentdata_year$Newpruned_nc <- catchmentdata_year$remainingcasesU2
	catchmentdata_year$averted <- catchmentdata_year$cases_averted_reac + catchmentdata_year$cases_averted_prev
	catchmentdata_year$total_averted <- catchmentdata_year$cases_averted_immunity + catchmentdata_year$cases_averted_reac + catchmentdata_year$cases_averted_prev
	catchmentdata_year$vaccinated <- catchmentdata_year$cases_vacc_reac + catchmentdata_year$cases_vacc_prev
	catchmentdata_year$total_vaccinated <- catchmentdata_year$cases_vacc_immunity + catchmentdata_year$cases_vacc_reac + catchmentdata_year$cases_vacc_prev
    
    #save total averted for immunity pruning next year
	total_averted_endyear <- cumsum(catchmentdata_year$total_averted)[nrow(catchmentdata_year)]
	total_vacc_endyear <- cumsum(catchmentdata_year$total_vaccinated)[nrow(catchmentdata_year)]
	
    #save and combine results with other years
    Lassa_results2 <<-  tibble(scenario = catchmentdata_year$Scenario, country = catchmentdata_year$COUNTRY, catchmentID = catchmentdata_year$identifier,
							   run = catchmentdata_year$Run, realyear = year, unpruned = catchmentdata_year$unpruned, unpruned_nc = catchmentdata_year$unpruned_nc,
							   spillover = catchmentdata_year$spillover, spillover_nc = catchmentdata_year$spillover_nc,
							   h2h_nc = catchmentdata_year$h2h_nc, pruned_nc = catchmentdata_year$Newpruned_nc,
							   averted = catchmentdata_year$averted, total_averted = catchmentdata_year$total_averted,
							   vaccinated = catchmentdata_year$vaccinated, total_vaccinated = catchmentdata_year$total_vaccinated)
    Lassa_results <<- bind_rows(Lassa_results, Lassa_results2)
    
  } } }
