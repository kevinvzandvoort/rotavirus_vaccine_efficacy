vaccine_efficacy_data <- fread("rotavirus_vaccine_efficacy_extracted.csv")
vaccine_efficacy_data[,"study_id"] <- 0
vaccine_efficacy_data[,"follow_up_p"] <- 0
vaccine_efficacy_data[,"cum_cases_placebo"] <- 0
vaccine_efficacy_data[,"cum_cases_vaccine"] <- 0
vaccine_efficacy_data[,"persontime_placebo"] <- 0
vaccine_efficacy_data[,"persontime_vaccine"] <- 0
vaccine_efficacy_data[,"N_placebo_start"] <- 0
vaccine_efficacy_data[,"N_vaccine_start"] <- 0

#calculate follow-up time per-period in each unique study
#loop through studies within each stratum
for(m in unique(vaccine_efficacy_data[, mortality])){
  vaccine_efficacy_stratum <- vaccine_efficacy_data[mortality == m]
  vaccine_efficacy_stratum[,"study_id"] <- cumsum(
    vaccine_efficacy_stratum[,period] == "Period 1"
  )
  for(s in unique(vaccine_efficacy_stratum[, study_id])){
    vaccine_efficacy_study <- vaccine_efficacy_stratum[study_id == s]
    #loop through study-periods
    for(p in 1:nrow(vaccine_efficacy_study)){
      if(p == 1){
        vaccine_efficacy_study[p,"follow_up_p"] <- vaccine_efficacy_study[p,follow_up_end]
        vaccine_efficacy_study[p,"cum_cases_placebo"] <- vaccine_efficacy_study[p,cases_placebo]
        vaccine_efficacy_study[p,"cum_cases_vaccine"] <- vaccine_efficacy_study[p,cases_vaccine]
      } else {
        vaccine_efficacy_study[p,"follow_up_p"] <- vaccine_efficacy_study[p,follow_up_end]
        vaccine_efficacy_study[p,"cum_cases_placebo"] <- vaccine_efficacy_study[p-1,cum_cases_placebo] + vaccine_efficacy_study[p,cases_placebo]
        vaccine_efficacy_study[p,"cum_cases_vaccine"] <- vaccine_efficacy_study[p-1,cum_cases_vaccine] + vaccine_efficacy_study[p,cases_vaccine]
      }
    }
    vaccine_efficacy_study[,"N_placebo_start"] <- vaccine_efficacy_study[1,N_placebo]
    vaccine_efficacy_study[,"N_vaccine_start"] <- vaccine_efficacy_study[1,N_vaccine]
    
    #calculate follow-up time, use N at start in denominator, as studies do not consistently censor at follow-up
    vaccine_efficacy_study[,"persontime_placebo"] <- vaccine_efficacy_study[,N_placebo_start] * vaccine_efficacy_study[,follow_up_p]
    vaccine_efficacy_study[,"persontime_vaccine"] <- vaccine_efficacy_study[,N_vaccine_start] * vaccine_efficacy_study[,follow_up_p]
    
    vaccine_efficacy_stratum <- rbindlist(
      list(
        vaccine_efficacy_stratum[study_id != s],
        vaccine_efficacy_study
      )
    )
  }
  
  vaccine_efficacy_data <- rbindlist(
    list(
      vaccine_efficacy_data[mortality != m],
      vaccine_efficacy_stratum
    )
  )
}

#add 0.5 if no cases have been observed in vaccinated arm (cannot divide by 0), validated method
vaccine_efficacy_data[cum_cases_vaccine == 0, "N_placebo_start"] <- vaccine_efficacy_data[cum_cases_vaccine == 0, N_placebo_start] + 0.5
vaccine_efficacy_data[cum_cases_vaccine == 0, "N_vaccine_start"] <- vaccine_efficacy_data[cum_cases_vaccine == 0, N_vaccine_start] + 0.5
vaccine_efficacy_data[cum_cases_vaccine == 0, "cases_placebo"] <- vaccine_efficacy_data[cum_cases_vaccine == 0, cases_placebo] + 0.5
vaccine_efficacy_data[cum_cases_vaccine == 0, "cases_vaccine"] <- vaccine_efficacy_data[cum_cases_vaccine == 0, cases_vaccine] + 0.5
vaccine_efficacy_data[cum_cases_vaccine == 0, "cum_cases_placebo"] <- vaccine_efficacy_data[cum_cases_vaccine == 0, cum_cases_placebo] + 0.5
vaccine_efficacy_data[cum_cases_vaccine == 0, "cum_cases_vaccine"] <- vaccine_efficacy_data[cum_cases_vaccine == 0, cum_cases_vaccine] + 0.5

#calculate the relative risk (relative rate not possible with this data)
vaccine_efficacy_data[,"rr"] <- (
  vaccine_efficacy_data[,cum_cases_vaccine]/vaccine_efficacy_data[,N_vaccine_start]
)/(
  vaccine_efficacy_data[,cum_cases_placebo]/vaccine_efficacy_data[,N_placebo_start]
)
vaccine_efficacy_data[,"rr_high"] <- exp(
  log(vaccine_efficacy_data[,rr])
  +1.96*sqrt(
    (
      (vaccine_efficacy_data[,N_vaccine_start] - vaccine_efficacy_data[,cum_cases_vaccine])/vaccine_efficacy_data[,cum_cases_vaccine]
    )/vaccine_efficacy_data[,N_vaccine_start] +(
      (vaccine_efficacy_data[,N_placebo_start] - vaccine_efficacy_data[,cum_cases_placebo])/vaccine_efficacy_data[,cum_cases_placebo]
    )/vaccine_efficacy_data[,N_placebo_start]
  )
)

vaccine_efficacy_data[,"rr_low"] <- exp(
  log(vaccine_efficacy_data[,rr])
  -1.96*sqrt(
    (
      (vaccine_efficacy_data[,N_vaccine_start] - vaccine_efficacy_data[,cum_cases_vaccine])/vaccine_efficacy_data[,cum_cases_vaccine]
    )/vaccine_efficacy_data[,N_vaccine_start] +(
      (vaccine_efficacy_data[,N_placebo_start] - vaccine_efficacy_data[,cum_cases_placebo])/vaccine_efficacy_data[,cum_cases_placebo]
    )/vaccine_efficacy_data[,N_placebo_start]
  )
)

vaccine_efficacy_data[, "vaccine_efficacy"] <- "cumulative"
vaccine_efficacy_data[, "mortality_factor"] <- factor(
  vaccine_efficacy_data[, mortality],
  levels=c("Low", "Medium", "High")
)