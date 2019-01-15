#Note Rjags models not R code!

jcodes <- list(
"waning_none"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
		# cases poisson distributed
		cases_placebo[i] ~ dpois(cases_placebo_dist[i])
		cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
		
		log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
		log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + study_specific_rr[ID[i]]
		
		#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(u in 1:length(uID)){
		study_specific_rr[u] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
}",
 "waning_linear"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
		# cases poisson distributed
		cases_placebo[i] ~ dpois(cases_placebo_dist[i])
		cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
		
		log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
		log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + log( exp(study_specific_rr[ID[i]]) + exp(alpha)*timept[i] )
		
		#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01) 
}",
"waning_power"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
	# cases poisson distributed
	cases_placebo[i] ~ dpois(cases_placebo_dist[i])
	cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])

	log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
	log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + study_specific_rr[ID[i]] + exp(alpha)*log( timept[i] )
	
	#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01)
}",
"waning_power_01"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
		# cases poisson distributed
		cases_placebo[i] ~ dpois(cases_placebo_dist[i])
		cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
		
		log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
		log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + log((exp(alpha)*timept[i]^exp(beta))/(exp(study_specific_rr[ID[i]])+exp(alpha)*timept[i]^exp(beta)))
		
		#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01)
	beta ~ dnorm(0,0.01)
}",
"waning_power2"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
		# cases poisson distributed
		cases_placebo[i] ~ dpois(cases_placebo_dist[i])
		cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
		
		log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
		log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + study_specific_rr[ID[i]] + alpha*log(timept[i])
		
		#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01) 
}",
"waning_power3"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
		# cases poisson distributed
		cases_placebo[i] ~ dpois(cases_placebo_dist[i])
		cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
		
		log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
		log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + study_specific_rr[ID[i]] + alpha*timept[i]
		
		#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01) 
}",
"waning_sigmoid"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
	# cases poisson distributed
	cases_placebo[i] ~ dpois(cases_placebo_dist[i])
	cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
	
	log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
	log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + study_specific_rr[ID[i]] + log( 1/( 1 + exp(alpha)*exp( -exp(beta)*timept[i]) ) )
	
	#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01)
	beta ~ dnorm(0,0.01)
}",
"waning_sigmoid_01"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
	# cases poisson distributed
	cases_placebo[i] ~ dpois(cases_placebo_dist[i])
	cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
	
	log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
	log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + log( exp(study_specific_rr[ID[i]])/( exp(study_specific_rr[ID[i]]) + exp(alpha)*exp( -exp(beta)*timept[i]) ) )
	
	#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific RR, centered around mean RR
	for(ID in 1:length(uID)){
		study_specific_rr[ID] ~ dnorm(mean_rr,between_study_var)
	}
	
	#prior for mean RR and between study var
	mean_rr ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	alpha ~ dnorm(0,0.01)
	beta ~ dnorm(0,0.01)
}",
"waning_gamma_01"="model{
	#loop through studyIDs
	for (i in 1:length(ID)){
		# cases poisson distributed
		cases_placebo[i] ~ dpois(cases_placebo_dist[i])
		cases_vaccine[i] ~ dpois(cases_vaccine_dist[i])
		
		log(cases_placebo_dist[i]) = base_rate[i] + log(person_weeks_placebo[i])
		log(cases_vaccine_dist[i]) = base_rate[i] + log(person_weeks_vaccine[i]) + log( pgamma( timept[i], exp(study_specific_alpha[ID[i]]), exp(beta) ) )
		
		#estimate rate in each study-period
		base_rate[i] ~ dnorm(0,0.01) #prior for risk in unvaccinated
	}
	
	#prior study specific mean, centered around mean
	for(ID in 1:length(uID)){
		study_specific_alpha[ID] ~ dnorm(alpha,between_study_var)
	}
	
	#prior for mean RR and between study var
	alpha ~ dnorm(0,0.01)
	between_study_var <- 1/sd^2
	sd ~ dunif(0,2)
	
	#priors for reduction in RR by time since vaccination
	beta ~ dnorm(0,0.01)
}"
)
