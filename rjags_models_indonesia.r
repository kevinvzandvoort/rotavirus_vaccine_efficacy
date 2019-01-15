jcodes <- list(
	"waning_none"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+mean_RR
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+mean_RR
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_linear"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i]) + mean_RR + alpha_neonatal + log(neonatal_time[i])
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i]) + rho + mean_RR + beta_neonatal + log(infant_time[i])
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_power"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+mean_RR+exp(alpha_neonatal)*log( neonatal_time[i] )
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+mean_RR+exp(beta_neonatal)*log( infant_time[i] )
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_power_01"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+ log((exp(alpha_neonatal)*neonatal_time[i]^exp(alpha_infant))/(exp(mean_RR)+exp(alpha_neonatal)*neonatal_time[i]^exp(alpha_infant)))
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+ log((exp(beta_neonatal)*infant_time[i]^exp(beta_infant))/(exp(mean_RR)+exp(beta_neonatal)*infant_time[i]^exp(beta_infant)))
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		alpha_rho ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		beta_rho ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_power2"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+mean_RR+ alpha_neonatal*log(neonatal_time[i])
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+mean_RR+ beta_neonatal*log(infant_time[i])
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_power3"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+mean_RR+ alpha_neonatal*neonatal_time[i]
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+mean_RR+ beta_neonatal*infant_time[i]
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_sigmoid"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+mean_RR+ log( 1/( 1 + exp(alpha_neonatal)*exp( -exp(alpha_infant)*neonatal_time[i]) ) )
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+mean_RR+ log( 1/( 1 + exp(beta_neonatal)*exp( -exp(beta_infant)*infant_time[i]) ) )
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		alpha_rho ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		beta_rho ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_sigmoid_01"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i])+ log( exp(mean_RR)/( exp(mean_RR) + exp(alpha_neonatal)*exp( -exp(alpha_infant)*neonatal_time[i]) ) )
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+ log( exp(mean_RR)/( exp(mean_RR) + exp(beta_neonatal)*exp( -exp(beta_infant)*infant_time[i]) ) )
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		alpha_rho ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		beta_rho ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
		mean_RR ~ dnorm(0,0.01)
	}",
	"waning_gamma_01"="model{
		for (i in 1:length(neonatal_time)){
			neonatal_cases_placebo[i] ~ dpois(pnc[i]) #poisson distributed observed cases in placebo
			neonatal_cases_vaccine[i] ~ dpois(vnc[i]) #poisson distributed observed cases in vaccinated
			log(pnc[i]) = base_rate + log(neonatal_atrisk_placebo[i])
			log(vnc[i]) = base_rate + log(neonatal_atrisk_vaccine[i]) +log( ( pgamma( neonatal_time[i], exp(alpha_neonatal), exp(alpha_infant) ) ) )
		}
		for (i in 1:length(infant_time)){
			infant_cases_placebo[i] ~ dpois(pic[i])
			infant_cases_vaccine[i] ~ dpois(vic[i]) #poisson distributed observed cases in vaccinated
			log(pic[i]) = base_rate + log(infant_atrisk_placebo[i])
			log(vic[i]) = base_rate + log(infant_atrisk_vaccine[i])+rho+log( ( pgamma( infant_time[i], exp(beta_neonatal), exp(beta_infant) ) ) )
		}
		#rate in unvaccinated
		base_rate ~ dnorm(0,0.01)
		alpha_neonatal ~ dnorm(0,0.01)
		beta_neonatal ~ dnorm(0,0.01)
		alpha_rho ~ dnorm(0,0.01)
		beta_rho ~ dnorm(0,0.01)
		#relative_rate_vaccinated
		rho ~ dnorm(0,0.01)
	}"
)