model_code <- jcodes[[model]]
for(m in c("Low", "Medium", "High")){
  jdat <- list(
    cases_placebo=round(vaccine_efficacy_data[mortality == m, cum_cases_placebo]),
    cases_vaccine=round(vaccine_efficacy_data[mortality == m, cum_cases_vaccine]),
    person_weeks_placebo=vaccine_efficacy_data[mortality == m, persontime_placebo],
    person_weeks_vaccine=vaccine_efficacy_data[mortality == m, persontime_vaccine],
    timept=vaccine_efficacy_data[mortality == m, follow_up_end],
    ID=vaccine_efficacy_data[mortality == m, study_id],
    uID=unique(vaccine_efficacy_data[mortality == m, study_id])
  )
  
  jmodel <- jags.model(
    textConnection(model_code),
    data=jdat,
    n.chains=mcmc.chains,
    n.adapt=1000
  )
  jposterior <- coda.samples(
    jmodel,
    c("mean_rr", "alpha", "beta"),
    n.iter=mcmc.length,
    thin=thin
  )
  #get DIC value
  jdic <- dic.samples(
    jmodel,
    n.iter=mcmc.length,
    thin=thin,
    type="pD"
  )
  
  assign(
    paste0("posterior_", tolower(m)),
    jposterior
  )
  assign(
    paste0("dic_", tolower(m)),
    jposterior
  )
}