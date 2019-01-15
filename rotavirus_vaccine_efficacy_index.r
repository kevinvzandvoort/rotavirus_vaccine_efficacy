#Estimating rotavirus vaccine efficacy
#Author: London School of Hygiene & Tropical Medicine - Kevin van Zandvoort
#Date: 5/1/2019
#Description: This code accompanies the paper: Efficacy of live oral rotavirus
# vaccines by duration of follow-up: a meta-regression of 
# randomised controlled trials
# In order to accurately capture the effect of waning VE, we fit out models
# to cumulative incidence values extracted from multiple RCTs.
# Extracted data is provided in the csv file (see prepare ).
# We fit models with different assumptions about waning vaccine efficacy using
# Rjags. Models are compared visually, and by the DIC value (see)

library(data.table)
library(ggplot2)
library(rjags)
library(Rcpp)

#additional functions
source("functions.r")

#prepare extracted data pooled analysis
#We re-calculate the RR in exactly the same way in each study period.
#We need to calculate the relative risk, due to the sparse follow-up
# data, and because censored number of people is not consistently 
# available in each study.
#Will create a data-table with the cleaned and processed estimates: vaccine_efficacy_data
source("prepare_extracted_data.r")

#show extracted data by mortality stratum
plotEstimates(vaccine_efficacy_data)

#load models for pooled analysis in jcodes object (in jags language)
source("rjags_models_pooled.r")

#Nb. we could not provide data the data for the RV3-BB Indonesia trial
#However, the models (rjags code) used in this study are provided in rjags_models_indonesia.r
#source("rjags_models_indonesia.r")

#load models for pooled analaysis in rcodes object (in R language)
source("r_models_pooled.r")

#select model (waning_power)
model <- c(
  "waning_none", "waning_linear", "waning_power",
  "waning_power_01", "waning_power2", "waning_power3",
  "waning_sigmoid", "waning_sigmoid_01", "waning_gamma_01"
)[3]

#set mcmc options
mcmc.chains <- 2
mcmc.length <- 100000
thin <- 4

#run the models with rjags. This will create 3 data-tables: posterior_low,
# posterior_medium, and posterior_high (one for each mortality stratum)
source("run_rjags_model.r")
#trace warnings can be ignored

#check mcmc chains for each mortality stratum
plot(posterior_low)

#further diagnostics
#acfplot(posterior_low)
#gelmanplot(posterior_low)

#months in which VE and iVE should be estimated
times <- seq(0.5,36,0.5)

#process modelled results
# generate VE and iVE for each saved draw from the joint posterior distribution)
# see Appendix A in the main paper for more information about the conversion process
# this will create 7 data-tables:
# 3 tables with cumulative VE for each stratum, i.e.: ve_cumulative_low
# 3 tables with instantaneous VE for each stratum, i.e.: ve_instantaneous_low
# 1 table with all VE (iVE and VE) combined: vaccine_efficacy_central
source("process_modelled_results.r")

#show fit to observed data in each dataset
plotEstimates(vaccine_efficacy_data, vaccine_efficacy_central)

#calculate empirical p-values and credible intervals for set timepoints
# compares columns 'cols' in data_a with those in data_b
#default: timepoint 1 and 24 (2 weeks and 12 months follow-up)
#change data_a, data_b, and cols if needed
empiricalDifference(
  data_a = ve_instantaneous_low,
  data_b = ve_instantaneous_medium,
  cols = c(1,24)
)