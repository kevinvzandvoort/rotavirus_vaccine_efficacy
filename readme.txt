This repository is supplemental to the paper: Efficacy of live oral rotavirus vaccines by duration of follow-up: a meta-regression of randomised controlled trials

DATA
The data for the pooled analysis is provided in rotavirus_vaccine_efficacy_extracted.csv

CODE
The main script is provided in rotavirus_vaccine_efficacy_index.r
This script sources multiple other scripts:
- functions.r
  * loads a number of helper functions used to show the results of the analysis
- process_modelled_results.r
  * takes the extracted data-points and re-calculates the relative risk and VE in each study
- rjags_models_pooled.r
  * the models that were fitted to the data in the pooled analysis (in Rjags language)
- r_models_pooled.r
  * the (same) models that were fitted to the data in the pooled analysis (in R language)
- rjags_models_indonesia.r
  * the (same) models that were fitted to the data in the re-analysis of the RV3-BB Indonesia trial
- run_rjags_model.r
  * runs the model in Rjags
- process_modelled_results.r
  * processes the posterior estimated with Rjags
  * calculates VE and iVE for each posterior sample
    ** provides median and 95% credible intervals
  

