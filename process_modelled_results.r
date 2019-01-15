
for(m in c("Low", "Medium", "High")){
  #combine chains
  jposterior <- do.call(
    rbind,
    get(
      paste0("posterior_", tolower(m))
    )
  )
  veCumulative <- rcodes[[model]]
  
  ve_cumulative <- matrix(
    0,
    ncol = length(times),
    nrow = nrow(jposterior)
  )
  ve_instantaneous <- ve_cumulative
  for(r in 1:nrow(jposterior)){
    ve_cumulative[r, ] <- 1 - veCumulative(
      t = times,
      mean_rr = jposterior[r, "mean_rr"],
      alpha = if("alpha" %in% colnames(jposterior)){
        jposterior[r, "alpha"]
      } else {
        0
      },
      beta = if("beta" %in% colnames(jposterior)){
        jposterior[r, "beta"]
      } else {
        0
      }
    )
    #convert to instantaneous measure, assume no seasonality in average baseline rate
    ve_instantaneous[r, ] <- cumVEtoInsFast(
      ve_cumulative[r, ],
      0
    )
  }
  
  vaccine_efficacy_central <- rbindlist(
    list(
      data.table(
        time = times,
        vaccine_efficacy = rep(
          "cumulative",
          length(times)
        ),
        median = sapply(
          times+1,
          function(x){
            median(
              ve_cumulative[, x]
            ) 
          }
        ),
        low = sapply(
          times+1,
          function(x){
            quantile(
              ve_cumulative[, x],
              0.025
            ) 
          }
        ),
        high = sapply(
          times+1,
          function(x){
            quantile(
              ve_cumulative[, x],
              0.975
            ) 
          }
        )
      ),
      data.table(
        time = times,
        vaccine_efficacy = rep(
          "instantaneous",
          length(times)
        ),
        median = sapply(
          times+1,
          function(x){
            median(
              ve_instantaneous[, x]
            ) 
          }
        ),
        low = sapply(
          times+1,
          function(x){
            quantile(
              ve_instantaneous[, x],
              0.025
            ) 
          }
        ),
        high = sapply(
          times+1,
          function(x){
            quantile(
              ve_instantaneous[, x],
              0.975
            ) 
          }
        )
      )
    )
  )
  vaccine_efficacy_central[, "mortality"] <- m
  assign(
    paste0(
      "vaccine_efficacy_central_",
      tolower(m)
    ),
    vaccine_efficacy_central
  )
  assign(
    paste0(
      "ve_cumulative_",
      tolower(m)
    ),
    ve_cumulative
  )
  assign(
    paste0(
      "ve_instantaneous_",
      tolower(m)
    ),
    ve_instantaneous
  )
}
vaccine_efficacy_central <- rbindlist(
  list(
    vaccine_efficacy_central_high,
    vaccine_efficacy_central_medium,
    vaccine_efficacy_central_low
  )
)

vaccine_efficacy_central[, "mortality_factor"] <- factor(
  vaccine_efficacy_central[, mortality],
  levels=c("Low", "Medium", "High")
)