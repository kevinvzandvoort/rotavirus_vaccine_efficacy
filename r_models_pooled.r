rcodes <- list(
  "waning_none" = function(t, mean_rr, alpha, beta){
    return(
      rep(
        exp(mean_rr),
        length(t)
      )
    )
  },
  "waning_linear" = function(t, mean_rr, alpha, beta){
    return(
      exp(mean_rr) + exp(alpha)*t
    )
  },
  "waning_power" = function(t, mean_rr, alpha, beta){
    return(
      exp(mean_rr + exp(alpha)*log(t))
    )
  },
  "waning_power_01" = function(t, mean_rr, alpha, beta){
    return(
      (
        exp(alpha)*t^exp(beta)
      )/(
        exp(mean_rr)+exp(alpha)*t^exp(beta)
      )
    )
  },
  "waning_power2" = function(t, mean_rr, alpha, beta){
    return(
      exp(mean_rr + alpha*log(t))
    )
  },
  "waning_power3" = function(t, mean_rr, alpha, beta){
    return(
      exp(mean_rr + alpha*t)
    )
  },
  "waning_sigmoid" = function(t, mean_rr, alpha, beta){
    return(
      exp( mean_rr ) * (1/( 1 + exp(alpha)*exp( -exp(beta)*t) ) )
    )
  },
  "waning_sigmoid_01" = function(t, mean_rr, alpha, beta){
    return(
      exp(mean_rr)/(
        exp(mean_rr) + exp(alpha)*exp( -exp(beta)*t )
      )
    )
  },
  "waning_gamma_01" = function(t, mean_rr, alpha, beta){
    return(
      pgamma( t, exp(alpha), exp(beta) )
    )
  }
)