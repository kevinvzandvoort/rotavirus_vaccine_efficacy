#source c++ VE to iVE conversion method
sourceCpp("./cumVEtoInsFast.cpp")

#make a simple plot
plotEstimates <- function(vaccine_efficacy_data, vaccine_efficacy_central = NULL){
  p <- ggplot(
  )+geom_errorbar(
    data=vaccine_efficacy_data,
    aes(
      x=follow_up_end,
      ymin=1-rr_high,
      ymax=1-rr_low
    )
  )+geom_point(
    data=vaccine_efficacy_data,
    aes(
      x=follow_up_end,
      y=1-rr,
      #Standard error RR dominated by number of cases
      size=(cases_vaccine + cases_placebo)
    )
  )+coord_cartesian(
    ylim=c(0, 1),
    xlim=c(0.2, 36)
  )+facet_grid(
    vaccine_efficacy~mortality_factor
  )+labs(
    x="Months since completed vaccine schedule",
    y="Vaccine efficacy (prop)"
  )+scale_size_continuous(
    range = c(0.5,2.5)
  )+guides(
    size="none"
  )+geom_hline(
    yintercept=0
  )
  if(!is.null(vaccine_efficacy_central)){
    p <- p +geom_ribbon(
      data=vaccine_efficacy_central,
      aes(
        x=time,
        ymin=low,
        ymax=high
      ),
      alpha=0.2
    )+geom_line(
      data=vaccine_efficacy_central,
      aes(
        x=time,
        y=median
      )
    )
  }
  print(p)
}

#calculate empirical p-values and credible intervals from 2 matrixes (data_a and data_b) with the posterior samples that are to be compared
#cols argument can be used to specify the index of the column(s) to be compared
empiricalDifference <- function(
  data_a,
  data_b,
  cols = NULL
){
  if(is.null(cols)){
    cols <- c(1:ncol(data_a))
  }
  data_a <- data_a[, cols]
  data_b <- data_b[, cols]
  data_difference <- matrix(
    0,
    ncol=ncol(data_a),
    nrow=nrow(data_a)*nrow(data_b)
  )
  for(r in 1:nrow(data_a)){
    data_difference[c(1:nrow(data_a))+nrow(data_a)*(r-1), ] <- - sweep(data_b, 2, data_a[r,], "-")
  }
  data_difference_low <- sapply(
    c(1:ncol(data_difference)),
    function(x){
      quantile(
        data_difference[, x],
        probs=c(0.025)
      )
    }
  )
  data_difference_median <- sapply(
    c(1:ncol(data_difference)),
    function(x){
      quantile(
        data_difference[, x],
        probs=c(0.5)
      )
    }
  )
  data_difference_high <- sapply(
    c(1:ncol(data_difference)),
    function(x){
      quantile(
        data_difference[, x],
        probs=c(0.975)
      )
    }
  )
  data_pval <- matrix(
    0,
    ncol=ncol(data_a),
    nrow=nrow(data_b)
  )
  #count number of times data_a is higher than data_b
  for(r in 1:nrow(data_a)){
    data_pval[r, ] <- colSums(
      sweep(data_b, 2, data_a[r,], "<")
    )
  }
  data_pval <- 1 - (colSums(data_pval) / (nrow(data_a)*nrow(data_b)))
  return(
    list(
      "pvalue" = data_pval,
      "diff_low" = data_difference_low,
      "diff_high" = data_difference_high,
      "diff_median" = data_difference_median
    )
  )
}