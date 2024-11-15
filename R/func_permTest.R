zoib.fit <- function(df,type){
  if(type==1){
    model <-  brm(
      formula_ate,
      data = df,
      family = zero_one_inflated_beta(),
      init = 0,
      chains = 4, iter = 2000, warmup = 1000,
      cores = 1, seed = 1234
    )
  }else{
    if(type==2){
      model <- brm(
        formula,
        data = df,
        family = zero_one_inflated_beta(),
        init = 0,
        chains = 4, iter = 2000, warmup = 1000,
        cores = 1, seed = 1234
      )
    }else{
      model <-  brm(
        formula_offset,
        data = df,
        family = zero_one_inflated_beta(),
        init = 0,
        chains = 4, iter = 2000, warmup = 1000,
        cores = 1, seed = 1234
      )
    }
  }
  model
}

zero_mu_pred <- function(fit,data){
  # Predictions for COI (predicted prob)
  predcoi <- posterior_linpred(fit,newdata=data, dpar="coi", transform=TRUE)   #predcoi <- apply(predcoic,2,mean)

  # Predictions for mu (predicted prob)
  predmu<- posterior_linpred(fit,newdata=data, dpar="mu", transform=TRUE)

  # predictions for zoi (zero or one) probability scale
  predzoi<- posterior_linpred(fit,newdata=data, dpar="zoi", transform=TRUE)

  #predictions for just 0: zoi*(1-coi)
  pred0<- predzoi*(1-predcoi)

  # predictions for 1s: zoi(coi)
  pred1<-predzoi*predcoi

  ##predictions for joint 0 and mu: mu*(1-(zeroes/1-ones)) # E[Y | Y < 1]
  pred0mu <- predmu*(1-(pred0/(1-pred1)))

}
