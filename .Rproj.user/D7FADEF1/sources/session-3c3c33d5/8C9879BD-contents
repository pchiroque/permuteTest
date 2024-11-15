#' @export

perm.test <- function(dfc,dft, treat, trtperms){
  #
  data <- rbind(dft,dfc)

  data$trt <- trtperms

  ate_model <- zoib.fit(data,1)

  estimates <- as.data.frame(fixef(ate_model))

  z_b_beta <- estimates[5,1]
  z_b_zoi <- estimates[6,1]
  z_b_coi <- estimates[7,1]

  data$z_beta = trtperms*estimates[5,1] # offset.beta
  data$z_zoi = trtperms*estimates[6,1] # offset.zoi
  data$z_coi = trtperms*estimates[7,1] # offset.coi

  dft <- (data[treat==1,]) %>% dplyr::select(-trt)
  dfc <- (data[treat==0,]) %>% dplyr::select(-trt)

  f_t_w_ate <-  zoib.fit(dft,3)
  f_c_w_ate <-  zoib.fit(dfc,3)

  data <- rbind(dft,dfc)
  zmu_t <- zero_mu_pred(f_t_w_ate,data)
  zmu_c <-zero_mu_pred(f_c_w_ate,data)

  zmu_t - zmu_c

}
