#' @export

pite.ind <- function(dfc,dft){
  f_t <-  zoib.fit(dft,2)
  f_c <-  zoib.fit(dfc,2)

  data <- rbind(dft,dfc)

  zmu_t <- zero_mu_pred(f_t,data)
  zmu_c <-zero_mu_pred(f_c,data)

  zmu_t - zmu_c

}
