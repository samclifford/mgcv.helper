#' Variance Inflation Factor
#'
#' This function takes a fitted mgcv model object and returns a data frame of
#' variance inflation factors
#'
#' @param mod An object of class mgcv
#'
#' @return VIF.df A data frame consisting of the VIF values for each parametric
#' term in a fitted Generalised Additive Model
#'
#' @export
#'
#' @examples
vif.mgcv <- function(mod){

  mod.sum <- summary(mod)
  s2 <- mod$sig2 # estimate of standard deviation of residuals
  X <- mod$model # data used to fit the model
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- mod.sum$p.table[v,2]^2 # variance in estimates
  varXj <- apply(X=X[,row.names(mod.sum$p.table)[v]],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j

  VIF.df <- data.frame(variable=names(VIF),
                       vif=VIF,
                       row.names=NULL)

  return(VIF.df)
}
