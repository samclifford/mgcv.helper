#' Variance Inflation Factor
#'
#' This function takes a fitted mgcv model object and returns a data frame of
#' variance inflation factors
#'
#' @param object An object of class gam
#'
#' @return VIF.df A data frame consisting of the VIF values for each parametric
#' term in a fitted Generalised Additive Model
#'
#' @export
#'
#' @examples
#'
#' library(mgcv)
#' library(dplyr)
#'
#' set.seed(101)

#' N <- 100
#' x1 <- runif(n=N)
#' x2 <- runif(n=N)
#' x3 <- runif(n=N) + 0.9*x1 - 1.75*x2
#'
#' df <- data.frame(x1 = x1,
#'                  x2 = x2,
#'                  x3 = x3) %>%
#'   mutate(y = rnorm(n=N,
#'                    mean = 1 - 2*x1 + 3*x2 - 0.5*x3,
#'                    sd = 0.5))
#'
#' fit1 <- gam(data=df, y ~ x1 + x2 + x3)
#'
#' summary(fit1)
#'
#' vif.gam(fit1)
#'
vif.gam <- function(object){

  obj.sum <- mgcv::summary.gam(object)

  s2 <- obj$sig2 # estimate of standard deviation of residuals
  X <- obj$model # data used to fit the model
  n <- nrow(X) # how many observations were used in fitting?
  v <- -1 # omit the intercept term, it can't inflate variance
  varbeta <- obj.sum$p.table[v,2]^2 # variance in estimates
  varXj <- apply(X=X[,row.names(obj.sum$p.table)[v]],MARGIN=2, var) # variance of all the explanatory variables
  VIF <- varbeta/(s2/(n-1)*1/varXj) # the variance inflation factor, obtained by rearranging
  # var(beta_j) = s^2/(n-1) * 1/var(X_j) * VIF_j

  VIF.df <- data.frame(variable=names(VIF),
                       vif=VIF,
                       row.names=NULL)

  return(VIF.df)
}
