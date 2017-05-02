
#' Confidence Intervals for Model Parameters
#'
#' Computes confidence intervals for one or more parameters in a fitted model.
#' This is a method specific to the \code{"gam"} class from package
#' \code{"mgcv"}.
#' @importFrom magrittr %>%
#' @param object a fitted model object of class \code{"gam"}.
#' @param parm a specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names. If missing, all
#' parameters are considered.
#' @param level the confidence level required.
#'
#' @return A tidy data frame containing parameter names, estimates and
#' confidence intervals for parametric terms
#' @export
#'
#' @examples
#' set.seed(101)
#' library(dplyr)
#' library(mgcv)
#' dat <- data.frame(x = runif(n=100),
#'                   y = runif(n=100)) %>%
#'   dplyr::mutate(z = rnorm(n=100,
#'                    mean = 1 - 2*x - sin(2*pi*y),
#'                    sd = 0.1))
#'
#' fit1 <- gam(data=dat, z ~ y + s(x))
#'
#' confint(fit1)
#' confint(fit1, parm="y", level=0.8)
#'
confint.gam <- function(object, parm = NULL, level = 0.95) {
  # a method for extracting confidence intervals and returning a tidy data frame

  obj.s <- summary.gam(object)

  E <- data.frame(Estimate = obj.s$p.coeff) %>%
    dplyr::mutate(Term = row.names(.)) %>%
    dplyr::select(Term, Estimate)

  SE <- data.frame(SE = obj.s$se) %>%
    dplyr::mutate(Term = row.names(.)) %>%
    dplyr::select(Term, SE)

  if (is.null(parm)){
    parm <- E$Term
  }

  nu <- obj.s$residual.df

  dplyr::inner_join(E, SE) %>%
    dplyr::filter(Term %in% parm) %>%
    dplyr::mutate(L = Estimate +
             SE * qt(df = nu,
                     p = (1 - level) / 2),
           U = Estimate +
             SE * qt(df = nu,
                     p = 1 - (1 - level) / 2)) %>%
    return

}
