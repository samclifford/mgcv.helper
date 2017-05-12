
#' Confidence Intervals for Model Parameters
#'
#' Computes confidence intervals for one or more parameters in a fitted model.
#' This is a method specific to the \code{"gam"} class from package
#' \code{"mgcv"}.
#' @importFrom stats qt
#' @importFrom tibble tibble
#' @importFrom mgcv summary.gam
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @param object a fitted model object of class \code{"gam"}.
#' @param parm a specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names. If missing, all
#' parameters are considered.
#' @param level the confidence level required.
#' @param ... not implemented
#'
#' @return A tidy data frame containing parameter names, Estimates and
#' confidence intervals for parametric terms
#'
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
confint.gam <- function(object, parm = NULL, level = 0.95, ...) {
  # a method for extracting confidence intervals and returning a tidy data frame

  obj.s <- mgcv::summary.gam(object)

  E <- obj.s$p.coeff %>%
    tibble(Estimate = .,
              term=names(.)) %>%
    #dplyr::mutate(., term = row.names(.)) %>%
    dplyr::select(., term, Estimate)

  SE <- obj.s$se %>%
    tibble(se = .,
           term = names(.)) %>%
    #dplyr::mutate(., term = row.names(.)) %>%
    dplyr::select(., term, se)

  if (is.null(parm)){
    parm <- E$term
  }

  nu <- obj.s$residual.df

  my.tbl <- dplyr::inner_join(E, SE) %>%
    dplyr::filter(., term %in% parm) %>%
    dplyr::mutate(.,
                  L = Estimate +
                    se * stats::qt(df = nu,
                                   p = (1 - level) / 2),
                  U = Estimate +
                    se * stats::qt(df = nu,
                                   p = 1 - (1 - level) / 2))

  names(my.tbl)[3] <- "Std. Error"

  names(my.tbl)[4] <- sprintf("%.1f%%",
                              100*(1-level)/2)
  names(my.tbl)[5] <- sprintf("%.1f%%",
                              100*(1-(1-level)/2))

  return(my.tbl)

}
