#' Tidy Data Frame of Smooth Terms
#'
#' Returns a tidy data frame containing the mean and confidence intervals
#' for fitted smooths from a model of class \code{"gam"} from package
#' \code{"mgcv"}.
#' @importFrom tibble as.tibble
#' @importFrom mgcv plot.gam
#' @importFrom stats qt
#' @param object a fitted model object of class \code{"gam"}.
#' @param dimension the dimension of the smooths that are desired for extraction.
#' @param parm a specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names. If missing, all
#' parameters are considered (not yet implemented).
#' @param level the confidence level required.
#' @param ... not implemented
#'
#' @return A tidy data frame containing parameter names, estimates and
#' confidence intervals for non-parametric terms
#'
#' @export


tidy_smooths <- function(object, dimension=1, level=0.95){

  # can only handle 1 and 2d smooths at this point

  list.object <- mgcv::plot.gam(object,
                                select=0,
                                se=abs(stats::qt(p = (1-level)/2,
                                                 df = object$df.residual)))

  get_lengths_internal <- function(x){
    unlist(lapply(x[c("x", "y","fit")], FUN="length"))
  }

  lengths <- lapply(FUN = get_lengths_internal, object)
  dimensions <- sapply(X = lengths,
                       FUN = function(x){"y" %in% names(x)}) + 1

  plot.me <- which(dimensions == dimension)

  extract_smooth_internal <- function(X, dimension){

    if (dimension == 1){
      tidied <- with(X,
                     data.frame(x=x,
                                y=fit,
                                ymin=fit - se*se.mult,
                                ymax=fit + se*se.mult,
                                xlab=xlab,
                                ylab=ylab))
    } else {
      tidied <- with(X,
                     data.frame(x=x,
                                y=y,
                                z=fit,
                                zmin=fit - se*se.mult,
                                zmax=fit + se*se.mult,
                                xlab=xlab,
                                ylab=ylab,
                                main=main))
    }


    return(tidied)

  }


  return(tibble::as.tibble(do.call(rbind, lapply(X = list.object[plot.me],
                               FUN = extract_smooth_internal,
                               dimension=dimension))))

}
