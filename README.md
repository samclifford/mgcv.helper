Helper functions for mgcv
================

Frustrated with `confint` not returning a data frame of confidence intervals when applied to a `gam` from `mgcv`?

Want that, but tidy?

Fear not, `mgcv.helper` is here!

This bare bones package allows you to call `confint` the same way you would for an `lm`, `glm` or `nls` object and get back a tidy data frame filled with the confidence intervals for parametric terms. Arguments taken are:

-   `object` - a fitted `gam`
-   `parm` - a vector of parameters you care about
-   `level` - a confidence level

We also provide a method for calculating the vif from a fitted `gam` object.

Future plans include:

-   Allowing multiple confidence levels to be used
-   Renaming columns corresponding to confidence intervals with the quantile value
-   Calculating confidence intervals for univariate, non-parametric smooths by returning a tidy data frame with `x` values and corresponding estimates and lower and upper confidence bounds

Installation instructions
-------------------------

``` r
# library(devtools)
devtools::install_github("samclifford/mgcv.helper")
```
