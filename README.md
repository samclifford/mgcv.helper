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

A demonstration
---------------

First, let's generate some data and fit a GAM

``` r
library(mgcv)
library(dplyr)
library(mgcv.helper)

set.seed(101)

N <- 100
x1 <- runif(n=N)
x2 <- runif(n=N)
x3 <- runif(n=N) + 0.9*x1 - 1.75*x2

df <- data.frame(x1 = x1,
                 x2 = x2,
                 x3 = x3) %>%
  mutate(y = rnorm(n=N,
                   mean = 1 - 2*x1 + 3*x2 - 0.5*x3,
                   sd = 0.5))

fit1 <- gam(data=df, y ~ x1 + x2 + x3)

summary(fit1)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## y ~ x1 + x2 + x3
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.9966     0.1410   7.071 2.48e-10 ***
    ## x1           -1.7966     0.2215  -8.111 1.65e-12 ***
    ## x2            2.8581     0.3377   8.464 2.94e-13 ***
    ## x3           -0.6007     0.1652  -3.637 0.000446 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## 
    ## R-sq.(adj) =  0.891   Deviance explained = 89.5%
    ## GCV = 0.22009  Scale est. = 0.21129   n = 100

Now that we've got our parameter estimates, etc., we may want to know the confidence intervals for them,

``` r
confint(fit1)
```

    ## Joining, by = "Term"

    ##          Term   Estimate        SE          L          U
    ## 1 (Intercept)  0.9966307 0.1409523  0.7168425  1.2764188
    ## 2          x1 -1.7966401 0.2215098 -2.2363335 -1.3569467
    ## 3          x2  2.8581132 0.3376971  2.1877896  3.5284367
    ## 4          x3 -0.6007309 0.1651840 -0.9286186 -0.2728432

And we might want to know whether or not our variables are related to each other in the sense that their variance inflation factors are large

``` r
vif.gam(fit1)
```

    ##   variable      vif
    ## 1       x1 2.047334
    ## 2       x2 4.900343
    ## 3       x3 5.393642
