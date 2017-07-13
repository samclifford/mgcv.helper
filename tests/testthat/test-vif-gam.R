context("vif.gam")

testthat::test_that("vif.gam works for an example", {
  library("dplyr")

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

  fit1 <- mgcv::gam(data=df, y ~ x1 + x2 + x3)


  expect_is(vif.gam(fit1), "tbl_df")
})

testthat::test_that("vif.gam works for an example with a Boolean variable", {
  library("dplyr")

  set.seed(101)
  N <- 100
  x1 <- runif(n=N)
  x2 <- runif(n=N)>0.5
  x3 <- runif(n=N) + 0.9*x1 - 1.75*x2

  df <- data.frame(x1 = x1,
                   x2 = x2,
                   x3 = x3) %>%
    mutate(y = rnorm(n=N,
                     mean = 1 - 2*x1 - 0.5*x3,
                     sd = 0.5))

  fit1 <- mgcv::gam(data=df, y ~ x1 + x2 + x3)


  expect_is(vif.gam(fit1), "tbl_df")
})
