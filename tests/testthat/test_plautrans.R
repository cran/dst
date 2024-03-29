# Tests "plautrans" function
context("Plausibility transformation")
library(dst)
test_that("plautrans", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "x1", idvar = 1)
  expect_error(plautrans(x1) , "Input argument not of class bcaspec.")
  # T2 check that the input is a normalized bca
  x2 <- bca(tt = matrix(c(0,0,0,0,1,1,1,1,0,1,1,1),nrow=4, byrow = TRUE), m=c(0.2,0.1,0.4, 0.3), cnames =c("a", "b", "c"), varnames = "x2", idvar = 1)
  expect_error(plautrans(x2) ,"Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
})