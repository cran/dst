# Tests "bcaRel" function
test_that("bcaRel", {
  # T1 tt must be a matrix
  tt1<- c(1,0,1,1)
  spec1 <- matrix(c(1,1,1,2,0.75,0.75,0.75,0.25), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
  info1 =matrix(c(4,5,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
  expect_error(bcaRel(tt = tt1, spec = spec1, infovar = info1) , "tt parameter must be a \\(0,1\\) or logical matrix.")
  #
  # T2 spec must be a matrix
  tt2 <- matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),nrow=4, byrow = TRUE, dimnames =list(NULL, c("rWdy", "rWdn", "Ry", "Rn")) )
  spec2 <- c(1,1,1,2)
  expect_error(bcaRel(tt = tt2, spec = spec2, infovar = info1) , "spec parameter must be a 2 columns matrix.")
  #
  # T3 infovar must be a matrix
  info3 =c(2,2)
  expect_error(bcaRel(tt = tt2, spec = spec1, infovar = info3) , "infovar parameter must be a 2 column numerical matrix with variables numbers in fist column and with sum of 2nd column = ncol\\(tt\\).")
  #
  # T4: nrow(tt)  must equal nrow(spec)
  spec2 <- matrix(c(1,1,2,0.75,0.75,0.25), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
  expect_error(bcaRel(tt = tt2, spec = spec2, infovar = info1) ,"Error in input arguments: check your input data.")
  #
  # T5: sum of masses must equal 1
  spec3 <- matrix(c(1,1,1,2,0.75,0.75,75,0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
  expect_error(bcaRel(tt = tt2, spec = spec3, infovar = info1, varnames = c("W", "R"  )) ,"Sum of masses not equal to 1 : check your data.")
  #
  # T6: varnames argument missing
  expect_error(bcaRel(tt = tt2, spec = spec1, infovar = info1) ,"varnames argument missing.")
  #
  # T7: Result is of class "bcaspec"
  result <- bcaRel(tt = tt2, spec = spec1, infovar = info1, varnames = c("W", "R"  )) 
  expect_is(result, "bcaspec")
  #
    # T8: check for column names
  tt3 <- matrix(c(0,1,1,0,1,0,1,0,1,0,0,1,1,1,1,1),nrow=4, byrow = TRUE )
  expect_error(bcaRel(tt = tt3, spec = spec1, infovar = info1), "Column names of tt matrix are missing.")
  #
  # T9: check for last row of tt matrix
  tt <- matrix(c(0,1,0,1,0,1,
                 0,1,0,1,1,0,
                 1,0,1,0,0,1,
                 0,1,1,0,0,1,
                 1,0,0,1,0,1), nrow = 2 + 3, ncol = 6, dimnames = list(NULL,c("a1 no", "a1 yes", "a2 no", "a2 yes", "b no", "b yes")))
  spec <- matrix(c(1,1,1,1,1,1,1,1,1,1), nrow = 5, ncol = 2)
  infovar <- matrix(c(1,2,3,2,2,2), nrow = 3, ncol = 2)
  varnames <- c("a1","a2","b")
  expect_error( bcaRel(tt,spec,infovar,varnames), "The last row of parameter tt must be a row of ones.")
  #
  # T10. Variables names must start with a letter
  tt <- matrix(c(1,0,0,1,0,0,
                 0,1,0,0,1,0,
                 0,0,1,0,0,1,
                 rep(1,6)), ncol = 6, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c", "x", "y", "z")))
  spec <-  matrix(c(1,2,3,4,0.2,0.3,0.5,0), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
  inforvar <- matrix(c(1,3,3,3), ncol = 2,  dimnames = list(NULL, c("varnb", "size")) )
  expect_error(bcaRel(tt = tt, spec = spec, infovar = inforvar, varnames = c(1, 3), relnb = 1), "Names of variables must start with a letter." ) 
  #
})