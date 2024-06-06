## ----setup, include=FALSE-----------------------------------------------------
#devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst) 
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
DSM1_tt <- matrix(c(1,0,0,1,1,1), nrow = 2 + 1, ncol = 2, byrow = TRUE)
DSM1_m <- matrix(c(0.3,0.7,0), nrow = 2 + 1, ncol = 1)
DSM1_cnames <- c("a is True", "a is False")
DSM1_varnames <- c("a")
DSM1_idvar <- 1
DSM1 <- bca(DSM1_tt, DSM1_m, cnames = DSM1_cnames, idvar = DSM1_idvar, varnames = DSM1_varnames)
bcaPrint(DSM1)

## -----------------------------------------------------------------------------
DSM3_tt <- matrix(c(1,1), nrow = 1, ncol = 2, byrow = TRUE)
DSM3_m <- matrix(c(1), nrow = 1, ncol = 1)
DSM3_cnames <- c("b is True", "b is False")
DSM3_varnames <- c("b")
DSM3_idvar <- 3
DSM3 <- bca(DSM3_tt, DSM3_m, cnames = DSM3_cnames, idvar = DSM3_idvar, varnames = DSM3_varnames)
bcaPrint(DSM3)

## -----------------------------------------------------------------------------
DSM2_tt <- matrix(c(0,1,0,1,
                    0,1,1,0,
                    1,0,1,0,
                    1,1,1,1), nrow = 3 + 1, ncol = 4, byrow = TRUE, dimnames = list(NULL, c("a is True", "a is False", "b is True", "b is False")))
DSM2_m <- matrix(c(1,1,1,2,
                   1,1,1,0), nrow = 3 + 1, ncol = 2)
DSM2_infovar <- matrix(c(1,3,2,2), nrow = 2, ncol = 2)
DSM2_varnames <- c("a","b")
DSM2 <- bcaRel(DSM2_tt, DSM2_m, DSM2_infovar, DSM2_varnames)
bcaPrint(DSM2)

## -----------------------------------------------------------------------------
uporj_DSM1 <- extmin(DSM1, DSM2)
bcaPrint(uporj_DSM1)

## -----------------------------------------------------------------------------
uporj_DSM1_xDSC_DSM2 <- dsrwon(uporj_DSM1, DSM2)
bcaPrint(uporj_DSM1_xDSC_DSM2)

## -----------------------------------------------------------------------------
uporj_DSM1_xDSC_DSM2_dproj <- elim(uporj_DSM1_xDSC_DSM2, 1)
bcaPrint(uporj_DSM1_xDSC_DSM2_dproj)

## -----------------------------------------------------------------------------
DSA <- belplau(uporj_DSM1_xDSC_DSM2_dproj)
DSA

