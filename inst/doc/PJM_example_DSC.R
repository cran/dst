## ----setup, include=FALSE-----------------------------------------------------
#devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst) 
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
tt_SSMw1 <- matrix(c(1,0,0,1,1,1), nrow = 3, ncol = 2, byrow = TRUE)
m_DSMw1 <- matrix(c(0.4,0.6,0), nrow = 3, ncol = 1)
cnames_SSMw1 <- c("w1y", "w1n") 
varnames_SSMw1 <- "w1"
idvar_SSMw1 <- 1
DSMw1 <- bca(tt_SSMw1, m_DSMw1, cnames = cnames_SSMw1, idvar = idvar_SSMw1, varnames = varnames_SSMw1)
bcaPrint(DSMw1)

## -----------------------------------------------------------------------------
tt_SSMw2 <- matrix(c(1,0,0,1,1,1), nrow = 3, ncol = 2, byrow = TRUE)
m_DSMw2 <- matrix(c(0.3,0.7,0), nrow = 3, ncol = 1)
cnames_SSMw2 <- c("w2y", "w2n") 
varnames_SSMw2 <- "w2"
idvar_SSMw2 <- 2
DSMw2 <- bca(tt_SSMw2, m_DSMw2, cnames = cnames_SSMw2, idvar = idvar_SSMw2, varnames = varnames_SSMw2)
bcaPrint(DSMw2)

## -----------------------------------------------------------------------------
tt_SSMA <- matrix(c(1,1), nrow = 1, ncol = 2, byrow = TRUE)
m_DSMA <- matrix(c(1), nrow = 1, ncol = 1)
cnames_SSMA <- c("Ay", "An") 
varnames_SSMA <- "A"
idvar_SSMA <- 3
DSMA <- bca(tt_SSMA, m_DSMA, cnames = cnames_SSMA, idvar = idvar_SSMA, varnames = varnames_SSMA)
bcaPrint(DSMA)

## -----------------------------------------------------------------------------
tt_SSMC <- matrix(c(1,1), nrow = 1, ncol = 2, byrow = TRUE)
m_DSMC <- matrix(c(1), nrow = 1, ncol = 1)
cnames_SSMC <- c("Cy", "Cn") 
varnames_SSMC <- "C"
idvar_SSMC <- 4
DSMC <- bca(tt_SSMC, m_DSMC, cnames = cnames_SSMC, idvar = idvar_SSMC, varnames = varnames_SSMC)
bcaPrint(DSMC)

## -----------------------------------------------------------------------------
tt_SSMP <- matrix(c(1,1), nrow = 1, ncol = 2, byrow = TRUE)
m_DSMP <- matrix(c(1), nrow = 1, ncol = 1)
cnames_SSMP <- c("Py", "Pn") 
varnames_SSMP <- "P"
idvar_SSMP <- 5
DSMP <- bca(tt_SSMP, m_DSMP, cnames = cnames_SSMP, idvar = idvar_SSMP, varnames = varnames_SSMP)
bcaPrint(DSMP)

## -----------------------------------------------------------------------------
tt_SSMR_1 <- matrix(c(1,0,1,0,1,0,0,1,
                     1,0,0,1,1,0,0,1,
                     1,0,1,0,0,1,0,1,
                     
                     0,1,1,0,0,1,0,1,
                     0,1,0,1,1,0,0,1,
                     0,1,0,1,0,1,1,0,
                     0,1,1,0,1,0,0,1,
                     0,1,0,1,1,0,1,0,
                     0,1,1,0,0,1,1,0,
                     0,1,1,0,1,0,1,0,
                     
                     1,1,1,1,1,1,1,1), nrow = 3 + 7 + 1, ncol = 8, byrow = TRUE, dimnames = list(NULL, c("w1y","w1n","Ay","An","Cy","Cn","Py","Pn")))
spec_DSMR_1 <- matrix(c(1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,0), nrow = 3 + 7 + 1, ncol = 2)
infovar_SSMR_1 <- matrix(c(1,3,4,5,2,2,2,2), nrow = 4, ncol = 2)
varnames_SSMR_1 <- c("w1", "A", "C", "P")
relnb_SSMR_1 <- 1
DSMR_1 <- bcaRel(tt_SSMR_1, spec_DSMR_1, infovar_SSMR_1, varnames_SSMR_1, relnb_SSMR_1)
bcaPrint(DSMR_1)

## -----------------------------------------------------------------------------
tt_SSMR_2 <- matrix(c(1,0,0,1,1,0,0,1,
                     1,0,0,1,0,1,1,0,
                     1,0,0,1,1,0,1,0,
                     
                     0,1,1,0,0,1,0,1,
                     0,1,0,1,1,0,0,1,
                     0,1,0,1,0,1,1,0,
                     0,1,1,0,1,0,0,1,
                     0,1,0,1,1,0,1,0,
                     0,1,1,0,0,1,1,0,
                     0,1,1,0,1,0,1,0,
                     
                     1,1,1,1,1,1,1,1), nrow = 3 + 7 + 1, ncol = 8, byrow = TRUE, dimnames = list(NULL, c("w2y","w2n","Ay","An","Cy","Cn","Py","Pn")))
spec_DSMR_2 <- matrix(c(1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,0), nrow = 3 + 7 + 1, ncol = 2)
infovar_SSMR_2 <- matrix(c(2,3,4,5,2,2,2,2), nrow = 4, ncol = 2)
varnames_SSMR_2 <- c("w2", "A", "C", "P")
relnb_SSMR_2 <- 2
DSMR_2 <- bcaRel(tt_SSMR_2, spec_DSMR_2, infovar_SSMR_2, varnames_SSMR_2, relnb_SSMR_2)
bcaPrint(DSMR_2)

## -----------------------------------------------------------------------------
DSMw1_uproj <- extmin(DSMw1,DSMR_1)
bcaPrint(DSMw1_uproj)

## -----------------------------------------------------------------------------
DSM1 <- dsrwon(DSMw1_uproj,DSMR_1)
bcaPrint(DSM1)

## -----------------------------------------------------------------------------
DSM1_dproj <- elim(DSM1,1)
bcaPrint(DSM1_dproj)

## -----------------------------------------------------------------------------
DSMw2_uproj <- extmin(DSMw2,DSMR_2)
DSM2 <- dsrwon(DSMw2_uproj,DSMR_2)
DSM2_dproj <- elim(DSM2,2)
bcaPrint(DSM2_dproj)

## -----------------------------------------------------------------------------
DSM3 <- dsrwon(DSM1_dproj,DSM2_dproj)
bcaPrint(DSM3)

## -----------------------------------------------------------------------------
DSM3_dprojSSMC <- elim(elim(DSM3, 3), 5)
bcaPrint(DSM3_dprojSSMC)

