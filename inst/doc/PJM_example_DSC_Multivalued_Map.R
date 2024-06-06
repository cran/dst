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
tt_SSMacp <- matrix(c(1,1,1), nrow = 1, ncol = 3, byrow = TRUE)
m_DSMacp <- matrix(c(1), nrow = 1, ncol = 1)
cnames_SSMacp <- c("A", "C", "P") 
varnames_SSMacp <- "ACP"
idvar_SSMacp <- 3
DSMacp <- bca(tt_SSMacp, m_DSMacp, cnames = cnames_SSMacp, idvar = idvar_SSMacp, varnames = varnames_SSMacp)
bcaPrint(DSMacp)

## -----------------------------------------------------------------------------
tt_SSMR_1 <- matrix(c(1,0,0,1,0,
                     1,0,1,0,0,
                     
                     0,1,1,0,0,
                     0,1,0,1,0,
                     0,1,0,0,1,
                     
                     1,1,1,1,1), nrow = 2 + 3 + 1, ncol = 2 + 3, byrow = TRUE, dimnames = list(NULL, c("w1y","w1n","A","C","P")))
spec_DSMR_1 <- matrix(c(1,1,1,1,1,2,1,1,1,1,1,0), nrow = 2 + 3 + 1, ncol = 2)
infovar_SSMR_1 <- matrix(c(1,3,2,3), nrow = 2, ncol = 2)
varnames_SSMR_1 <- c("w1", "ACP")
relnb_SSMR_1 <- 1
DSMR_1 <- bcaRel(tt_SSMR_1, spec_DSMR_1, infovar_SSMR_1, varnames_SSMR_1, relnb_SSMR_1)
bcaPrint(DSMR_1)

## -----------------------------------------------------------------------------
tt_SSMR_2 <- matrix(c(1,0,0,1,0,
                     1,0,0,0,1,

                     0,1,1,0,0,
                     0,1,0,1,0,
                     0,1,0,0,1,
                     
                     1,1,1,1,1), nrow = 2 + 3 + 1, ncol = 2 + 3, byrow = TRUE, dimnames = list(NULL, c("w2y","w2n","A","C","P")))
spec_DSMR_2 <- matrix(c(1,1,1,1,1,2,1,1,1,1,1,0), nrow = 2 + 3 + 1, ncol = 2)
infovar_SSMR_2 <- matrix(c(2,3,2,3), nrow = 2, ncol = 2)
varnames_SSMR_2 <- c("w2", "ACP")
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

