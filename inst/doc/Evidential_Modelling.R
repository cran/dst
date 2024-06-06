## ----setup, include=FALSE-----------------------------------------------------
#devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst) 
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
tt_DSMa <- matrix(c(1,0,0,1,1,1), nrow = 2 + 1, ncol = 2, byrow = TRUE)
m_DSMa <- matrix(c(0.3,0.7,0), nrow = 2 + 1, ncol = 1)
cnames_DSMa <- c("a is T", "a is F") 
varnames_DSMa <- "a"
idvar_DSMa <- 1
DSMa <- bca(tt_DSMa, m_DSMa, cnames = cnames_DSMa, idvar = idvar_DSMa, varnames = varnames_DSMa)

## -----------------------------------------------------------------------------
tt_DSMh <- matrix(c(1,0,1,1), nrow = 1 + 1, ncol = 2, byrow = TRUE)
m_DSMh <- matrix(c(1,0), nrow = 1 + 1, ncol = 1)
cnames_DSMh <- c("h is T", "h is F") 
varnames_DSMh <- "h"
idvar_DSMh <- 2
DSMh <- bca(tt_DSMh, m_DSMh, cnames = cnames_DSMh, idvar = idvar_DSMh, varnames = varnames_DSMh)

## -----------------------------------------------------------------------------
tt_DSMe <- matrix(c(1,0,1,1), nrow = 1 + 1, ncol = 2, byrow = TRUE)
m_DSMe <- matrix(c(1,0), nrow = 1 + 1, ncol = 1)
cnames_DSMe <- c("e is T", "e is F")
varnames_DSMe <- "e"
idvar_DSMe <- 3
DSMe <- bca(tt_DSMe, m_DSMe, cnames = cnames_DSMe, idvar = idvar_DSMe, varnames = varnames_DSMe)

## -----------------------------------------------------------------------------
tt_DSM1 <- matrix(c(0,1,0,1,0,1,
                    0,1,1,0,0,1,
                    0,1,0,1,1,0,
                    0,1,1,0,1,0,
                    1,0,0,1,0,1,
                    1,0,0,1,1,0,
                    1,0,1,0,1,0,
                    1,1,1,1,1,1), nrow = 7 + 1, ncol = 6, byrow = TRUE, dimnames = list(NULL, c("a is T", "a is F", "h is T", "h is F", "e is T", "e is F")))
spec_DSM1 <- matrix(c(1,1,1,1,1,1,1,2,
                      1,1,1,1,1,1,1,0), nrow = 7 + 1, ncol = 2)
infovar_DSM1 <- matrix(c(1,2,3,2,2,2), nrow = 3, ncol = 2)
varnames_DSM1 <- c("a", "h", "e")
relnb_DSM1 <- 1
DSM1 <- bcaRel(tt_DSM1, spec_DSM1, infovar_DSM1, varnames_DSM1, valuenames_DSM1, relnb_DSM1)

## -----------------------------------------------------------------------------
tt_DSM2 <- matrix(c(0,1,0,1,
                    1,0,0,1,
                    1,0,1,0,
                    1,1,1,1), nrow = 3 + 1, ncol = 4, byrow = TRUE, dimnames = list(NULL, c("h is T", "h is F", "e is T", "e is F")))
spec_DSM2 <- matrix(c(1,1,1,2,
                      1,1,1,0), nrow = 3 + 1, ncol = 2)
infovar_DSM2 <- matrix(c(2,3,2,2), nrow = 2, ncol = 2)
varnames_DSM2 <- c("h","e")
relnb_DSM2 <- 2
DSM2 <- bcaRel(tt_DSM2, spec_DSM2, infovar_DSM2, varnames_DSM2, valuenames_DSM2, relnb_DSM2)

## -----------------------------------------------------------------------------
DSMa_uproj <- extmin(DSMa, DSM1)
DSMa_uproj_xDSC1_DSM1 <- dsrwon(DSMa_uproj, DSM1)
DSMa_uproj_xDSC1_DSM1_dproj <- elim(DSMa_uproj_xDSC1_DSM1, 1)
DSMh_uproj <- extmin(DSMh, DSM2)
DSMh_uproj_xDSC2_DSM2 <- dsrwon(DSMh_uproj, DSM2)
DSMa_uproj_xDSC1_DSM1_dproj_xDSC2_DSMh_uproj <- dsrwon(DSMa_uproj_xDSC1_DSM1_dproj, DSMh_uproj_xDSC2_DSM2)
DSMa_uproj_xDSC1_DSM1_dproj_xDSC2_DSMh_uproj_dproj <- elim(DSMa_uproj_xDSC1_DSM1_dproj_xDSC2_DSMh_uproj, 2)
DSMa_uproj_xDSC1_DSM1_dproj_xDSC2_DSMh_uproj_dproj_DSA <- belplau(DSMa_uproj_xDSC1_DSM1_dproj_xDSC2_DSMh_uproj_dproj)
DSMa_uproj_xDSC1_DSM1_dproj_xDSC2_DSMh_uproj_dproj_DSA

