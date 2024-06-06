## ----setup, include=FALSE-----------------------------------------------------
# devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst) 
knitr::opts_chunk$set(echo = TRUE)

## ----chk1---------------------------------------------------------------------
tt <- matrix(c(0,1,0,1,0,1,
               1,0,1,0,1,0,
               1,0,1,0,0,1,
               0,1,1,0,0,1,
               1,0,0,1,0,1,
               1,1,1,1,1,1), nrow = 2 + 3 + 1, ncol = 6, byrow = TRUE, dimnames = list(NULL,c("a1 no", "a1 yes", "a2 no", "a2 yes", "b no", "b yes")))
spec <- matrix(c(1,1,1,1,1,2,1,1,1,1,1,0), nrow = 5 + 1, ncol = 2)
infovar <- matrix(c(1,2,3,2,2,2), nrow = 3, ncol = 2)
varnames <- c("a1","a2","b")
bcaRel1<-bcaRel(tt,spec,infovar,varnames)
cat("The implication relation","\n")
bcaPrint(bcaRel1)

## ----chk2---------------------------------------------------------------------
tt <- matrix(c(0,1,1,1), nrow = 2, ncol = 2, dimnames = list(NULL, c("a1 no", "a1 yes")))
m <- c(0.3,0.7)
varnames <- "a1"
idvar <- 1
bca1 <- bca(tt, m, idvar=idvar, varnames=varnames)
bcaPrint(bca1)

## ----chk3---------------------------------------------------------------------
tt <- matrix(c(0,1,1,1), nrow = 2, ncol = 2, dimnames = list(NULL, c("a2 no", "a2 yes")))
m <- c(0.4,0.6)
varnames <- "a2"
idvar <- 2
bca2 <- bca(tt, m, idvar=idvar, varnames=varnames)
bcaPrint(bca2)

## ----chk4---------------------------------------------------------------------
bca1_extmin <- extmin(bca1,bcaRel1)
bcaPrint(bca1_extmin)

## ----chk5---------------------------------------------------------------------
bca2_extmin <- extmin(bca2,bcaRel1)
bcaPrint(bca2_extmin)

## ----chk6---------------------------------------------------------------------
bca12_extmin <- dsrwon(bca1_extmin,bca2_extmin)
bcaPrint(bca12_extmin)

## ----chk7---------------------------------------------------------------------
bca12_extmin_dsrwon_bcaRel1 <- dsrwon(bca12_extmin,bcaRel1)
bcaPrint(bca12_extmin_dsrwon_bcaRel1)

## ----chk8---------------------------------------------------------------------
bca12_extmin_elim1 <- elim(bca12_extmin_dsrwon_bcaRel1,1)
bcaPrint(bca12_extmin_elim1)

## ----chk9---------------------------------------------------------------------
bca12_extmin_elim12 <- elim(bca12_extmin_elim1,2)
bcaPrint(bca12_extmin_elim12)

## ----chk10--------------------------------------------------------------------
belplau(bca12_extmin_elim12)

## ----chk11--------------------------------------------------------------------
tt <- matrix(c(0,1,0,1,
               1,0,0,1,
               1,0,1,0,
               1,1,1,1), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(NULL,c("a1 no", "a1 yes", "b no", "b yes")))
spec <- matrix(c(1,1,1,2,1,1,1,0), nrow = 4, ncol = 2)
infovar <- matrix(c(1,3,2,2), nrow = 2, ncol = 2)
varnames <- c("a1","b")
bcaRel1<-bcaRel(tt,spec,infovar,varnames)
bcaPrint(bcaRel1)

## ----chk12--------------------------------------------------------------------
tt <- matrix(c(0,1,0,1,
               1,0,0,1,
               1,0,1,0,
               1,1,1,1), nrow = 4, ncol = 4, byrow = TRUE, dimnames = list(NULL,c("a2 no", "a2 yes", "b no", "b yes")))
spec <- matrix(c(1,1,1,2,1,1,1,0), nrow = 4, ncol = 2)
infovar <- matrix(c(2,3,2,2), nrow = 2, ncol = 2)
varnames <- c("a2","b")
bcaRel2<-bcaRel(tt,spec,infovar,varnames)
bcaPrint(bcaRel2)

## ----chk13--------------------------------------------------------------------
bca1_extmin <- extmin(bca1,bcaRel1)
bca1_extmin_bcaRel1_dsrwon <- dsrwon(bca1_extmin, bcaRel1)
bca1_extmin_bcaRel1_dsrwon_elim <- elim(bca1_extmin_bcaRel1_dsrwon, 1)
bcaPrint(bca1_extmin_bcaRel1_dsrwon_elim)

## ----chk14--------------------------------------------------------------------
bca2_extmin <- extmin(bca2,bcaRel2)
bca2_extmin_bcaRel2_dsrwon <- dsrwon(bca2_extmin, bcaRel2)
bca2_extmin_bcaRel2_dsrwon_elim <- elim(bca2_extmin_bcaRel2_dsrwon, 2)
bcaPrint(bca2_extmin_bcaRel2_dsrwon_elim)

## ----chk15--------------------------------------------------------------------
bca12 <- dsrwon(bca1_extmin_bcaRel1_dsrwon_elim,bca2_extmin_bcaRel2_dsrwon_elim)
bcaPrint(bca12)

## ----chk16--------------------------------------------------------------------
belplau(bca12)

