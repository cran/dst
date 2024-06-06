## ----setup, include=FALSE-----------------------------------------------------
#devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst)
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
bpa1 <- bca(matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), c(0.8, 0.2), cnames=c("Peter", "John", "Mary"))
bpa1$tt
bpa1$spec

## -----------------------------------------------------------------------------
belplau1<-belplau(bpa1)
belplau1

## -----------------------------------------------------------------------------
bpa2 <- bca(matrix(c(0,1,1,1,1,1), nrow = 2, byrow = TRUE), c(0.6, 0.4), cnames=c("Peter", "John", "Mary"))
bpa2$tt
bpa2$spec

## -----------------------------------------------------------------------------
belplau2<-belplau(bpa2)
belplau2

## -----------------------------------------------------------------------------
bpa3<-dsrwon(bpa1, bpa2)

## -----------------------------------------------------------------------------
belplau3<-belplau(bpa3)
belplau3

## -----------------------------------------------------------------------------
bpa3_plus_singl <- addTobca(bpa3, tt = diag(3)) 
belplau(bpa3_plus_singl)

## -----------------------------------------------------------------------------
bpa3<-dsrwonLogsumexp(bpa1, bpa2, use_qq = TRUE)
belplau3 <- belplauHQQ(bpa3$qq,ttmatrixFromQQ(bpa3$qq,as.integer(bpa3$infovar[1,2]),unlist(bpa3$valuenames)))
round(belplau3, 5)

## -----------------------------------------------------------------------------
bpa3$tt<-ttmatrixFromQQ(bpa3$qq,as.integer(bpa3$infovar[1,2]),unlist(bpa3$valuenames))
bpa3$spec <- matrix(c(1:nrow(bpa3$tt),mFromQQ(bpa3$qq,bpa3$tt)), ncol = 2, byrow = FALSE, dimnames = list(NULL, c("specnb","mass")))
belplau3<-belplau(bpa3)
round(belplau3, 5)

## -----------------------------------------------------------------------------
bpa3_plus_singl <- addTobca(bpa3, tt = diag(3)) 
round(belplau(bpa3_plus_singl), 5)

