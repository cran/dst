## ----setup, include=FALSE-----------------------------------------------------
library(dst) 
# devtools::load_all(".") # only used in place of dst when testing with R-devel
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
bpa1 <- bca(matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), c(0.8, 0.2), c("Peter", "John", "Mary"))
bpa1$tt
bpa1$spec

## -----------------------------------------------------------------------------
belplau1<-belplau(bpa1)
belplau1

## -----------------------------------------------------------------------------
bpa2 <- bca(matrix(c(0,1,1,1,1,1), nrow = 2, byrow = TRUE), c(0.6, 0.4), c("Peter", "John", "Mary"))
bpa2$tt
bpa2$spec

## -----------------------------------------------------------------------------
belplau2<-belplau(bpa2)
belplau2

## -----------------------------------------------------------------------------
bpa3<-dsrwon(bpa1, bpa2)
bpa3$tt
bpa3$spec

## -----------------------------------------------------------------------------
belplau3<-belplau(bpa3)
belplau3

## -----------------------------------------------------------------------------
bpa3_plus_singl <- addTobca(bpa3, tt = diag(3)) 
belplau(bpa3_plus_singl)

