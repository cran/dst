## ----setup, include = FALSE---------------------------------------------------
#
# devtools::load_all(".") # only used in place of dst when testing with R-devel
# attach package dst
library(dst) 
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Evidence for sun
# All the events of interest are encoded in a binary matrix tt.  
# Each column of the matrix is a possible value. 
# Each row is subset of the set of possible values, described by a complete disjunctive coding
Weather_tt <- matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE)
Weather <-  bca(tt = Weather_tt, m= c(0, 0.45, 0.55), cnames =c("Sun", "NoSun"), varnames = "Weather", idvar = 1)
Weather$tt
# The belief function of Weather
belplau(Weather)
tabresul(Weather)

## -----------------------------------------------------------------------------
plautrans(Weather)

## -----------------------------------------------------------------------------
# Relation between Rain and Roadworks
# Define variable Rain. Values: Ry for rain = yes, Rn for rain = no
rain <-  bca(tt = matrix(c(1,0,0,1,1,1), ncol = 2, byrow = TRUE), m=c(0,0, 1), cnames=c("Ry", "Rn"),  varnames = "Rain", idvar = 5)
# Define variable Roadworks Values: Wy for rdworks = yes, Wn for rdworks = no
# Define variable Roadworks
rdworks <-  bca(tt= matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0, 0, 1), cnames =c("Wy", "Wn"), varnames = "RdWorks", idvar = 4)
# Establish the relation between Rain and Roadworks
# A simple implication rule
# the binary matrix
ttrwt <- matrix(c(0,1,0,1,
                  0,1,1,0,
                  1,0,0,1,
                  1,1,1,1), nrow=4, byrow = TRUE, dimnames = list(NULL, c("Wy", "Wn", "Ry", "Rn")) )
# I use the function nameRows to name the rows here
rownames(ttrwt) <- nameRows(ttrwt)
ttrwt
inforwt <- matrix(c(4,5,2,2), ncol = 2,  dimnames = list(NULL, c("varnb", "size")) )
specrwt <-  matrix(c(1,1,1,2,
                     0.9,0.9,0.9,0.1), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
# The relation
noW_if_R <- bcaRel(tt = ttrwt, spec = specrwt, infovar = inforwt, varnames = c("RdWorks", "Rain"), relnb = 1)
 noW_if_R

## -----------------------------------------------------------------------------
# Evidence of rain on the day of departure
rain$spec[,2] <- c(0.6, 0, 0.4)
bcaPrint(rain)

## -----------------------------------------------------------------------------
# Evidence of rain extended to the space W x R
rain_xtnd <- extmin(rain, noW_if_R)
bcaPrint(rain_xtnd)

## -----------------------------------------------------------------------------
# combine the relation noW_if_R with variable rain extended on W x R (rain_xtnd)
comb_rel <- nzdsr(dsrwon(rain_xtnd, noW_if_R))
bcaPrint(comb_rel)

## -----------------------------------------------------------------------------
# marginalize to variable roadworks by eliminating variable rain (variable nb = 5)
roadworks_ev <- elim(comb_rel, xnb = 5)
belplau(roadworks_ev)
# use function addTobca to show all the singletons
roadworks_ev_plus_sing <- addTobca(roadworks_ev, tt = matrix(c(1,0), ncol = 2))
tabresul(roadworks_ev_plus_sing)
plautrans(roadworks_ev_plus_sing)

