## ----setup, include=FALSE-----------------------------------------------------
# devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst) 
knitr::opts_chunk$set(echo = TRUE)

## ----bpa1 definition,  echo = FALSE, warning=FALSE----------------------------
Theta<-matrix(c(1,0,0,0,1,0,0,0,1,1,1,1), nrow = 4, byrow = TRUE)
H <- bca(tt=matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = TRUE), m = c(0.2, 0.3, 0.5), cnames = c("a", "b", "c"), idvar = 1)
cat("The prior distribution H","\n")
bcaPrint(H)
# round(belplau(H, h=Theta), digits = 3)

## ----bpa2 definition,  echo = FALSE, warning=FALSE----------------------------
bpa2 <- bca(tt=rbind(diag(x=1, nrow=3), matrix(c(0,1,1,1,1,1), nrow=2, byrow = TRUE)), m = c(0,0,0,1,0), cnames = c("a", "b", "c"), idvar = 1)
Event <-  addTobca(bpa2, tt = diag(x=1, nrow = 3))
cat("Setting an Event E = {b,c} with mass = 1","\n")
bcaPrint(Event)

## ----H_Event Dempster_rule1,  echo = FALSE, warning=FALSE---------------------
H_Event <- dsrwon(H, bpa2)
cat("The combination of H and Event E","\n")
bcaPrint(H_Event)

## ----H_Event Dempster_rule2,  echo = FALSE, warning=FALSE---------------------
H_given_E <- nzdsr(H_Event)
cat("The posterior distribution P(H|E)","\n")
bcaPrint(H_given_E)

## ----H_Event Dempster_rule3,  echo = FALSE, warning=FALSE---------------------
round(belplau(H_given_E, h=Theta), digits = 3)

## ----bpa1_copy,  echo = FALSE, warning=FALSE----------------------------------
Theta<-matrix(c(1,0,0,0,1,0,0,0,1,1,1,1), nrow = 4, byrow = TRUE)
X <- bca(tt=matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = TRUE), m = c(0.2, 0.3, 0.5), cnames = c("a", "b", "c"), idvar = 1, varnames = "x")
cat("The prior distribution","\n")
bcaPrint(X)

## ----relation,  echo = FALSE, warning=FALSE-----------------------------------
# bpa4 <- bca(tt=matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3, byrow = TRUE), m = c(1, 0, 0), cnames = c("d", "e", "f"), idvar = 4, varnames = "y")
# bcaPrint(bpa4)
#
cat("Specify information on variables, description matrix and mass vector","\n")
inforvar_EX <- matrix(c(1,4,3,3), ncol = 2,  dimnames = list(NULL, c("varnb", "size")) )
cat("Identifying variables and frames","\n")
inforvar_EX
cat("Note that variables numbers must be in increasing order","\n")
# 
tt_EX <- matrix(c(1,0,0,1,0,0,
                   0,1,0,1,0,0,
                   0,0,1,1,0,0,
                   1,1,1,1,1,1), ncol = 6, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c", "d", "e", "f")))
cat("The description matrix of the relation between X and E","\n")
tt_EX
cat("Note Columns of matrix must follow variables ordering. ","\n")
#
spec_EX <-  matrix(c(1:4, 0.1, 0.2, 0.7, 0 ), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
cat("Mass specifications","\n")
spec_EX
# 
rel_EX <- bcaRel(tt = tt_EX, spec = spec_EX, infovar = inforvar_EX, varnames = c("x", "y"), relnb = 1)
cat("The relation between Evidence E and X","\n")
bcaPrint(rel_EX)

## ----X_xtnd,  echo = FALSE, warning=FALSE-------------------------------------
X_xtnd <- extmin(X, relRef = rel_EX)
cat("Prior X extended in product space of (X,E","\n")
bcaPrint(X_xtnd)

## ----relation2,  echo = FALSE, warning=FALSE----------------------------------
comb_X_EX <- dsrwon(X_xtnd, rel_EX)
cat("Mass distribution of the combination of X extended and E_X","\n")
bcaPrint(comb_X_EX)

## ----relation3,  echo = FALSE, warning=FALSE----------------------------------
norm_comb_X_EX <- nzdsr(comb_X_EX)
cat("The normalized mass distribution of the combination of X extended and E_X","\n")
bcaPrint(norm_comb_X_EX)
dist_XgE <- elim(norm_comb_X_EX, xnb = 4)
cat("The posterior distribution P(X|E) for (a,d), (b,d), (c,d), after eliminating variable E","\n")
bcaPrint(dist_XgE)

