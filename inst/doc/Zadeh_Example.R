## ----setup, include=FALSE-----------------------------------------------------
#
# devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst)  # attach package dst
#
# knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----"Zadeh's example", echo = FALSE, warning=FALSE---------------------------
# Diagnosis from Expert 1. Coding the evidence with the bca function
Expert1 <- bca(tt = matrix(c(1,0,0,0,1,0,1,1,1), ncol=3, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("M", "T", "C"), varnames = "Diagnosis1", idvar = 1)
# show the definition of Expert1
cat("Space of possibilities and Basic Chance Assignment of Expert 1")
Expert1$valuenames
cat("\r")
bcaPrint(Expert1)
#
# Diagnosis from Expert 2. Coding the evidence with the bca function
Expert2 <- bca(tt = matrix(c(0,1,0,0,0,1,1,1,1), ncol=3, byrow=TRUE), m= c(0.01, 0.99, 0), cnames =c("M", "T", "C"), varnames = "Diagnosis2", idvar = 2)
# show the definition of Expert2
cat("\r")
cat("Space of possibilities and Basic Chance Assignment of Expert 2")
Expert2$valuenames
cat("\r")
bcaPrint(Expert2)
# Combination of Expert 1 and Expert 2 using Dempster's rule
cat("\r")
cat("Combination of the two experts by Dempster's rule")
Ze1e2 <- nzdsr(dsrwon(Expert1, Expert2, relnb = 1))
zz <- tabresul(Ze1e2)
format(as.data.frame(zz$mbp), digits=2)

## ----"pieces of evidence", echo = FALSE, warning=FALSE------------------------
library(dst)  # attach package dst
#
# Diagnosis from first expert (evidence e1 attached to variable D1)
e1 <- bca(tt = matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("M", "T"), varnames = "D1", idvar = 1)
#
# show the definition of e1
cat("Space of possibilities and Basic Chance Assignment of Expert 1")
e1$valuenames
cat("\r")
bcaPrint(e1)
#
# Diagnosis from second expert (evidence e2 attached to variable D2)
e2 <- bca(tt = matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("C", "T"), varnames = "D2", idvar = 2)
#
# show the definition of e2
cat("\r")
cat("Space of possibilities and Basic Chance Assignment of Expert 2")
e2$valuenames
cat("\r")
bcaPrint(e2)

## ----"relation", echo = FALSE, warning=FALSE----------------------------------
# 1. Defining the relation with a (0,1) matrix
tt_r1 <- matrix(c(1,0,1,0,1,0,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1), ncol = 7,byrow = TRUE)
colnames(tt_r1) = c("M", "T", "C", "T", "M", "T", "C")
#
# 2. Setting the mass function
spec_r1 = matrix(c(rep(1,7),2, rep(1,7), 0), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#
# 3. Names of variables names and dimension of their space of possibilities
info_r1 =matrix(c(1:3, 2,2,3), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
#
#  The relation between e1, e2 and a patient p
r1 <-bcaRel(tt = tt_r1, spec = spec_r1, infovar = info_r1, varnames = c("D1", "D2", "D"), relnb = 1)
#
cat(" The relation r1")
bcaPrint(r1)

## ----fig.show='hold', fig_caption: yes, echo=FALSE, message=FALSE-------------
# The network
if (requireNamespace("igraph", quietly = TRUE) ) {
library(igraph)
# Encode pieces of evidence and relations with an incidence matrix
rel1 <- 1*1:3 %in% r1$infovar[,1]
ev1 <- 1*1:3 %in% e1$infovar[,1]
ev2 <- 1*1:3 %in% e2$infovar[,1]

# information on variables
meddiag_vars1 <- c(r1$valuenames)
meddiag_vars <- rbind(r1$infovar)
meddiag_var_names <-names(meddiag_vars1)
rownames(meddiag_vars) <- meddiag_var_names
# infos on relations
meddiag_data_names <- c("e1", "e2", "r1")
# the incidence matrix
meddiag_hgm <- matrix(c(ev1,ev2, rel1), ncol=3, dimnames = list(c("D1", "D2", "D"), c("e1","e2", "r1")))
meddiag <- list(meddiag_hgm, meddiag_var_names, meddiag_data_names)
#
## The graph structure of the problem
#
meddiag_hg <- graph_from_incidence_matrix(incidence = meddiag_hgm, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
V(meddiag_hg)
# Show variables as circles, relations and evidence as rectangles
V(meddiag_hg)$shape <- c("circle", "crectangle")[V(meddiag_hg)$type+1]
V(meddiag_hg)$label.cex <- 0.6
V(meddiag_hg)$label.font <- 2
# render graph
plot(meddiag_hg, vertex.label = V(meddiag_hg)$name, vertex.size=(3+6*V(meddiag_hg)$type)*6, sub="Belief network for Zadeh's Example")
}

## ----Print incidence matrix, echo=FALSE---------------------------------------
cat("Row names are variables names (nodes).\n")
cat("Column names are for pieces of evidence and relations (edges).\n")
print(meddiag_hgm)

## ----Print names of evidence and relations, echo=FALSE------------------------
meddiag_data_names

## ----Define elimination order, echo=FALSE-------------------------------------
format(as.data.frame(cbind(r1$infovar, r1$varnames) ) )
elim_order = c(1, 2, 3)

## ----The peeling, echo = FALSE, warning=FALSE---------------------------------
# cat("\  ")
p <- peeling(vars_def = meddiag_vars1, hgm = meddiag_hgm, hg_rel_names = meddiag_data_names, elim_order = c(1, 2, 3), verbose = FALSE ) 
#
# add singletons with 0 mass to show all singletons in the results
p_sing <- addTobca(x = p,  tt = matrix(c(1,0,0,0,0,1), ncol=3))
# "The final result after elimination of variable D2"
cat("\  ")
zz <- tabresul(p_sing)
format(as.data.frame(zz$mbp), digits=2)

## ----echo = FALSE, warning=FALSE----------------------------------------------
format(as.data.frame(plautrans(p) ), digits = 4)

