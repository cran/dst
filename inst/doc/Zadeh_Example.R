## ----setup, include=FALSE-----------------------------------------------------
#
# devtools::load_all(".") # only used in place of dst when testing with R-devel
# attach package dst
library(dst)
#
# knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, warning=FALSE---------------------------------------------
# Diagnosis from Expert 1. Coding the evidence with the bca function
ZExpert1 <- bca(f= matrix(c(1,0,0,0,1,0,1,1,1), ncol=3, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("M", "T", "C"), infovarnames = "Diagnosis1", varnb = 1)
# show the definition of ZExpert1
cat("Diagnosis of Expert 1")
bcaPrint(ZExpert1)
#
# Diagnosis from Expert 2. Coding the evidence with the bca function
ZExpert2 <- bca(f= matrix(c(0,1,0,0,0,1,1,1,1), ncol=3, byrow=TRUE), m= c(0.01, 0.99, 0), cnames =c("M", "T", "C"), infovarnames = "Diagnosis2", varnb = 2)
# show the definition of ZExpert2
cat("Diagnosis of Expert 2")
bcaPrint(ZExpert2)
# Combination of Expert 1 and Expert 2 using Dempster's rule
cat("Combination of the two experts by Dempster's rule")
Ze1e2 <- nzdsr(dsrwon(ZExpert1, ZExpert2, relnb = 1))
tabresul(Ze1e2)

## -----------------------------------------------------------------------------
# attach package dst
library(dst)
#
# Diagnosis from first expert (function e1 attached to variable D1)
e1 <- bca(f= matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("M", "T"), infovarnames = "D1", varnb = 1)
#
# show the definition of e1
# "Diagnosis of Expert 1 (function e1 attached to variable D1)"
bcaPrint(e1)
#
# Diagnosis from second expert (function e2 attached to variable D2)
e2 <- bca(f= matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("C", "T"), infovarnames = "D2", varnb = 2)
#
# show the definition of e2
# "Diagnosis of Expert 2 (function e2 attached to variable D2)"
bcaPrint(e2)

## -----------------------------------------------------------------------------
# 1. Defining the relation with a (0,1) matrix
tt_r1 <- matrix(c(1,0,1,0,1,0,0,1,0,1,0,0,0,1,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,1,0,0,1,1,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1), ncol = 7,byrow = TRUE)
colnames(tt_r1) = c("M", "T", "C", "T", "M", "T", "C")
#
# 2. Setting the mass function
spec_r1 = matrix(c(rep(1,7),2, rep(1,7), 0), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#
# 3. Names of variables names and dimension of their Fod
info_r1 =matrix(c(1:3, 2,2,3), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
#
#  The relation between e1, e2 and a patient p
r1 <-bcaRel(tt = tt_r1, spec = spec_r1, infovar = info_r1, infovarnames = c("D1", "D2", "D"), relnb = 1)
# show the relation
# "Relation between Experts and patient defined in the product space D1 x D2 x D"
bcaPrint(r1)

## ---- fig.show='hold', fig_caption: yes, echo=FALSE, message=FALSE------------
# The network
if (requireNamespace("igraph", quietly = TRUE) ) {
library(igraph)
# Encode pieces of evidence and relations with an incidence matrix
rel1 <- 1*1:3 %in% r1$infovar[,1]
ev1 <- 1*1:3 %in% e1$infovar[,1]
ev2 <- 1*1:3 %in% e2$infovar[,1]

# information on variables
meddiag_vars1 <- c(r1$infovaluenames)
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
plot(meddiag_hg, vertex.label = V(meddiag_hg)$name, vertex.size=(3+6*V(meddiag_hg)$type)*6, sub="Hypergraph (Belief network) for Zadeh's Example")
}

## -----------------------------------------------------------------------------
# some settings to begin
# variables numbers: D1=1, D2=2, D=3
N <- 1:3
# Elimination order of the variables. The goal: Disease (variable 3)
elim_order <- c(1,2,3)
var_to_elim <- rownames(meddiag_hgm)[order(elim_order)]
#
# 1: first step
# "first variable to eliminate"
cat(var_to_elim[1]) # Diagnosis1 (1)
irel_to_elim<- meddiag_hgm["D1",]*1:ncol(meddiag_hgm)
rels_nb <- irel_to_elim[irel_to_elim>0]
#
# 1 Fusion of Expert1 and Relation 1 and eliminate variable Diagnosis1
# 1.1 Extension of e1 (Expert1)
Expert1_ext <- extmin(get(meddiag_data_names[1]), get(meddiag_data_names[3])) 
# "Evidence of Expert 1 extended to the product space D1 x D2 x D"
bcaPrint(Expert1_ext)
#
# 1.2 Combination of Expert1 and r1
r2 <- nzdsr(dsrwon(Expert1_ext, get(meddiag_data_names[3]), relnb = 1 + length(meddiag_data_names)))
# "Subsets resulting from the combination of Expert 1 extended and r1"
bcaPrint(r2)
#
# 1.3 Marginalization (eliminate variable D1)
rel_2 <- elim(r2, xnb = order(elim_order)[1])
# "subsets in the product space D2 x D (after elimination of D1"
bcaPrint(rel_2)

## ----echo=FALSE---------------------------------------------------------------
# update hg and relations names
# remove e1 and r1, add r2
R2 <- 1*1:3 %in% rel_2$infovar[,1]
meddiag_hgm1 <- cbind(meddiag_hgm[,-c(1,3)], R2)
colnames(meddiag_hgm1) <- c("e2", "r2")  
meddiag_data_names1 <- c(meddiag_data_names[-c(1,3)], "rel_2")
meddiag_var_names1 <- meddiag_var_names[-order(elim_order)[1]]
meddiag <- list(meddiag_hgm1, meddiag_var_names1, meddiag_data_names1)
meddiag_hg <- graph_from_incidence_matrix(incidence = meddiag_hgm1, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
# V(meddiag_hg)
# Show variables as circles, relations and evidence as rectangles
V(meddiag_hg)$shape <- c("circle", "crectangle")[V(meddiag_hg)$type+1]
V(meddiag_hg)$label.cex <- 0.6
V(meddiag_hg)$label.font <- 2
# render graph
plot(meddiag_hg, vertex.label = V(meddiag_hg)$name, vertex.size=(3+6*V(meddiag_hg)$type)*6, sub = "Reduced belief network after elimination of variable D1")

## -----------------------------------------------------------------------------
# 2. Second step
# Fusion of Expert2 and Relation 2 and eliminate diagnosis2
#
# 2.1 Extension of Expert2 (e2)
# here, use rel_2 as the relation of reference for extmin 
Expert2_ext <- extmin(e2, rel_2) 
temp1 <- as.data.frame(cbind(rownames(Expert2_ext$tt), Expert2_ext$spec))
colnames(temp1)[1] <- "Expert2_ext"
# "Evidence of Expert 2 extended to the product space D2 x D"
print(temp1)
#
# 2.2 combinaison pf Expert2 and rel_2
r3 <- nzdsr(dsrwon(Expert2_ext, rel_2, relnb = 3))
temp1 <- as.data.frame(cbind(rownames(r3$tt), r3$spec))
colnames(temp1)[1] <- "r3"
# "Subsets of the space D2 x D resulting from the combination of Expert 2 extended and r2"
print(temp1)
#
# 2.3 Final result: the belief function p attached to variable D)
p <- elim(r3, xnb = order(elim_order)[2])
#
# add singletons with 0 mass to show all singletons in the results
p_sing <- addTobca(p,  f = matrix(c(1,0,0,0,0,1), ncol=3))
# "The final result after elimination of variable D2"
tabresul(p_sing)

## -----------------------------------------------------------------------------
plautrans(p)

