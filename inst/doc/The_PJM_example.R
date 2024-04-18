## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# devtools::load_all(".") # only used in place of dst when testing development version
library(dst)

## ----Pooling1, echo = FALSE, warning=FALSE------------------------------------
cat("Witness1, A janitor asleep","\n")
witness1 <- bca(tt = matrix(c(1,1,0,0,1,1,1,1,1), ncol=3, byrow=TRUE), m= c(0.5, 0.2, 0.3), cnames =c("Peter", "John", "Mary"), varnames = "AMurder", idvar = 1)
bcaPrint(witness1)
#
cat("Add all singletons in order to view results later","\n")
witness1_plus_singl <- addTobca(witness1, tt = diag(3))
bcaPrint(witness1_plus_singl)
#
cat("Witness2, An old lady with bad sighting saw a tall man","\n")
tall <- bca(tt = matrix(c(1,0,0,1,1,1), ncol=3, byrow=TRUE), m= c(0.6, 0.4), cnames =c("Peter", "John", "Mary"), varnames = "AMurder", idvar = 1)
bcaPrint(tall)

## ----Pooling2, echo = FALSE, warning=FALSE------------------------------------
w_and_tall <- dsrwon(witness1_plus_singl, tall)
norm_w_and_tall <- nzdsr(w_and_tall)
result <- tabresul(norm_w_and_tall)
cat("Combination of witnesses 1 and 2","\n")
round(result$mbp, digits = 2)
cat("Conflict between evidence","\n")
result$Conflict


## ----Pooling3, echo = FALSE, warning=FALSE------------------------------------
girlfriend <- bca(tt = matrix(c(0,1,1,1,1,1), ncol=3, byrow=TRUE), m= c(1, 0), cnames =c("Peter", "John", "Mary"), varnames = "AMurder", idvar = 1)
cat("Witness 3, the girlfriend clears Peter","\n")
bcaPrint(girlfriend)
girlfriend_w_and_tall <- dsrwon(w_and_tall, girlfriend)
norm_girlfriend_w_and_tall <- nzdsr(girlfriend_w_and_tall)
tabresul(norm_girlfriend_w_and_tall)
#
# # Test en normalisant pas-à-pas
# girlfriend_w_and_tall2 <- dsrwon(norm_w_and_tall, girlfriend)
# norm_girlfriend_w_and_tall2 <- nzdsr(girlfriend_w_and_tall2)
# End test

## ----network1, echo = FALSE, warning=FALSE------------------------------------
cat("witness 1 induced relation ","\n")
cat("The description matrix of the relation","\n")
witness1_tt <- matrix(c(1,0,0,1,0,1,
                        0,1,1,0,0,1,
                        0,1,1,0,0,1,
                        0,1,0,1,1,0,
                        1,1,1,1,1, 1), ncol=6, byrow=TRUE)
colnames(witness1_tt) <- rep(c("yes", "no"), 3 )
witness1_tt
witness1_spec <- matrix(c(1,1,2,2,3, rep(0.5, 2), rep(0.2, 2), 0.3), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
witness1_info <- matrix(c(1:3, rep(2,3)), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
witness1_rel <- bcaRel(tt = witness1_tt, spec = witness1_spec, infovar = witness1_info, varnames = c("Peter", "John", "Mary"), relnb = 1)
cat(" the testimony Peter or John (0.5), John or Mary (0.2) in the product space","\n")
bcaPrint(witness1_rel)
#
cat("Evidence for witness 2","\n")
witness2 <- bca(tt = matrix(c(1,0,1,1), ncol=2, byrow=TRUE), m= c(0.6, 0.4), cnames =c("yes", "no"), varnames = "Peter", idvar = 1)
cat("Peter (.6)","\n")
bcaPrint(witness2)
#
cat("witness 3 induced relation ","\n")
cat("The description matrix of the relation","\n")
witness3_tt <- matrix(c(0,1,1,0,
                        1,0,0,1,
                        1,1,1, 1), ncol=4, byrow=TRUE)
colnames(witness3_tt) <- rep(c("yes", "no"), 2 )
witness3_tt
witness3_spec <- matrix(c(1,1,2, 1, 1, 0), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
witness3_info <- matrix(c(2:3, rep(2,2)), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
witness3_rel <- bcaRel(tt = witness3_tt, spec = witness3_spec, infovar = witness3_info, varnames = c("John", "Mary"), relnb = 2)
cat("The testimony John or Mary (1) expressed in the product space","\n")
bcaPrint(witness3_rel)
#
cat("Defining a relation linking the three variables in PxJxM","\n")
# 1. The tt table
tt_PJM <- matrix(c(1,0,0,1,0,1,
                   0,1,1,0,0,1,
                   0,1,0,1,1,0,
                   rep(1,6)),
  nrow = 4, byrow = TRUE, 
  dimnames = list(NULL, rep(c("yes", "no"), 3 ) ) )
#  
# 2. The mass distribution
spec_PJM <-  matrix(c(rep(1,6),2, 0) , ncol = 2, byrow = TRUE,
dimnames = list(NULL, c("specnb", "mass")))
#   
# 3. Variables numbers and sizes
info_PJM <- matrix(c(1:3,2,2,2), ncol = 2, 
dimnames = list(NULL, c("varnb", "size")) )
PJM_rel <- bcaRel(tt = tt_PJM, spec = spec_PJM, infovar = info_PJM,
  varnames = c("Peter", "John", "Mary"), relnb = 3)
cat("Linking the variables","\n")
cat("Detective's assumption expressed in the product space","\n")
bcaPrint(PJM_rel)
#

## ----fig.show='hold', fig_caption: yes, echo=FALSE, message=FALSE-------------
# The network
if (requireNamespace("igraph", quietly = TRUE) ) {
library(igraph)
cat("Encode pieces of evidence and relations with an incidence matrix","\n")
N <- 3 # 3 variables
r_W1 <- 1*1:N %in% witness1_rel$infovar[,1]
ev_W2 <- 1*1:N %in% witness2$infovar[,1]
r_W3 <- 1*1:N %in% witness3_rel$infovar[,1]
r_Detective <- 1*1:N %in% PJM_rel$infovar[,1]
#
# the incidence matrix
PJM_hgm <- matrix(c(r_W1, ev_W2,r_W3 ,r_Detective), ncol=4, dimnames = list(c("Peter", "John", "Mary"), c("r_W1", "ev_W2", "r_W3", "r_Detective")))
cat("The incidence matrix of the Hypergraph","\n")
print(PJM_hgm)
#
cat("information on variables necessary for the Peeling algorithm","\n")
PJM_vars1 <- c(PJM_rel$valuenames)
PJM_vars <- rbind(PJM_rel$infovar)
PJM_var_names <-names(PJM_vars1)
rownames(PJM_vars) <- PJM_var_names
#
# infos on relations
PJM_data_names <- c("witness1_rel", "witness2", "witness3_rel", "PJM_rel")
PJM <- list(PJM_hgm, PJM_var_names, PJM_data_names)
print(PJM)
#
## The graph structure of the problem
#
PJM_hg <- graph_from_biadjacency_matrix(incidence = PJM_hgm, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
V(PJM_hg)
# Show variables as circles, relations and evidence as rectangles
V(PJM_hg)$shape <- c("circle", "crectangle")[V(PJM_hg)$type+1]
V(PJM_hg)$label.cex <- 0.6
V(PJM_hg)$label.font <- 2
# render graph
plot(PJM_hg, vertex.label = V(PJM_hg)$name, vertex.size=(3+6*V(PJM_hg)$type)*6, sub="Belief network for Peter,John, Mary's Example")
##
#
cat("\n")
}

## ----Combining the evidence, echo = FALSE, warning=FALSE----------------------
cat("Combining in the product space P x J x M","\n")
cat("We use witness1_rel relation as a reference to extend the others.", "\n")
#
# cat("witness 1 induced relation ","\n")
# cat("The description matrix of the relation","\n")
# witness1_tt <- matrix(c(1,0,0,1,0,1,
#                         0,1,1,0,0,1,
#                         0,1,1,0,0,1,
#                         0,1,0,1,1,0,
#                         1,1,1,1,1, 1), ncol=6, byrow=TRUE)
# colnames(witness1_tt) <- rep(c("yes", "no"), 3 )
# witness1_tt
# witness1_spec <- matrix(c(1,1,2,2,3, rep(0.5, 2), rep(0.2, 2), 0.3), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
# witness1_info <- matrix(c(1:3, rep(2,3)), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
# witness1_rel <- bcaRel(tt = witness1_tt, spec = witness1_spec, infovar = witness1_info, varnames = c("Peter", "John", "Mary"), relnb = 1)
# cat(" the testimony Peter or John (0.5), John or Mary (0.2) in the product space","\n")
# bcaPrint(witness1_rel)
# #
# cat("Evidence for witness 2","\n")
# witness2 <- bca(tt = matrix(c(1,0,1,1), ncol=2, byrow=TRUE), m= c(0.6, 0.4), cnames =c("yes", "no"), varnames = "Peter", idvar = 1)
# cat("peter (.6)","\n")
# bcaPrint(witness2)
# #
# cat("witness 3 induced relation ","\n")
# cat("The description matrix of the relation","\n")
# witness3_tt <- matrix(c(0,1,1,0,
#                         1,0,0,1,
#                         1,1,1, 1), ncol=4, byrow=TRUE)
# colnames(witness3_tt) <- rep(c("yes", "no"), 2 )
# witness3_tt
# witness3_spec <- matrix(c(1,1,2, 1, 1, 0), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
# witness3_info <- matrix(c(2:3, rep(2,2)), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
# witness3_rel <- bcaRel(tt = witness3_tt, spec = witness3_spec, infovar = witness3_info, varnames = c("John", "Mary"), relnb = 2)
# cat("The testimony John or Mary (1) expressed in the product space","\n")
# bcaPrint(witness3_rel)
#
cat("Extend Witness2 on PJM","\n")
w2_xtnd <- extmin(witness2, relRef = witness1_rel)
bcaPrint(w2_xtnd)
cat("Extend Witness 3 on PJM","\n")
w3_xtnd <- extmin(witness3_rel, relRef = witness1_rel)
bcaPrint(w3_xtnd)
#
cat("Combining the three relations on PJM","\n")
cat("Combining w2_xtnd with witness1_rel","\n")
rel_comb <- dsrwon(w2_xtnd,witness1_rel)
cat("rel_comb", "\n")
bcaPrint(rel_comb)
#
# rel_comb2_norm <- nzdsr(rel_comb2)
# bcaPrint(rel_comb2_norm)
#
cat("Combining rel_comb with w3_xtnd","\n")
rel_comb2 <- dsrwon(rel_comb,w3_xtnd )
cat("rel_comb2", "\n")
bcaPrint(rel_comb2)
#
cat("Redo the combinations with a modifiied representation of witness 3","\n")
cat("Define w3 par ¬Peter (1), instead of jojn or Mary","\n")
#
witness3b <- bca(tt = matrix(c(0,1,1,1), ncol=2, byrow=TRUE), m= c(1, 0), cnames =c("yes", "no"), varnames = "Peter", idvar = 1)
bcaPrint(witness3b)
#
cat("Extend Witness 3 on PJM","\n")
w3b_xtnd <- extmin(witness3b, relRef = witness1_rel)
bcaPrint(w3b_xtnd)
#
cat("combining with w3b_xtnd.  ")
cat("Note the differennce with the preceding result","\n")
rel_comb2b <- dsrwon(rel_comb,w3b_xtnd )
bcaPrint(rel_comb2b)
# cat("Normalizing...","\n")
# rel_comb2b_norm <- nzdsr(rel_comb2b)
# bcaPrint(rel_comb2b_norm)
# rel_comb2b_norm$con
#
cat("Combining rel_comb2 rith PJM_rel","\n")
rel_comb3 <- dsrwon(rel_comb2,PJM_rel )
bcaPrint(rel_comb3)
#
cat("Combining rel_comb2b rith PJM_rel","\n")
rel_comb3b <- dsrwon(rel_comb2b,PJM_rel )
bcaPrint(rel_comb3b)
cat("Now we obtain the same result","\n")
rel_comb3_norm <- nzdsr(rel_comb3)
bcaPrint(rel_comb3_norm)
rel_comb3_norm$con
#
rel_comb3b_norm <- nzdsr(rel_comb3b)
bcaPrint(rel_comb3b_norm)
rel_comb3b_norm$con
#
#
cat("Compute marginals","\n")
cat("Peter","\n")
m_PJ <- elim(rel_comb3b_norm, xnb = 3)
m_P <- elim(m_PJ, xnb = 2)
bcaPrint(m_P)
cat("John or Mary","\n")
m_JM <- elim(rel_comb3b_norm, xnb = 1)
bcaPrint(m_JM)
cat("John","\n")
m_J <- elim(m_JM, xnb = 3)
bcaPrint(m_J)
cat("Mary","\n")
m_M <- elim(m_JM, xnb = 2)
bcaPrint(m_M)
#
# cat("Combining rel_comb2 rith PJM_rel","\n")
# rel_comb3 <- dsrwon(rel_comb2,PJM_rel )
# bcaPrint(rel_comb3)
# rel_comb3_norm <- nzdsr(rel_comb3)
# bcaPrint(rel_comb3_norm)
# rel_comb3_norm$con
# #
# cat("Combining rel_comb2b rith PJM_rel","\n")
# rel_comb3b <- dsrwon(rel_comb2b,PJM_rel )
# bcaPrint(rel_comb3b)
# rel_comb3b_norm <- nzdsr(rel_comb3b)
# bcaPrint(rel_comb3b_norm)
# rel_comb3b_norm$con
# #
# cat("Compare rel_comb3 and rel_comb3b")
# all.equal(rel_comb3_norm,rel_comb3b_norm)

## ----peeling, echo = FALSE, warning=FALSE-------------------------------------
# cat("\  ")
bel_culprit <- peeling(vars_def = PJM_vars1, hgm = PJM_hgm, hg_rel_names = PJM_data_names, elim_order = c(1, 2, 3), verbose = TRUE ) 
cat("Result for the variable of interest","\n")
zz <- tabresul(bel_culprit)
format(as.data.frame(zz$mbp), digits=2)
cat("Contradiction Indice: ", bel_culprit$con)
#
# # add singletons with 0 mass to show all singletons in the results
# p_sing <- addTobca(x = bel_culprit,  tt = matrix(c(1,0,0,0,1,0,0,0,1), ncol=3))
# cat("The final result after elimination of variables","\n")
# cat("\n  ")
# zz <- tabresul(p_sing)
# format(as.data.frame(zz$mbp), digits=2)
# cat("Contradiction Indice: ", bel_culprit$con)

