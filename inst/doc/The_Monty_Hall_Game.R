## ----setup, include = FALSE---------------------------------------------------
#
# devtools::load_all(".") # only used in place of dst when testing with R-devel.
# attach package dst
library(dst)
#
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# 1. define the tt matrix MHABC_tt, which encodes the subset S
# 
MHABC_tt <- matrix(c(1,0,0,0,1,0,0,0,1,
                     1,0,0,0,0,1,0,1,0,
                     0,1,0,1,0,0,0,0,1,
                     0,1,0,0,0,1,1,0,0,
                     0,0,1,1,0,0,0,1,0,
                     0,0,1,0,1,0,1,0,0), ncol=9, byrow=TRUE)
colnames(MHABC_tt) <- rep(c("car", "goat1", "goat2"), 3)
#
# 2. define the spec matrix. 
# Here we have one subset of six elements
# 
MHABC_spec = matrix(rep(1,12), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
# 
# 3. define the info matrix. 
# for each variable, we attribute a number and give the size of the frame
# 
MHABC_info =matrix(c(1:3, rep(3,3)), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
# 
# 4. call of the function with the name of the variables and the numbering of the relation
# 
MHABC_rel <-  bcaRel(tt = MHABC_tt, spec = MHABC_spec, infovar = MHABC_info, infovarnames = c("MHA", "MHB", "MHC"), relnb = 1)
# 
# Relation vetween the three doors A, B and C
bcaPrint(MHABC_rel)
# Note that row labels can become pretty long. If it is the case, the prmatrix function can be used to view results, for example:
#
# prmatrix(t(MHABC_rel$tt), collab = "")
#
# Another way to check the tt matrix is:
#
# which(MHABC_rel$tt[1,] == TRUE)

## -----------------------------------------------------------------------------
# Evidence related to choice of door A
MHA_E <-  bca(f= diag(1,3,3), m= rep(1/3, 3), cnames =c("car", "goat1", "goat2"), infovarnames = "MHA", varnb = 1)
# Evidence of the contestant (function MHA_E attached to variable A)
bcaPrint(MHA_E)

## -----------------------------------------------------------------------------
# Evidence for door B
MHB_E <- bca(f= matrix(c(0,1,1), ncol=3, byrow = TRUE), m=1, cnames =c("car", "goat1", "goat2"), infovarnames = "MHB" , varnb=2)
# Evidence added by the Host (function MHB_E attached to variable B)
bcaPrint(MHB_E)

## ---- fig.show='hold', fig_caption: yes, echo=FALSE, message=FALSE------------
# The network
if (requireNamespace("igraph", quietly = TRUE) ) {
library(igraph)
# Encode pieces of evidence and relations with an incidence matrix
Monty_hgm <- matrix(c(1,1,1,1,0,0,0,1,0), ncol=3, dimnames = list(c("A", "B", "C"), c("r_ABC", "ev_A", "ev_B")))
# The graph structure
Monty_hg <- graph_from_incidence_matrix(incidence = Monty_hgm, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
V(Monty_hg)
# Show variables as circles, relations and evidence as rectangles
V(Monty_hg)$shape <- c("circle", "crectangle")[V(Monty_hg)$type+1]
V(Monty_hg)$label.cex <- 0.6
V(Monty_hg)$label.font <- 2
# render graph
plot(Monty_hg, vertex.label = V(Monty_hg)$name, vertex.size=(4+4*V(Monty_hg)$type)*8)
}

## -----------------------------------------------------------------------------
# 1. Extend MHA to the product space A x B x C
MHA_ext <- extmin(MHA_E, MHABC_rel )
"Evidence of Contestant extended to the product space A x B x C"
bcaPrint(MHA_ext)
#
# 2. Combine MHA_ext and MHABC_rel
MHA_ABC_comb <- dsrwon(MHA_ext,MHABC_rel)
# since  the measure of contradiction is 0, no need to normalize
MHA_ABC_comb$con
# "Subsets resulting from the combination of Expert 1 extended and r1"
bcaPrint(MHA_ABC_comb)
#
# 3. Eliminate variable A
MHBC <- elim(MHA_ABC_comb, xnb = 1)
bcaPrint(MHBC)

## ----echo=FALSE---------------------------------------------------------------
# {r, fig.show='hold', fig_caption: yes, echo=FALSE, message=FALSE}
Monty2_hgm <- matrix(c(1,1,1,0), ncol=2, dimnames = list(c("B", "C"), c("r_BC", "ev_B")))
Monty2_hg <- graph_from_incidence_matrix(incidence = Monty2_hgm, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
V(Monty2_hg)
# Variables as circles, relations and evidence as rectangles
V(Monty2_hg)$shape <- c("circle","crectangle")[V(Monty2_hg)$type+1]
V(Monty2_hg)$label.cex <- 0.6
V(Monty2_hg)$label.font <- 2
# render graph
# plot(Monty_hg, vertex.size=40)
plot(Monty2_hg, vertex.label = V(Monty2_hg)$name, vertex.size=(4+4*V(Monty2_hg)$type)*8)

## -----------------------------------------------------------------------------
# 1. Extend MHB_E to the space B x C
MHB_ext <- extmin(MHB_E, MHBC )
# Evidence of Host extended to the product space B x C"
bcaPrint(MHB_ext)
#
# 2. combination of MHB_ext and MHBC
MHB_BC_comb <- dsrwon(MHB_ext, MHBC)
# "Subsets of the space B x C resulting from the combination of Host extended and MHBC"
bcaPrint(MHB_BC_comb)
# MHA_BC_comb$con = 0, no need to normalize)
MHB_BC_comb$con
#
# 3. Eliminate variable B
MHC <- elim(MHB_BC_comb, xnb = 2)
# Final result: the belief function MHC attached to variable C
belplau(MHC)

## -----------------------------------------------------------------------------
MHC_plus_singl <- addTobca(MHC, f=matrix(c(0,1,0,0,0,1), ncol = 3, byrow = TRUE))
tabresul(MHC_plus_singl)

