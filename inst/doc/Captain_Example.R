## ----setup, include=FALSE-----------------------------------------------------
#
# devtools::load_all(".") # only used in place of dst when testing with R-devel
library(dst) 
#
knitr::opts_knit$set(echo = TRUE, root.dir = "..")

## ----Relation ADS-------------------------------------------------------------
# library(dst)
load("data/ads.rda")
ads_tt<- ads[-1,-c(1,2)]
ads_tt  <- as.matrix(ads_tt)
ads_info = matrix(c(1,2,3,7,4,4), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )      
ads_spec = matrix(c(rep(1,16), 2,rep(1,16),0), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
ads_rel <- bcaRel(tt = ads_tt, spec = ads_spec, infovar = ads_info, varnames = c("Arrival", "Departure", "Sail"), relnb = 1)
bcaPrint(ads_rel)

## ----Relation DLFM------------------------------------------------------------
load("data/dlfm.rda")
dlfm_tt<- dlfm[-1,-c(1,2)]
dlfm_tt  <- as.matrix(dlfm_tt)
colnames(dlfm_tt) <- colnames(dlfm)[-c(1,2)]
dlfm_info = matrix(c(2,4,5,6,4,2,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )      
dlfm_spec = matrix(c(rep(1,8), 2,rep(1,8),0), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
dlfm_rel <- bcaRel(tt = dlfm_tt, spec = dlfm_spec, infovar = dlfm_info, varnames = c("Departure", "Loading", "Forecast", "Maintenance"), relnb = 2)
bcaPrint(dlfm_rel)

## ----Relation SWR-------------------------------------------------------------
load("data/swr.rda")
swr_tt<- swr[-1,-c(1,2)]
swr_tt  <- as.matrix(swr_tt)
swr_info = matrix(c(3,7,8,4,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )      
swr_spec = matrix(c(rep(1,4), 2,rep(0.9,4), 0.1), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
swr_rel <- bcaRel(tt = swr_tt, spec = swr_spec, infovar = swr_info, varnames = c("Sail", "Weather", "Repairs"), relnb = 3)
bcaPrint(swr_rel)

## ----Relation FW--------------------------------------------------------------
load("data/fw.rda")
fw_tt<- fw[-1,-c(1,2)]
fw_tt  <- as.matrix(fw_tt)
fw_info = matrix(c(5,7,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )      
fw_spec = matrix(c(rep(1,2), 2,rep(0.8,2), 0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
fw_rel <- bcaRel(tt = fw_tt, spec = fw_spec, infovar = fw_info, varnames = c("Forecast", "Weather"), relnb = 4)
bcaPrint(fw_rel)

## ----Relation MR1-------------------------------------------------------------
load("data/mrt.rda")
mrt_tt<- mrt[-1,-c(1,2)]
mrt_tt  <- as.matrix(mrt_tt)
colnames(mrt_tt) <- c("true", "false", "true", "false")
mrt_info = matrix(c(6,8,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )      
mrt_spec = matrix(c(rep(1,3), rep(2,3), 3, rep(0.1,3), rep(0.7,3), 0.2), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
mrt_rel <- bcaRel(tt = mrt_tt, spec = mrt_spec, infovar = mrt_info, varnames = c("Maintenance", "Repairs"), relnb = 5) 
bcaPrint(mrt_rel)

## ----Relation MR2-------------------------------------------------------------
load("data/mrf.rda")
mrf_tt<- mrf[-1,-c(1,2)]
mrf_tt  <- as.matrix(mrf_tt)
mrf_info = matrix(c(6,8,2,2), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )      
mrf_spec = matrix(c(rep(1,3), rep(2,3), 3, rep(0.2,3), rep(0.2,3), 0.6), ncol = 2, dimnames = list(NULL, c("specnb", "mass")))
mrf_rel <- bcaRel(tt = mrf_tt, spec = mrf_spec, infovar = mrf_info, varnames = c("Maintenance", "Repairs"), relnb = 6) 
bcaPrint(mrf_rel)

## ----Relation MR12------------------------------------------------------------
mr_rel <- nzdsr(dsrwon(mrt_rel, mrf_rel)) 
bcaPrint(mr_rel)

## ----evidence Loading---------------------------------------------------------
l_rel <- bca(f = matrix(c(1,0,0,1,1,1), ncol = 2, byrow = TRUE), m = c(0.3, 0.5, 0.2), cnames = c("true", "false"), varnb = 4, varnames = "Loading")
bcaPrint(l_rel)

## ----evidence Forecast--------------------------------------------------------
f_rel <- bca(f = matrix(c(1,0,0,1,1,1), ncol = 2, byrow = TRUE), m = c(0.2, 0.6, 0.2), cnames = c("foul", "fair"), varnb = 5, varnames = "Forecast")
bcaPrint(f_rel)

## ----evidence Maintenance-----------------------------------------------------
m_rel <- bca(f = matrix(c(1,0,0,1), ncol = 2, byrow = TRUE), m = c(0, 1), cnames = c("true", "false"), varnb = 6, varnames = "Maintenance")
bcaPrint(m_rel)

## ----fig.show='hold', fig_caption: yes----------------------------------------
# The network
if (requireNamespace("igraph", quietly = TRUE) ) {
library(igraph)
# Encode pieces of evidence and relations with an incidence matrix
R1 <- 1*1:8 %in% ads_rel$infovar[,1]
R2 <- 1*1:8 %in% dlfm_rel$infovar[,1]
R3 <- 1*1:8 %in% swr_rel$infovar[,1]
R4 <- 1*1:8 %in% fw_rel$infovar[,1]
R5 <- 1*1:8 %in% mr_rel$infovar[,1]
E1 <- 1*1:8 %in% l_rel$infovar[,1]
E2 <- 1*1:8 %in% f_rel$infovar[,1]
E3 <- 1*1:8 %in% m_rel$infovar[,1]

# information on variables
captain_vars1 <- c( ads_rel$valuenames,  dlfm_rel$valuenames[2:4],  swr_rel$valuenames[2:3])
captain_vars <- rbind( ads_rel$infovar,  dlfm_rel$infovar[2:4,],  swr_rel$infovar[2:3,])
captain_var_names <-names(captain_vars1)
rownames(captain_vars) <- captain_var_names
# infos on relations
captain_rel_names <- c("ads_rel", "dlfm_rel", "swr_rel", "fw_rel", "mr_rel", "l_rel", "f_rel", "m_rel")
# the incidence matrix
captain_hgm <- matrix(c(R1,R2,R3,R4,R5,E1,E2,E3), ncol=8, dimnames = list(c("Arrival", "Departure", "Sailing", "Loading", "Forecast", "Maintenance", "Weather", "Repairs"), c("R1", "R2", "R3", "R4","R5","E1","E2","E3")))
captain <- list(captain_hgm, captain_var_names, captain_rel_names)
#
## The graph structure of the problem
#
captain_hg <- graph_from_incidence_matrix(incidence = captain_hgm, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
V(captain_hg)
# Show variables as circles, relations and evidence as rectangles
V(captain_hg)$shape <- c("circle", "crectangle")[V(captain_hg)$type+1]
V(captain_hg)$label.cex <- 0.6
V(captain_hg)$label.font <- 2
# render graph
plot(captain_hg, vertex.label = V(captain_hg)$name, vertex.size=(3+6*V(captain_hg)$type)*6)
}

## ----Calculation--------------------------------------------------------------
# variables numbers
N <- 1:8
# Elimination order of variables. The goal: Arrival
elim_order <- c(8,7,6,1,5,4,3,2)
var_to_elim <- rownames(captain_hgm)[order(elim_order)]
#
# 1: first step
# first var to eliminate
var_to_elim[1] # Loading (4)
irel_to_elim<- captain_hgm["Loading",]*1:ncol(captain_hgm)
rels_nb <- irel_to_elim[irel_to_elim>0]
#
## To do: find which rel to extend (here, it is E1)
# extend R6 (Z7_T EN APL)
l_ext <- extmin(get(captain_rel_names[6]), get(captain_rel_names[2])) 
# combine E1, R2
# use length(captain_rel_names) to assign the next relation nb
rel_2_6 <- nzdsr(dsrwon(l_ext, get(captain_rel_names[2]), relnb = 1+length(captain_rel_names)) )
# eliminate the variable "Loading" (4)
rel_9 <- elim(rel_2_6, xnb = order(elim_order)[1])
#
# update hg and relations names
# remove R2 and E7, add R9
R9 <- 1*1:8 %in% rel_9$infovar[,1]
captain_hgm1 <- cbind(captain_hgm[,-c(2,6)], R9)
captain_rel_names1 <- c(captain_rel_names[-c(2,6)], "rel_9")
captain_var_names1 <- captain_var_names[-order(elim_order)[1]]
#
## second step eliminate var "Repairs" (8)
var_to_elim[2] 
order(elim_order)[2] # Repairs (8)
irel_to_elim<- captain_hgm1[var_to_elim[2],]*1:ncol(captain_hgm1)
rels_nb <- irel_to_elim[irel_to_elim>0]
# find variables numbers of each relation to obtain the space to construct
rels_names = captain_rel_names1[rels_nb]
yv1 = get(rels_names[1])$infovar
yv2 = get(rels_names[2])$infovar
yv=rbind(yv1,yv2)
yinfov = doubles(yv)
infovar <- yinfov[order(yinfov[,1]),]
#
# extract valuenames
infovalues <- captain_vars1[infovar[,1]]

# extend the two relations before combining them
# 1. making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
# 1: construct the tt matrix
init_tt= matrix(rep(1,10),nrow=1, 
dimnames =list(NULL, c("3", "2", "1", "0", 
"false", "true",  "fair","foul",  "false", "true")) )
# 2: mass values
init_spec <- matrix(c(1,1), ncol = 2, 
dimnames = list(NULL, c("specnb", "mass")))
# 3: info on variables
init_info <- matrix(c(3,6,7,8,4,2,2,2), ncol = 2,
dimnames = list(NULL, c("varnb", "size")) )
# 4: the relation
relRef <- bcaRel(tt = init_tt, spec = init_spec,
infovar = init_info, 
varnames = c("Sail", "Maintenance", "Weather", "Repairs"),
 relnb = 10)
# extend the relations
mr_ext <- extmin(get(captain_rel_names1[4]), relRef)
swr_ext <- extmin(get(captain_rel_names1[2]), relRef)
# 3: combine the two relations
# combine extended relations
rel_3_5 <- nzdsr(dsrwon(mr_ext,swr_ext, relnb = 10) ) 
#4 eliminate the variable "Maintenance" (8)
rel_10 <- elim(rel_3_5, xnb = order(elim_order)[2])
#
# update hg and relations names
# remove R3 and R5, add R10
R10 <- 1*1:8 %in% rel_10$infovar[,1]
captain_hgm2 <- cbind(captain_hgm1[,-rels_nb], R10)
captain_rel_names2 <- c(captain_rel_names1[-rels_nb], "rel_10")
#
## Third step eliminate var "Weather" (7)
var_to_elim[3] 
order(elim_order)[3] # Weather (7)
irel_to_elim<- captain_hgm2[var_to_elim[3],]*1:ncol(captain_hgm2)
rels_nb <- irel_to_elim[irel_to_elim>0]
# find variables numbers of each relation to obtain the space to construct
rels_names = captain_rel_names2[rels_nb]
yv1 = get(rels_names[1])$infovar
yv2 = get(rels_names[2])$infovar
yv=rbind(yv1,yv2)
yinfov = doubles(yv)
infovar <- yinfov[order(yinfov[,1]),]
#
# extract valuenames
infovalues <- captain_vars1[infovar[,1]]
#
# extend the two relations before combining them
# 1. making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
# 1: construct the tt matrix
init_tt= matrix(rep(1,10),nrow=1, 
dimnames =list(NULL, c("3", "2", "1", "0","foul", "fair",
"true", "false",  "foul", "fair")) )
# 2: mass values
init_spec <- matrix(c(1,1), ncol = 2, 
dimnames = list(NULL, c("specnb", "mass")))
# 3: info on variables
init_info <- matrix(as.vector(infovar), ncol = 2,
dimnames = list(NULL, c("varnb", "size")) )
# 4: the relation
#
relRef <- bcaRel(tt = init_tt, spec = init_spec,
infovar = init_info, varnames = names(infovalues), relnb = 11)
#
# extend the relations
fw_ext <- extmin(get(captain_rel_names2[rels_nb[1]]), relRef)
rel_10_ext <- extmin(get(captain_rel_names2[rels_nb[2]]), relRef)
# 3: combine the two relations
# combine extended relations
rel_4_10 <- nzdsr(dsrwon(fw_ext,rel_10_ext, relnb = 11) ) 
#
# 4 eliminate the variable "Weather" (7)
rel_11 <- elim(rel_4_10, xnb = order(elim_order)[3])
#
## Fourth step 
var_to_elim[4] 
order(elim_order)[4] # Maintenance (6)
#eliminate var "Maintenance" (6)
# update hg and relations names
# 
# remove rels_nb R4 and R10, add R11
R11 <- 1*1:8 %in% rel_11$infovar[,1]
captain_hgm3 <- cbind(captain_hgm2[,-rels_nb], R11)
captain_rel_names3 <- c(captain_rel_names2[-rels_nb], "rel_11")
#
irel_to_elim<- captain_hgm3[var_to_elim[4],]*1:ncol(captain_hgm3)
rels_nb <- irel_to_elim[irel_to_elim>0]
# find variables numbers of each relation to obtain the space to construct
rels_names = captain_rel_names3[rels_nb]
yv1 = get(rels_names[1])$infovar
yv2 = get(rels_names[2])$infovar
yv3 = get(rels_names[3])$infovar
yv=rbind(yv1,yv2, yv3)
yinfov = doubles(yv)
infovar <- yinfov[order(yinfov[,1]),]
#
# extract valuenames
## test
infovalues = captain_vars1[infovar[,1]]
#
# extend the relations before combining them
# 1. making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
# 1: construct the tt matrix
# 
init_tt <- matrix(rep(1,sum(infovar[,2])),nrow=1, dimnames = list(NULL, unlist(infovalues) ) )
# 2: mass values
init_spec <- matrix(c(1,1), ncol = 2, 
dimnames = list(NULL, c("specnb", "mass")))
# 3: info on variables
init_info <- matrix(as.vector(infovar), ncol = 2,
dimnames = list(NULL, c("varnb", "size")) )
# 4: the relation
relRef <- bcaRel(tt = init_tt, spec = init_spec,
infovar = init_info, varnames = names(infovalues), relnb = 12)
# extend the relations (3 relations)
m_ext <- extmin(get(captain_rel_names3[rels_nb[1]]), relRef)
rel_9_ext <- extmin(get(captain_rel_names3[rels_nb[2]]), relRef)
rel_11_ext <- extmin(get(captain_rel_names3[rels_nb[3]]), relRef)
#
# 3: combine the relations
# combine extended relations
rel_3_9 <- nzdsr(dsrwon(m_ext,rel_9_ext, relnb = 12) ) 
rel_3_9_11 <- nzdsr(dsrwon(rel_3_9,rel_11_ext, relnb = 12) ) 
#
# 4 eliminate the variable "Maintenance" (6)
rel_12 <- elim(rel_3_9_11, xnb = order(elim_order)[4])
#
## Fifth step 
var_to_elim[5] 
order(elim_order)[5] # Forecast (5)
#eliminate var "Forecast" (5)
# update hg and relations names
# rels_nb to remove
print(rels_nb) # 3, 4, 5
# add R12
R12 <- 1*1:8 %in% rel_12$infovar[,1]
# remove rels_nb E3 and R9, add R11, add R12
captain_hgm4 <- cbind(captain_hgm3[,-rels_nb], R12)
captain_rel_names4 <- c(captain_rel_names3[-rels_nb], "rel_12")
#
irel_to_elim<- captain_hgm4[var_to_elim[5],]*1:ncol(captain_hgm4)
rels_nb <- irel_to_elim[irel_to_elim>0]
# find variables numbers of each relation to obtain the space to construct
rels_names = captain_rel_names4[rels_nb]
yv1 = get(rels_names[1])$infovar
yv2 = get(rels_names[2])$infovar
yv=rbind(yv1,yv2)
yinfov = doubles(yv)
infovar <- yinfov[order(yinfov[,1]),]
#
# extract valuenames
infovalues <- captain_vars1[infovar[,1]]
#
# extend the relations before combining them
# 1. making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
# 1: construct the tt matrix
##
init_tt <- matrix(rep(1,sum(infovar[,2])),nrow=1, dimnames = list(NULL, unlist(infovalues) ) )
# 2: mass values
init_spec <- matrix(c(1,1), ncol = 2, 
dimnames = list(NULL, c("specnb", "mass")))
# 3: info on variables
init_info <- matrix(as.vector(infovar), ncol = 2,
dimnames = list(NULL, c("varnb", "size")) )
# 4: the relation
relRef <- bcaRel(tt = init_tt, spec = init_spec,
infovar = init_info, varnames = names(infovalues), relnb = 13)
# extend the relations 
f_ext <- extmin(get(captain_rel_names4[rels_nb[1]]), relRef)
#
# 3: combine the relations
# combine extended relations
rel_E2_12 <- nzdsr(dsrwon(f_ext,rel_12, relnb = 13) ) 
# 4 eliminate the variable "Forecast" (5)
rel_13 <- elim(rel_E2_12, xnb = order(elim_order)[5])
#
## sixth step 
var_to_elim[6] 
order(elim_order)[6] # Sailing (3)
#eliminate var "Sailing" (3)
#
# update hg and relations names
# rels_nb to remove
print(rels_nb) # 2,3
# add R13
R13 <- 1*1:8 %in% rel_13$infovar[,1]
# remove rels_nb E3 and R9, add R11, add R12
captain_hgm5 <- cbind(captain_hgm4[,-rels_nb], R13)
captain_rel_names5 <- c(captain_rel_names4[-rels_nb], "rel_13")
#
irel_to_elim<- captain_hgm5[var_to_elim[6],]*1:ncol(captain_hgm5)
rels_nb <- irel_to_elim[irel_to_elim>0]
# find variables numbers of each relation to obtain the space to construct
rels_names = captain_rel_names5[rels_nb]
yv1 = get(rels_names[1])$infovar
yv2 = get(rels_names[2])$infovar
yv=rbind(yv1,yv2)
yinfov = doubles(yv)
infovar <- yinfov[order(yinfov[,1]),]
#
# extract valuenames
infovalues <- captain_vars1[infovar[,1]]
#
# extend the relations before combining them
# 1. making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
# 1: construct the tt matrix
##
init_tt <- matrix(rep(1,sum(infovar[,2])),nrow=1, dimnames = list(NULL, unlist(infovalues) ) )
# 2: mass values
init_spec <- matrix(c(1,1), ncol = 2, 
dimnames = list(NULL, c("specnb", "mass")))
# 3: info on variables
init_info <- matrix(as.vector(infovar), ncol = 2,
dimnames = list(NULL, c("varnb", "size")) )
# 4: the relation
relRef <- bcaRel(tt = init_tt, spec = init_spec,
infovar = init_info, varnames = names(infovalues), relnb = 14)
# extend the relations 
captain_rel_names5[rels_nb[1]] # " ads_rel"
captain_rel_names5[rels_nb[2]] # rel_13
rel_13_ext <- extmin(get(captain_rel_names5[rels_nb[2]]), relRef)   
#
# 3: combine the relations
# combine extended relations
rel_1_13 <- nzdsr(dsrwon(ads_rel,rel_13_ext, relnb = 14, mcores = "no") ) 
# 4 eliminate the variable "SAiling" (3)
rel_14 <- elim(rel_1_13, xnb = order(elim_order)[6])
#
## Step 7 
var_to_elim[7] 
order(elim_order)[7] # Departure (2)
#eliminate var "Departure" (2)
#
# update hg and relations names
# rels_nb to remove
print(rels_nb) # 1,2
# add R14
R14 <- 1*1:8 %in% rel_14$infovar[,1]
# remove rels_nb E3 and R9, add R11, add R12
captain_hgm6 <- cbind(captain_hgm5[,-rels_nb], R14)
captain_rel_names6 <- c(captain_rel_names5[-rels_nb], "rel_14")
#
irel_to_elim<- captain_hgm6[var_to_elim[7],]*1:ncol(captain_hgm6)
rels_nb <- irel_to_elim[irel_to_elim>0]
# find variables numbers of each relation to obtain the space to construct
rels_names = captain_rel_names6[rels_nb]
yv1 = get(rels_names[1])$infovar
yv = yv1
if (length(rels_names) > 1 ) {
  yv2 = get(rels_names[2])$infovar
  yv=rbind(yv1,yv2)
} 
yinfov = doubles(yv)
infovar <- yinfov[order(yinfov[,1]),]
#
# extract valuenames
infovalues <- captain_vars1[infovar[,1]]
#
if (length(rels_names) > 1 ) {
# extend the relations before combining them
# 1. making an empty reference relation with mass(frame) = 1 and
# extending a bca to it.
# 1: construct the tt matrix
##
  init_tt <- matrix(rep(1,sum(infovar[,2])),nrow=1, dimnames = list(NULL, unlist(infovalues) ) )
# 2: mass values
  init_spec <- matrix(c(1,1), ncol = 2, 
dimnames = list(NULL, c("specnb", "mass")))
# 3: info on variables
  init_info <- matrix(as.vector(infovar), ncol = 2,
dimnames = list(NULL, c("varnb", "size")) )
# 4: the relation
  relRef <- bcaRel(tt = init_tt, spec = init_spec,
infovar = init_info, varnames = names(infovalues), relnb = 15)
# extend the relations 
  captain_rel_names6[rels_nb[1]] # " rel"
  rel_ext <- extmin(get(captain_rel_names6[rels_nb[1]]), relRef) ## no need to extend. make a check on this case in an algorithm.
  captain_rel_names6[rels_nb[2]] # rel_14
  rel_14_ext <- extmin(get(captain_rel_names6[rels_nb[2]]), relRef)   
#
# 3: combine the relations
# combine extended relations
rel_comb <- nzdsr(dsrwon(rel_ext,rel_14_ext, relnb = 15, mcores = "no") ) 
cat("Results")
}
# 4 eliminate the variable "Sailing" (3)
rel_15 <- elim(rel_14, xnb = order(elim_order)[7]) 
bcaPrint(rel_15)
belplau(rel_15)

