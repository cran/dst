## ----setup, include=FALSE-----------------------------------------------------
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
l_rel <- bca(tt = matrix(c(1,0,0,1,1,1), ncol = 2, byrow = TRUE), m = c(0.3, 0.5, 0.2), cnames = c("true", "false"), idvar = 4, varnames = "Loading")
bcaPrint(l_rel)

## ----evidence Forecast--------------------------------------------------------
f_rel <- bca(tt = matrix(c(1,0,0,1,1,1), ncol = 2, byrow = TRUE), m = c(0.2, 0.6, 0.2), cnames = c("foul", "fair"), idvar = 5, varnames = "Forecast")
bcaPrint(f_rel)

## ----evidence Maintenance-----------------------------------------------------
m_rel <- bca(tt = matrix(c(1,0,0,1), ncol = 2, byrow = TRUE), m = c(0, 1), cnames = c("true", "false"), idvar = 6, varnames = "Maintenance")
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
captain_hg <- graph_from_biadjacency_matrix(incidence = captain_hgm, directed = FALSE, multiple = FALSE, weighted = NULL,add.names = NULL)
V(captain_hg)
# Show variables as circles, relations and evidence as rectangles
V(captain_hg)$shape <- c("circle", "crectangle")[V(captain_hg)$type+1]
V(captain_hg)$label.cex <- 0.6
V(captain_hg)$label.font <- 2
# render graph
plot(captain_hg, vertex.label = V(captain_hg)$name, vertex.size=(3+6*V(captain_hg)$type)*6)
}

## ----Peeling, echo = FALSE, warning=FALSE-------------------------------------
A <- peeling(vars_def = captain_vars1, hgm = captain_hgm, hg_rel_names = captain_rel_names, elim_order = c(8,4,7,2,6,5,3,1), verbose = TRUE ) 
bcaPrint(A)
round(belplau(A), digits = 2)


