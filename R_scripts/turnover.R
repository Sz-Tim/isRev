# Exploring patterns of turnover & similarity
# Tim Szewczyk
# Created 2015 March 05

# This script explores elevational patterns of turnover in ant communities. 
# Specifically, it includes:
# - Calculating the number of species in common between adjacent elevations
# - Pres/Abs-based similarity estimators for each site-pair in each study
# - Faunal congruity curves (Terborgh 1971, Longino & Colwell 2011)

# Manual switches for species vs genus vs subfamily
# - change .xlsx for spRng.df
# - change Subfamily vs Genus vs Binomial in "Create PresAbs matrix" section


#######
## Load libraries, functions, data
#######

  library(ggplot2); theme_set(theme_bw()); 
  library(xlsx); library(plyr); library(vegan); library(betapart)
  source("R_scripts/FuncsGen.R")
  spRng.df <- read.xlsx("Sheets/ranges_spp.xlsx", 1)  # Species ranges
  over.df <- read.xlsx("Sheets/datasetOverview.xlsx", 1)  # Dataset summaries
  ivars.df <- read.xlsx("Sheets/intVars.xlsx", 1)  # Elev's sampled, env var's
  Transects <- levels(spRng.df$Transect)  # Transects w/species range data
  nTrans <- nlevels(spRng.df$Transect)


#########
## Create PresAbs matrix for each transect & estimate dissimilarity
#########

  pa.ls <- vector("list", nTrans); names(pa.ls) <- Transects
  sim.ls <- vector("list", nTrans); names(sim.ls) <- Transects
  sne.ls <- vector("list", nTrans); names(sne.ls) <- Transects
  sor.ls <- vector("list", nTrans); names(sor.ls) <- Transects
  el.ls <- vector("list", nTrans); names(el.ls) <- Transects

  for(tr in 1:nTrans) {
    trans <- Transects[tr]
    varTr <- droplevels(subset(ivars.df, ivars.df$Transect==trans))
    els <- varTr$Elsamp[is.na(varTr$Elsamp)==FALSE]
    sppTr <- droplevels(subset(spRng.df, spRng.df$Transect==trans))
    
    tr.pa <- matrix(nrow=length(els), ncol=nlevels(sppTr$Binomial),
                    dimnames=list(els, levels(sppTr$Binomial)))
    for(e in 1:length(els)) {
      for(s in 1:nlevels(sppTr$Binomial)) {
        r <- which(sppTr$Binomial == levels(sppTr$Binomial)[s])
        el <- els[e]
        tr.pa[e,s] <- ifelse((sppTr$LowEl[r] <= el) & (sppTr$HighEl[r] >= el),
                               1, 
                               0)
      }
    }
    el.ls[[tr]] <- els[rowSums(tr.pa) > 0]
    tr.pa <- tr.pa[rowSums(tr.pa) > 0, ]
    pa.ls[[tr]] <- tr.pa
    sim.ls[[tr]] <- as.dist(beta.pair(tr.pa)[[1]], upper=TRUE, diag=TRUE)
    sne.ls[[tr]] <- as.dist(beta.pair(tr.pa)[[2]], upper=TRUE, diag=TRUE)
    sor.ls[[tr]] <- as.dist(beta.pair(tr.pa)[[3]], upper=TRUE, diag=TRUE)
    cat("Finished transect ", tr, " of ", nTrans, "\n")
  }


#########
## Create a dataframe for faunal congruency curves
#########
  
  df.ls <- vector("list", nTrans); names(df.ls) <- Transects

  for(tr in 1:nTrans) {
    df <- expand.grid(el.ls[[tr]], el.ls[[tr]])
    names(df) <- c("El1", "El2")
    df$sim <- c(as.matrix(sim.ls[[tr]]))
    df$sne <- c(as.matrix(sne.ls[[tr]]))
    df$sor <- c(as.matrix(sor.ls[[tr]]))
    
    df.ls[[tr]] <- df
  }

  diss.df <- ldply(df.ls)
  names(diss.df)[1] <- "Transect"
  diss.df$ymin <- rep(0, nrow(diss.df))
  write.csv(diss.df, file="Sheets/spp_Sorensen.csv")

ggplot(diss.df, aes(x=El2, group=El1, y=1-sor)) + 
  geom_line() + facet_wrap(~Transect) 
ggplot(diss.df, aes(x=El2, group=El1, ymax=1-sor, ymin=ymin)) + 
  geom_ribbon(colour=NA, alpha=0.25) + facet_wrap(~Transect) 
ggplot(diss.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Transect) +
  geom_point(size=2.5, shape=15) + 
  scale_colour_gradient(name="sim", low="white", high="black")
ggplot(diss.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Transect) +
  geom_point(size=2.5, shape=15) + 
  scale_colour_gradient(name="sne", low="white", high="black")
ggplot(diss.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Transect) +
  geom_point(size=2.5, shape=15) + 
  scale_colour_gradient(name="sor", low="white", high="black")



