# Exploring patterns of turnover & nestedness
# Tim Szewczyk
# Created 2015 March 05

# This script explores elevational patterns of turnover in ant communities. 
# Specifically, it includes:
# - Calculating the number of species in common between adjacent elevations
# - Pres/Abs-based similarity estimators for each site-pair in each study
# - Faunal congruity curves (Terborgh 1971, Longino & Colwell 2011)
#   - These don't work well for gradients with many sites. So I created matrix
#     plots as well

# Manual switches for species vs genus vs subfamily
# - change .xlsx for spRng.df
# - change Subfamily vs Genus vs Binomial in "Create PresAbs matrix" section


#######
## Load libraries, functions, data
#######

  library(ggplot2); theme_set(theme_bw()); 
  library(xlsx); library(plyr); library(vegan); library(betapart)
  source("R_scripts/FuncsGen.R")
  loadAll()


#########
## Create PresAbs matrix for each transect & estimate dissimilarity
#########

  pa.ls <- vector("list", nTrans); names(pa.ls) <- Transects
  sim.ls <- vector("list", nTrans); names(sim.ls) <- Transects
  sne.ls <- vector("list", nTrans); names(sne.ls) <- Transects
  sor.ls <- vector("list", nTrans); names(sor.ls) <- Transects
  el.ls <- vector("list", nTrans); names(el.ls) <- Transects
  pair.ls <- vector("list", nTrans); names(pair.ls) <- Transects

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
    
    # Calculate all pairwise beta diversity metrics
    sim.ls[[tr]] <- as.dist(beta.pair(tr.pa)[[1]], upper=TRUE, diag=TRUE)
    sne.ls[[tr]] <- as.dist(beta.pair(tr.pa)[[2]], upper=TRUE, diag=TRUE)
    sor.ls[[tr]] <- as.dist(beta.pair(tr.pa)[[3]], upper=TRUE, diag=TRUE)
    
    # Pull out adjacent sites
    pair.ls[[tr]] <- data.frame(El1=el.ls[[tr]][1:length(el.ls[[tr]])-1],
                                El2=el.ls[[tr]][2:length(el.ls[[tr]])],
                                sim=rep(NA, length(el.ls[[tr]])-1),
                                sne=rep(NA, length(el.ls[[tr]])-1),
                                sor=rep(NA, length(el.ls[[tr]])-1))
    for(r in 1:( nrow(pair.ls[[tr]])) ) {
      pair.ls[[tr]]$sim[r] <- as.matrix(sim.ls[[tr]])[r+1, r]
      pair.ls[[tr]]$sne[r] <- as.matrix(sne.ls[[tr]])[r+1, r]
      pair.ls[[tr]]$sor[r] <- as.matrix(sor.ls[[tr]])[r+1, r]
    }
    
    cat("Finished transect ", tr, " of ", nTrans, "\n")
  }
  
  
#########
## Create a dataframe for faunal congruency curves & adjacent sites
#########
  
  # All sites
  df.ls <- vector("list", nTrans); names(df.ls) <- Labels
  
  for(tr in 1:nTrans) {
    df <- expand.grid(el.ls[[tr]], el.ls[[tr]])
    names(df) <- c("El1", "El2")
    df$sim <- c(as.matrix(sim.ls[[tr]]))
    df$sne <- c(as.matrix(sne.ls[[tr]]))
    df$sor <- c(as.matrix(sor.ls[[tr]]))
    
    df.ls[[tr]] <- df
  }
  
  diss.df <- ldply(df.ls)
  names(diss.df)[1] <- "Label"
  diss.df$ymin <- rep(0, nrow(diss.df))

  # Adjacent sites  
  names(pair.ls) <- Labels
  pair.df <- ldply(pair.ls)
  names(pair.df)[1] <- "Label"
  pair.df$ymin <- rep(0, nrow(pair.df))


########
## Write csv's
########

  #write.csv(diss.df, file="Sheets/spp_Sorensen.csv")
  write.csv(pair.df, file="Sheets/spp_SorAdjSites.csv")


########
## Whole gradient calculations
########

  whole.df <- data.frame(Transect=Transects,
                         Label=Labels,
                         sp.sim=rep(NA, nTrans), 
                         sp.sne=rep(NA, nTrans),
                         sp.sor=rep(NA, nTrans),
                         gen.sim=rep(NA, nTrans), 
                         gen.sne=rep(NA, nTrans),
                         gen.sor=rep(NA, nTrans),
                         sf.sim=rep(NA, nTrans), 
                         sf.sne=rep(NA, nTrans),
                         sf.sor=rep(NA, nTrans))

  for(tr in 1:nTrans) {
    
    # subset dataframes to transect
    trans <- Transects[tr]
    varTr <- droplevels(subset(tvars.df, tvars.df$Transect==trans))
    els <- varTr$Elsamp[is.na(varTr$Elsamp)==FALSE]
    spTr <- droplevels(subset(spRng.df, spRng.df$Transect==trans))
    genTr <- droplevels(subset(genRng.df, genRng.df$Transect==trans))
    sfTr <- droplevels(subset(sfRng.df, sfRng.df$Transect==trans))
    
    # presence-absence matrices
    sp.pa <- matrix(nrow=length(els), ncol=nlevels(spTr$Binomial),
                    dimnames=list(els, levels(spTr$Binomial)))
    gen.pa <- matrix(nrow=length(els), ncol=nlevels(genTr$Genus),
                    dimnames=list(els, levels(genTr$Genus)))
    sf.pa <- matrix(nrow=length(els), ncol=nlevels(sfTr$Subfamily),
                    dimnames=list(els, levels(sfTr$Subfamily)))
    
    # fill species PA matrix
    for(e in 1:length(els)) {
      for(s in 1:nlevels(spTr$Binomial)) {
        r <- which(spTr$Binomial == levels(spTr$Binomial)[s])
        el <- els[e]
        sp.pa[e,s] <- ifelse((spTr$LowEl[r] <= el) & (spTr$HighEl[r] >= el),
                             1, 
                             0)
      }
    }
    sp.pa <- sp.pa[rowSums(sp.pa) > 0, ]

    # fill genus PA matrix
    for(e in 1:length(els)) {
      for(s in 1:nlevels(genTr$Genus)) {
        r <- which(genTr$Genus == levels(genTr$Genus)[s])
        el <- els[e]
        gen.pa[e,s] <- ifelse((genTr$LowEl[r] <= el) & (genTr$HighEl[r] >= el),
                             1, 
                             0)
      }
    }
    gen.pa <- gen.pa[rowSums(gen.pa) > 0, ]
    
    # fill subfamily PA matrix
    for(e in 1:length(els)) {
      for(s in 1:nlevels(sfTr$Subfamily)) {
        r <- which(sfTr$Subfamily == levels(sfTr$Subfamily)[s])
        el <- els[e]
        sf.pa[e,s] <- ifelse((sfTr$LowEl[r] <= el) & (sfTr$HighEl[r] >= el),
                             1, 
                             0)
      }
    }
    sf.pa <- sf.pa[rowSums(sf.pa) > 0, ]
    
    # gradient-wide dissimilarities
    whole.df$sp.sim[tr] <- beta.multi(sp.pa)$beta.SIM
    whole.df$sp.sne[tr] <- beta.multi(sp.pa)$beta.SNE
    whole.df$sp.sor[tr] <- beta.multi(sp.pa)$beta.SOR
    whole.df$gen.sim[tr] <- beta.multi(gen.pa)$beta.SIM
    whole.df$gen.sne[tr] <- beta.multi(gen.pa)$beta.SNE
    whole.df$gen.sor[tr] <- beta.multi(gen.pa)$beta.SOR
    whole.df$sf.sim[tr] <- beta.multi(sf.pa)$beta.SIM
    whole.df$sf.sne[tr] <- beta.multi(sf.pa)$beta.SNE
    whole.df$sf.sor[tr] <- beta.multi(sf.pa)$beta.SOR
  }



########
## Whole gradient Tuomisto (2010) version
########

  # Dbeta = (Stot)/mean(S.band)
  # STB = (Dbeta - 1)/(nSites - 1)

  STB.df <- data.frame(Transect=levels(over.df$Transect),
                       nSites=over.df$Sites,
                       sp.Dbeta=rep(NA, nrow(over.df)),
                       sp.STB=rep(NA, nrow(over.df)),
                       gen.Dbeta=rep(NA, nrow(over.df)),
                       gen.STB=rep(NA, nrow(over.df)),
                       sf.Dbeta=rep(NA, nrow(over.df)),
                       sf.STB=rep(NA, nrow(over.df)))

  for(tr in 1:nlevels(ivars.df$Transect)) {
    varTr <- subset(ivars.df, ivars.df$Transect==levels(over.df$Transect)[tr])
    STB.df$sp.Dbeta[tr] <- varTr$Stot[1]/mean(varTr$S, na.rm=T)
    STB.df$sp.STB[tr] <- (STB.df$sp.Dbeta[tr]-1)/(STB.df$nSites[tr]-1)
    STB.df$gen.Dbeta[tr] <- varTr$GenTot[1]/mean(varTr$Gen, na.rm=T)
    STB.df$gen.STB[tr] <- (STB.df$gen.Dbeta[tr]-1)/(STB.df$nSites[tr]-1)
    STB.df$sf.Dbeta[tr] <- varTr$SFTot[1]/mean(varTr$SF, na.rm=T)
    STB.df$sf.STB[tr] <- (STB.df$sf.Dbeta[tr]-1)/(STB.df$nSites[tr]-1)
  }


########
## Write csv
########

  write.csv(STB.df, file="STB.csv")
#  write.csv(whole.df, file="multiSor.csv")
