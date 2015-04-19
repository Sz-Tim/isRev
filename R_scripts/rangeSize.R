# Range sizes across elevations
# Tim Szewczyk
# Created 2015 April 13

# This script estimates the change in mean elevational range size across an
# elevational gradient with three methods (McCain & Bracy Knight 2012):
# - Steven's: mean(el.range of all species at that el)
#   - results added to over.df: steven.b, steven.p, steven.r
# - Midpoint: mean(el.range of all species with range midpoint at that el)
#   - results added to over.df: mp.b, mp.p, mp.r
# - Quartile: mean(el.range of all species at that el w/ranges <25% gradient)
#   - results added to over.df: quart.b, quart.p, quart.r


#######
## Load libraries, functions, data
#######

  library(ggplot2); theme_set(theme_bw()); library(lme4)
  library(xlsx); library(plyr); library(vegan); library(betapart)
  source("R_scripts/FuncsGen.R")
  loadAll()


###########
## Steven's method
###########

  stev.df <- data.frame(Label=Labels,
                        Transect=Transects,
                        b=rep(NA, nTrans),
                        p=rep(NA, nTrans),
                        r=rep(NA, nTrans))

  for(tr in 1:nTrans) {
    transVar <- subset(tvars.df, tvars.df$Transect==stev.df$Transect[tr])
    mod <- lm(AvgOfRngObs ~ Elsamp, data=transVar)
    stev.df$b[tr] <- coef(mod)[2]
    stev.df$p[tr] <- summary(mod)$coefficients[2,4]
    stev.df$r[tr] <- summary(mod)$r.squared
  }
  stev.df


###########
## Midpoint method
###########

  mp.df <- data.frame(Label=Labels,
                      Transect=Transects,
                      b=rep(NA, nTrans),
                      p=rep(NA, nTrans),
                      r=rep(NA, nTrans))
  spRng.df$Rng <- spRng.df$HighEl - spRng.df$LowEl
  spRng.df$mp <- spRng.df$LowEl + spRng.df$Rng/2
  
  for(tr in 1:nTrans) {
    transVar <- subset(tvars.df, tvars.df$Transect==mp.df$Transect[tr])
    transSpp <- subset(spRng.df, spRng.df$Transect==mp.df$Transect[tr])
    els <- unique(transVar$Elband)
    avg.df <- data.frame(els=els,
                         rng=rep(NA, length(els)))
    
    for(e in 1:length(els)) {
      avg.df$rng[e] <- mean(transSpp$Rng[transSpp$mp < (els[e] + 100) &
                                           transSpp$mp >= els[e]])
    }
    
    mod <- lm(rng ~ els, data=avg.df)
    mp.df$b[tr] <- coef(mod)[2]
    mp.df$p[tr] <- summary(mod)$coefficients[2,4]
    mp.df$r[tr] <- summary(mod)$r.squared
  }
  mp.df


###########
## Quartile method
###########

  q.df <- data.frame(Label=Labels,
                     Transect=Transects,
                     b=rep(NA, nTrans),
                     p=rep(NA, nTrans),
                     r=rep(NA, nTrans))
  spRng.df$Rng <- spRng.df$HighEl - spRng.df$LowEl

  for(tr in 1:nTrans) {
    transVar <- subset(tvars.df, tvars.df$Transect==q.df$Transect[tr])
    transSpp <- subset(spRng.df, spRng.df$Transect==q.df$Transect[tr])
    els <- unique(transVar$Elband)
    avg.df <- data.frame(els=els,
                         rng=rep(NA, length(els)))
    
    for(e in 1:length(els)) {
      avg.df$rng[e] <- mean(transSpp$Rng[transSpp$LowEl < (els[e] + 100) &
                                        transSpp$HighEl >= els[e] &
                                        transSpp$Rng <= 0.25*
                                        (transVar$MtnPeak-transVar$MtnBase)[1]])
    }
    
    mod <- lm(rng ~ els, data=avg.df)
    q.df$b[tr] <- coef(mod)[2]
    q.df$p[tr] <- summary(mod)$coefficients[2,4]
    q.df$r[tr] <- summary(mod)$r.squared
  }
  q.df


#########
## Median & mean range size
#########

  rng.df <- data.frame(Label=Labels,
                       Transect=Transects,
                       sp.mnRng=rep(NA, nTrans),
                       gen.mnRng=rep(NA, nTrans),
                       sf.mnRng=rep(NA, nTrans),
                       sp.medRng=rep(NA, nTrans),
                       gen.medRng=rep(NA, nTrans),
                       sf.medRng=rep(NA, nTrans))

  for(tr in 1:nTrans) {
    transSpp <- subset(spRng.df, spRng.df$Transect==rng.df$Transect[tr])
    transGen <- subset(genRng.df, genRng.df$Transect==rng.df$Transect[tr])    
    transSf <- subset(sfRng.df, sfRng.df$Transect==rng.df$Transect[tr])
  
    rng.df$sp.mnRng[tr] <- mean(transSpp$HighEl - transSpp$LowEl)
    rng.df$gen.mnRng[tr] <- mean(transGen$HighEl - transGen$LowEl)
    rng.df$sf.mnRng[tr] <- mean(transSf$HighEl - transSf$LowEl)
    rng.df$sp.medRng[tr] <- median(transSpp$HighEl - transSpp$LowEl)
    rng.df$gen.medRng[tr] <- median(transGen$HighEl - transGen$LowEl)
    rng.df$sf.medRng[tr] <- median(transSf$HighEl - transSf$LowEl)
  }


########
## Range truncation
########

  trunc.pts <- data.frame(Transect=over.df$Transect,
                          tr1600=rep(NA, nrow(over.df)),
                          tr1800=rep(NA, nrow(over.df)),
                          tr2000=rep(NA, nrow(over.df)))
  
  # mountain truncation points
  trunc.pts$tr1600 <- ifelse((over.df$MtnPeak-over.df$MtnBase >= 1600) &
                               (!is.na(over.df$sp.sne)),
                             over.df$MtnBase + 1600,
                             NA)
  trunc.pts$tr1800 <- ifelse((over.df$MtnPeak-over.df$MtnBase >= 1800) &
                               (!is.na(over.df$sp.sne)),
                             over.df$MtnBase + 1800,
                             NA)
  trunc.pts$tr2000 <- ifelse((over.df$MtnPeak-over.df$MtnBase >= 2000) &
                               (!is.na(over.df$sp.sne)),
                             over.df$MtnBase + 2000,
                             NA)
  trunc.pts <- droplevels(subset(trunc.pts, trunc.pts$Transect %in% Transects))

  # estimate mean & median ranges
  rng.df <- data.frame(Label=Labels,
                       Transect=Transects,
                       sp.mnRng.1600=rep(NA, nTrans),
                       gen.mnRng.1600=rep(NA, nTrans),
                       sf.mnRng.1600=rep(NA, nTrans),
                       sp.medRng.1600=rep(NA, nTrans),
                       gen.medRng.1600=rep(NA, nTrans),
                       sf.medRng.1600=rep(NA, nTrans),
                       sp.mnRng.1800=rep(NA, nTrans),
                       gen.mnRng.1800=rep(NA, nTrans),
                       sf.mnRng.1800=rep(NA, nTrans),
                       sp.medRng.1800=rep(NA, nTrans),
                       gen.medRng.1800=rep(NA, nTrans),
                       sf.medRng.1800=rep(NA, nTrans),
                       sp.mnRng.2000=rep(NA, nTrans),
                       gen.mnRng.2000=rep(NA, nTrans),
                       sf.mnRng.2000=rep(NA, nTrans),
                       sp.medRng.2000=rep(NA, nTrans),
                       gen.medRng.2000=rep(NA, nTrans),
                       sf.medRng.2000=rep(NA, nTrans))

  for(tr in 1:nTrans) {
    transSpp <- subset(spRng.df, spRng.df$Transect==rng.df$Transect[tr])
    transGen <- subset(genRng.df, genRng.df$Transect==rng.df$Transect[tr])    
    transSf <- subset(sfRng.df, sfRng.df$Transect==rng.df$Transect[tr])
    transTrunc <- subset(trunc.pts, trunc.pts$Transect==rng.df$Transect[tr])
  
    rng.df$sp.mnRng.1600[tr] <- mean(ifelse(
      transSpp$HighEl < transTrunc$tr1600,
      transSpp$HighEl - transSpp$LowEl,
      transTrunc$tr1600 - transSpp$LowEl))
    rng.df$gen.mnRng.1600[tr] <- mean(ifelse(
      transGen$HighEl < transTrunc$tr1600,
      transGen$HighEl - transGen$LowEl,
      transTrunc$tr1600 - transGen$LowEl))
    rng.df$sf.mnRng.1600[tr] <- mean(ifelse(
      transSf$HighEl < transTrunc$tr1600,
      transSf$HighEl - transSf$LowEl,
      transTrunc$tr1600 - transSf$LowEl))
    rng.df$sp.medRng.1600[tr] <- median(ifelse(
      transSpp$HighEl < transTrunc$tr1600,
      transSpp$HighEl - transSpp$LowEl,
      transTrunc$tr1600 - transSpp$LowEl))
    rng.df$gen.medRng.1600[tr] <- median(ifelse(
      transGen$HighEl < transTrunc$tr1600,
      transGen$HighEl - transGen$LowEl,
      transTrunc$tr1600 - transGen$LowEl))
    rng.df$sf.medRng.1600[tr] <- median(ifelse(
      transSf$HighEl < transTrunc$tr1600,
      transSf$HighEl - transSf$LowEl,
      transTrunc$tr1600 - transSf$LowEl))
    rng.df$sp.mnRng.1800[tr] <- mean(ifelse(
      transSpp$HighEl < transTrunc$tr1800,
      transSpp$HighEl - transSpp$LowEl,
      transTrunc$tr1800 - transSpp$LowEl))
    rng.df$gen.mnRng.1800[tr] <- mean(ifelse(
      transGen$HighEl < transTrunc$tr1800,
      transGen$HighEl - transGen$LowEl,
      transTrunc$tr1800 - transGen$LowEl))
    rng.df$sf.mnRng.1800[tr] <- mean(ifelse(
      transSf$HighEl < transTrunc$tr1800,
      transSf$HighEl - transSf$LowEl,
      transTrunc$tr1800 - transSf$LowEl))
    rng.df$sp.medRng.1800[tr] <- median(ifelse(
      transSpp$HighEl < transTrunc$tr1800,
      transSpp$HighEl - transSpp$LowEl,
      transTrunc$tr1800 - transSpp$LowEl))
    rng.df$gen.medRng.1800[tr] <- median(ifelse(
      transGen$HighEl < transTrunc$tr1800,
      transGen$HighEl - transGen$LowEl,
      transTrunc$tr1800 - transGen$LowEl))
    rng.df$sf.medRng.1800[tr] <- median(ifelse(
      transSf$HighEl < transTrunc$tr1800,
      transSf$HighEl - transSf$LowEl,
      transTrunc$tr1800 - transSf$LowEl))
    rng.df$sp.mnRng.2000[tr] <- mean(ifelse(
      transSpp$HighEl < transTrunc$tr2000,
      transSpp$HighEl - transSpp$LowEl,
      transTrunc$tr2000 - transSpp$LowEl))
    rng.df$gen.mnRng.2000[tr] <- mean(ifelse(
      transGen$HighEl < transTrunc$tr2000,
      transGen$HighEl - transGen$LowEl,
      transTrunc$tr2000 - transGen$LowEl))
    rng.df$sf.mnRng.2000[tr] <- mean(ifelse(
      transSf$HighEl < transTrunc$tr2000,
      transSf$HighEl - transSf$LowEl,
      transTrunc$tr2000 - transSf$LowEl))
    rng.df$sp.medRng.2000[tr] <- median(ifelse(
      transSpp$HighEl < transTrunc$tr2000,
      transSpp$HighEl - transSpp$LowEl,
      transTrunc$tr2000 - transSpp$LowEl))
    rng.df$gen.medRng.2000[tr] <- median(ifelse(
      transGen$HighEl < transTrunc$tr2000,
      transGen$HighEl - transGen$LowEl,
      transTrunc$tr2000 - transGen$LowEl))
    rng.df$sf.medRng.2000[tr] <- median(ifelse(
      transSf$HighEl < transTrunc$tr2000,
      transSf$HighEl - transSf$LowEl,
      transTrunc$tr2000 - transSf$LowEl))
}

