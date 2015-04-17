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
## Median range size
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
