# Statistical analyses and summary statistics
# Tim Szewczyk
# Created 2015 April 07


#######
## Load libraries, functions, data
#######

  library(ggplot2); theme_set(theme_bw()); library(lme4)
  library(xlsx); library(plyr); library(vegan); library(betapart)
  source("R_scripts/FuncsGen.R")
  loadAll()


########
## Range sizes
########

  #### SUMMARIES ####
  #--- average range size across transects ---#
  with(spRng.df, tapply(HighEl-LowEl, Transect, mean))
  with(spRng.df, tapply(HighEl-LowEl, Transect, se))

  with(over.df, tapply(mnRng, Zone, mean, na.rm=TRUE))
  with(over.df, tapply(mnRng, Zone, se))

  #### ANALYSES ####

  #--- species ranges by categories ---#
  t.test(over.df$mnRng ~ over.df$Zone)
  t.test(over.df$mnRng ~ over.df$Scale)  # t.11.14=-4.42, P<0.001
  t.test(over.df$sp.mnRng.1600 ~ over.df$Scale)  # t.10.99=-2.70, P=0.021
  t.test(over.df$sp.mnRng.1800 ~ over.df$Scale)  # t.9.99=-4.15, P=0.002
  t.test(over.df$sp.mnRng.2000 ~ over.df$Scale)  # t.7.55=-5.60, P<0.001

  #--- species ranges by latitude ---#
  lat.rng.sp.all <- lm(over.df$sp.mnRng ~ abs(over.df$Latsamp))
  summary(lat.rng.sp.all) # t.14=3.03, P=0.009, R2=0.353
  lat.rng.sp.med <- lm(over.df$sp.medRng ~ abs(over.df$Latsamp))
  summary(lat.rng.sp.med) # t.14=2.22, P=0.04, R2=0.208
  lat.rng.sp.1600 <- lm(over.df$sp.mnRng.1600 ~ abs(over.df$Latsamp))
  summary(lat.rng.sp.1600) # t.11=2.70, P=0.021, R2=0.344
  lat.rng.sp.1800 <- lm(over.df$sp.mnRng.1800 ~ abs(over.df$Latsamp))
  summary(lat.rng.sp.1800) # t.10=3.09, P=0.012, R2=0.437
  lat.rng.sp.2000 <- lm(over.df$sp.mnRng.2000 ~ abs(over.df$Latsamp))
  summary(lat.rng.sp.2000) # t.8=2.57, P=0.033, R2=0.384

  #--- genus ranges by latitude ---#
  lat.rng.gen.all <- lm(over.df$gen.mnRng ~ abs(over.df$Latsamp))
  summary(lat.rng.gen.all) # t.14=1.28, P=0.220, R2=0.041
  
  #--- subfamily ranges by latitude ---#
  lat.rng.sf.all <- lm(over.df$sf.mnRng ~ abs(over.df$Latsamp))
  summary(lat.rng.sf.all) # t.14=0.87, P=0.399, R2=-0.017

  #--- proportion of species at only one elevation ---#
  summary(lm(over.df$pr1el ~ abs(over.df$Latsamp)))

  #--- Rapoport's Rule ---#
  summary(over.df$stevens.r)
  sum(over.df$stevens.p < 0.05, na.rm=TRUE)
  summary(over.df$mp.r)
  sum(over.df$mp.p < 0.05 & over.df$mp.b > 0, na.rm=TRUE)
  summary(over.df$quart.r)
  sum(over.df$quart.p < 0.05 & over.df$quart.b > 0, na.rm=TRUE)

#######
## Beta diversity
#######

  t.test(spSorAdj.df$sim, genSorAdj.df$sim, paired=TRUE)
  t.test(spSorAdj.df$sne, genSorAdj.df$sne, paired=TRUE)
  t.test(spSorAdj.df$sor, genSorAdj.df$sor, paired=TRUE)

  #--- standardized beta ---#
  sp.beta.st <- lm(sp.STB ~ abs(Latsamp), data=over.df)
  summary(sp.beta.st)
  gen.beta.st <- lm(gen.STB ~ abs(Latsamp), data=over.df)
  summary(gen.beta.st)
  sf.beta.st <- lm(sf.STB ~ abs(Latsamp), data=over.df)
  summary(sf.beta.st)

  #--- gradient-wide proportions ---#
  wilcox.test(sp.sim/sp.sor ~ Zone, data=over.df)
  t.test(over.df$sp.sim, over.df$sp.sne, paired=TRUE)
  wilcox.test(gen.sim/gen.sor ~ Zone, data=over.df)
  t.test(over.df$gen.sim, over.df$gen.sne, paired=TRUE)
  wilcox.test(sf.sim/sf.sor ~ Zone, data=over.df)
  t.test(over.df$sf.sim, over.df$sf.sne, paired=TRUE)

  #--- adjacent bands ---#
  t.test(spSorAdj.df$sim, spSorAdj.df$sne, paired=TRUE) # all trans together
  mns <- tapply(spSorAdj.df$sne-spSorAdj.df$sim, spSorAdj.df$Label, mean)
  mean(mns)
  mean(mns) + qt(0.975, 15)*sd(mns)/sqrt(16)
  mean(mns) - qt(0.975, 15)*sd(mns)/sqrt(16)
  summary(aov(sne ~ sim + Error(Label/El1/sim), spSorAdj.df)) # I think?
  summary(aov(sne ~ sim + Error(Label/El1/sim), genSorAdj.df)) # I think?
  summary(aov(sne ~ sim + Error(Label/El1/sim), sfSorAdj.df)) # I think?

  summary(lm(sor ~ sne, data=spSorAdj.df))
    plot(sor ~ sne, data=spSorAdj.df, ylim=c(0,1), xlim=c(0,1));abline(b=1, a=0)
  summary(lm(sor ~ sim, data=spSorAdj.df)) 
    plot(sor ~ sim, data=spSorAdj.df, ylim=c(0,1), xlim=c(0,1));abline(b=1, a=0)

  summary(lm(sor ~ sne, data=genSorAdj.df))
  plot(sor ~ sne, data=genSorAdj.df, ylim=c(0,1), xlim=c(0,1));abline(b=1, a=0)
  summary(lm(sor ~ sim, data=genSorAdj.df)) 
  plot(sor ~ sim, data=genSorAdj.df, ylim=c(0,1), xlim=c(0,1));abline(b=1, a=0)

  summary(lm(sor ~ sne, data=sfSorAdj.df))
  plot(sor ~ sne, data=sfSorAdj.df, ylim=c(0,1), xlim=c(0,1));abline(b=1, a=0)
  summary(lm(sor ~ sim, data=sfSorAdj.df)) 
  plot(sor ~ sim, data=sfSorAdj.df, ylim=c(0,1), xlim=c(0,1));abline(b=1, a=0)


###########
## Richness
###########

  #--- species: patterns by zone ---#
  spp.patt <- matrix(c(3,1,9, 0,2,5), ncol=2, byrow=FALSE, 
                     dimnames=list(c("D","LP","MP"), c("Temperate","Tropic")))
  fisher.test(spp.patt)  # P=0.30

  #--- genus: patterns by zone ---#
  gen.patt <- matrix(c(3,1,0,6, 1,2,2,1), ncol=2, byrow=FALSE, 
                     dimnames=list(c("D","LP", "LPMP","MP"), 
                                   c("Temperate","Tropic")))
  fisher.test(gen.patt)  # P=0.11

  #--- genus: comparison to species patterns ---#
  spgen.patt <- matrix(c(3,3,0,14,0, 4,3,2,7,0), ncol=2, byrow=FALSE,
                       dimnames=list(as.character(patt.barSUM$Pattern[1:5]),
                                     c("Species", "Genus")))
  fisher.test(spgen.patt)  # P=0.28

  #--- genus: comparison to species patterns ---#
  spgen.patt <- matrix(c(2,3,0,11,0, 4,3,2,7,0), ncol=2, byrow=FALSE,
                     dimnames=list(as.character(patt.barSUM$Pattern[1:5]),
                                   c("Species", "Genus")))
  fisher.test(spgen.patt)  # P=0.39

  #--- log(S) ~ log(Gen) ---#
  lSlG.df <- data.frame(Label=Labels, 
                        b=rep(NA, nTrans),
                        p=rep(NA, nTrans),
                        r=rep(NA, nTrans))
  for(tr in 1:nTrans) {
    t.df <- subset(tvars.df, tvars.df$Label==Labels[tr])
    mod <- lm(log(S) ~ log(Gen), data=t.df)
    lSlG.df$b[tr] <- coef(mod)[2]
    lSlG.df$p[tr] <- summary(mod)$coefficients[2,4]
    lSlG.df$r[tr] <- summary(mod)$r.squared
  }
  mean(lSlG.df$r)  # 0.96

  #--- subfamily: patterns by zone ---#
  sf.patt <- matrix(c(1,4,0,4,1, 3,1,1,1,0), ncol=2, byrow=FALSE, 
                    dimnames=list(c("D","LP","LPMP","MP","None"), 
                                  c("Temperate","Tropic")))
  fisher.test(sf.patt) # P=0.28

  #--- subfamily: comparison to species patterns ---#
  spsf.patt <- matrix(c(3,3,0,14,0, 4,5,1,5,1), ncol=2, byrow=FALSE,
                       dimnames=list(as.character(patt.barSUM$Pattern[1:5]),
                                     c("Species", "Subfamily")))
  fisher.test(spsf.patt)  # P=0.10

  #--- subfamily: comparison to species patterns ---#
  spsf.patt <- matrix(c(2,3,0,11,0, 4,5,1,5,1), ncol=2, byrow=FALSE,
                      dimnames=list(as.character(patt.barSUM$Pattern[1:5]),
                                    c("Species", "Subfamily")))
  fisher.test(spsf.patt)  # P=0.20

  #--- subfamily: comparison to genus patterns ---#
  gensf.patt <- matrix(c(4,3,2,7,0, 4,5,1,5,1), ncol=2, byrow=FALSE,
                      dimnames=list(as.character(patt.barSUM$Pattern[1:5]),
                                    c("Genus", "Subfamily")))
  fisher.test(gensf.patt)  # P=0.84

  #--- log(S) ~ log(SF) ---#
  lSlSF.df <- data.frame(Label=Labels, 
                        b=rep(NA, nTrans),
                        p=rep(NA, nTrans),
                        r=rep(NA, nTrans))
  for(tr in 1:nTrans) {
    t.df <- subset(tvars.df, tvars.df$Label==Labels[tr])
    mod <- lm(log(S) ~ log(SF), data=t.df)
    lSlSF.df$b[tr] <- coef(mod)[2]
    lSlSF.df$p[tr] <- summary(mod)$coefficients[2,4]
    lSlSF.df$r[tr] <- summary(mod)$r.squared
  }
  mean(lSlSF.df$r)  # 0.79


############
## Taxonomic proportions
############

  #### SUMMARIES ####
  #--- most diverse genus ---#
  with(over.df, summary(maxDivGen/Stot, na.rm=TRUE)) 
  with(over.df, se(maxDivGen/Stot))
  # mn:0.200; se:0.0157; med:0.205; min:0.071; max:0.306;
  
  #--- most diverse subfamily ---#
  with(over.df, summary(maxDivSF/Stot, na.rm=TRUE)) 
  with(over.df, se(maxDivSF/Stot))
  # mn:0.545; se:0.0204; med:0.531; min:0.391; max:0.731
  
  #### ANALYSES ####
  #--- most diverse genus proportion by latitude ---#
  summary(lm(maxDivGen/Stot ~ abs(Latsamp), data=over.df)) # P=0.15, R2=0.08

  #--- most diverse genus predicting rest ---#
  mean(over.df$genPred.r, na.rm=TRUE) # 0.69
  summary(over.df$genPred.b)  # mean=3.5, range=-0.91-10.8)
  sum(over.df$genPred.p < 0.05, na.rm=TRUE)  # 14/16

  #--- most diverse genus predicting all ---#
  g.df <- data.frame(Label=Labels, 
                     b=rep(NA, nTrans),
                     p=rep(NA, nTrans),
                     r=rep(NA, nTrans))
  for(tr in 1:nTrans) {
    t.df <- subset(tvars.df, tvars.df$Label==Labels[tr])
    mod <- lm(log(S+1) ~ log(SmaxDivGen+1), data=t.df)
    g.df$b[tr] <- coef(mod)[2]
    g.df$p[tr] <- summary(mod)$coefficients[2,4]
    g.df$r[tr] <- summary(mod)$r.squared
  }
  mean(g.df$r)  # 0.79
  
  #--- most diverse subfamily proportion by latitude ---#
  summary(lm(maxDivSF/Stot ~ abs(Latsamp), data=over.df)) # P=0.38; R2=0.06

  #--- most diverse subfamily predicting rest ---#
  mean(over.df$sfPred.r, na.rm=TRUE) # 0.81  
  summary(over.df$sfPred.b)  # mean=0.77, range=0.3-1.3)
  sum(over.df$sfPred.p < 0.05, na.rm=TRUE)  # 16/16 

  #--- most diverse subfamily predicting all ---#
  sf.df <- data.frame(Label=Labels, 
                     b=rep(NA, nTrans),
                     p=rep(NA, nTrans),
                     r=rep(NA, nTrans))
  for(tr in 1:nTrans) {
    t.df <- subset(tvars.df, tvars.df$Label==Labels[tr])
    mod <- lm(log(S+1) ~ log(SmaxDivSF+1), data=t.df)
    sf.df$b[tr] <- coef(mod)[2]
    sf.df$p[tr] <- summary(mod)$coefficients[2,4]
    sf.df$r[tr] <- summary(mod)$r.squared
  }
  mean(sf.df$r)  # 0.97

