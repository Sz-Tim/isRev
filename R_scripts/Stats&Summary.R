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
  #--- most diverse genus ---#
  divG.lme <- lmer((S-SmaxDivGen) ~ SmaxDivGen + (1|Label), data=tvars.df)
  summary(divG.lme)
  
  #--- log(S) ~ log(Gen) ---#
  lSlG.lme <- lmer(log(S) ~ log(Gen) + (1|Label), data=tvars.df)
  summary(lSlG.lme)
  lSlG.anc <- lm(log(S) ~ log(Gen)*Label, data=tvars.df)
  anova(lSlG.anc)


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

  #--- Steven's method ---#
  summary(over.df$stevens.r)
  sum(over.df$stevens.p < 0.05, na.rm=TRUE)
  summary(over.df$mp.r)
  sum(over.df$mp.p < 0.05, na.rm=TRUE)
  summary(over.df$quart.r)
  sum(over.df$quart.p < 0.05, na.rm=TRUE)

#######
## Beta diversity
#######

  t.test(spSorAdj.df$sim, genSorAdj.df$sim, paired=TRUE)
  t.test(spSorAdj.df$sne, genSorAdj.df$sne, paired=TRUE)
  t.test(spSorAdj.df$sor, genSorAdj.df$sor, paired=TRUE)

  #--- standardized beta ---#
  beta.st <- lm(sp.STB ~ abs(Latsamp), data=over.df)
  summary(beta.st)

  #--- gradient-wide proportions ---#
  wilcox.test(sp.sim/sp.sor ~ Zone, data=over.df)
  t.test(over.df$sp.sim, over.df$sp.sne, paired=TRUE)

  #--- adjacent bands ---#
  t.test(spSorAdj.df$sim, spSorAdj.df$sne, paired=TRUE)

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