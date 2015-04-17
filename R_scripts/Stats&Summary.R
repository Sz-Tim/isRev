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
  t.test(over.df$mnRng ~ over.df$Zone)

#######
## Beta diversity
#######

  t.test(spSorAdj.df$sim, genSorAdj.df$sim, paired=TRUE)
  t.test(spSorAdj.df$sne, genSorAdj.df$sne, paired=TRUE)
  t.test(spSorAdj.df$sor, genSorAdj.df$sor, paired=TRUE)

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