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
  #--- log(S) ~ log(Gen) ---#
  lSlG.lme <- lmer(log(S) ~ log(Gen) + (1|Label), data=tvars.df)
  summary(lSlG.lme)
  lSlG.anc <- lm(log(S) ~ log(Gen)*Label, data=tvars.df)
  anova(lSlG.anc)
