# Range sizes across elevations
# Tim Szewczyk
# Created 2015 April 13

# This script estimates the change in mean elevational range size across an
# elevational gradient with three methods (McCain & Bracy Knight 2012):
# - Steven's: mean(el.range of all species at that el)
# - Midpoint: mean(el.range of all species with range midpoint at that el)
# - Quartile: mean(el.range of all species at that el w/ranges <25% gradient)


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


###########
## Midpoint method
###########



