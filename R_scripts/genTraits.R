# Exploring patterns of turnover & nestedness
# Tim Szewczyk
# Created 2015 March 30

# This script explores elevational patterns of ant nesting habits and diet
# specialization. In particular, ground vs arboreal ants with those habits 
# generalized at the genus level due to data availability and time constraints.

#######
## Load libraries, functions, data
#######

  library(ggplot2); theme_set(theme_bw()); 
  library(xlsx); library(plyr); library(vegan); library(betapart)
  source("R_scripts/FuncsGen.R")
  loadAll()


#########
## 
#########

  ggplot(traits.df, aes(x=factor(Elsamp), fill=NestingSite)) + 
    geom_bar(position="fill") + facet_wrap(~Transects)

  ggplot(nestSum.df, aes(x=Elsamp, y=nSpp, colour=NestingSite)) +
    geom_line(size=1) + facet_wrap(~Transects)

  ggplot(feedSum.df, aes(x=Elsamp, y=nSpp, colour=Specialization)) +
    geom_line(size=1) + facet_wrap(~Transects)
