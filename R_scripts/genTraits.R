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
  spRng.df <- read.xlsx("Sheets/ranges_spp.xlsx", 1)  # Species ranges
  over.df <- read.xlsx("Sheets/datasetOverview.xlsx", 1)  # Dataset summaries
  ivars.df <- read.xlsx("Sheets/intVars.xlsx", 1)  # Elev's sampled, env var's
  traits.df <- read.table("Sheets/occ_traits.txt", header=TRUE, sep="\t")
  Transects <- levels(spRng.df$Transect)  # Transects w/species range data
  nTrans <- nlevels(spRng.df$Transect)
  tvars.df <- droplevels(ivars.df[ivars.df$Transect %in% Transects, ])
  Labels <- as.character(unique(tvars.df$Label))
  nestSum.df <- ddply(traits.df, .(Transects, Elsamp, NestingSite), summarize,
                       nSpp=length(unique(Binomial)))
  feedSum.df <- ddply(traits.df, .(Transects, Elsamp, Specialization),
                      summarize, nSpp=length(unique(Binomial)))


#########
## 
#########

  ggplot(traits.df, aes(x=factor(Elsamp), fill=NestingSite)) + 
    geom_bar(position="fill") + facet_wrap(~Transects)

  ggplot(nestSum.df, aes(x=Elsamp, y=nSpp, colour=NestingSite)) +
    geom_line(size=1) + facet_wrap(~Transects)

  ggplot(feedSum.df, aes(x=Elsamp, y=nSpp, colour=Specialization)) +
    geom_line(size=1) + facet_wrap(~Transects)
