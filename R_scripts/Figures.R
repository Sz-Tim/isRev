# Code for creating pdf figures of all kinds
# Tim Szewczyk
# Created 2015 March 20


#######
## Load libraries, functions, data
#######

  #--- libraries & functions ---#
  library(ggplot2); theme_set(theme_bw()); 
  library(xlsx); library(plyr); library(vegan); library(betapart)
  source("R_scripts/FuncsGen.R")

  #--- data sources ---#
  spRng.df <- read.xlsx("Sheets/ranges_spp.xlsx", 1)  # Species ranges
  genRng.df <- read.xlsx("Sheets/ranges_gen.xlsx", 1)  # Genus ranges
  sfRng.df <- read.xlsx("Sheets/ranges_sf.xlsx", 1)  # Subfamily ranges
  spSor.df <- read.csv("Sheets/spp_Sorensen.csv") # Species beta diversity
  genSor.df <- read.csv("Sheets/gen_Sorensen.csv") # Genus beta diversity
  sfSor.df <- read.csv("Sheets/sf_Sorensen.csv") # Subfamily beta diversity
  over.df <- read.xlsx("Sheets/datasetOverview.xlsx", 1)  # Dataset summaries
  ivars.df <- read.xlsx("Sheets/intVars.xlsx", 1)  # Elev's sampled, env var's
  gen.bars <- read.csv("Sheets/relDiversity_gen.csv")  # Richness by genus
  sf.bars <- read.csv("Sheets/relDiversity_sf.csv")  # Richness by sf

  #--- useful summary objects ---#
  Transects <- levels(spRng.df$Transect)  # Transects w/species range data
  nTrans <- nlevels(spRng.df$Transect)
  tvars.df <- droplevels(ivars.df[ivars.df$Transect %in% Transects, ])

  #--- graphical parameters ---#
  w <- 15  # width of pdf (inches)
  h <- 12  # height of pdf (inches)


##########
## Overall diversity patterns
##########

  #--- species diversity ---#
  pdf(file="Plots/Diversity_spp.pdf", width=w, height=h)
    ggplot(ivars.df, aes(x=Elsamp, y=Ssimpint)) + facet_wrap(~Label) +
      geom_line() + labs(x="Elevation (m)", y="Number of species")
  dev.off()

  #--- genus diversity ---#
  pdf(file="Plots/Diversity_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=Gen)) + facet_wrap(~Label) +
      geom_line() + labs(x="Elevation (m)", y="Number of genera")
  dev.off()

  #--- subfamily diversity ---#
  pdf(file="Plots/Diversity_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=SF)) + facet_wrap(~Label) +
      geom_line() + labs(x="Elevation (m)", y="Number of subfamilies")
  dev.off()

  #--- within-genus diversity ---#
  pdf(file="Plots/DiversityIn_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=Aphaenogaster), colour="darkorange") + 
      geom_line(aes(y=Camponotus), colour="darkorange2") + 
      geom_line(aes(y=Cerapachys), colour="darkorange4") + 
      geom_line(aes(y=Crematogaster), colour="darkorchid1") + 
      geom_line(aes(y=Formica), colour="darkorchid3") + 
      geom_line(aes(y=Hypoponera), colour="darkorchid4") + 
      geom_line(aes(y=Lasius), colour="darkseagreen1") + 
      geom_line(aes(y=Leptogenys), colour="darkseagreen3") + 
      geom_line(aes(y=Leptothorax), colour="darkseagreen4") + 
      geom_line(aes(y=Monomorium), colour="firebrick1") + 
      geom_line(aes(y=Myrmecocystus), colour="firebrick3") + 
      geom_line(aes(y=Myrmica), colour="firebrick4") + 
      geom_line(aes(y=Pachycondyla), colour="goldenrod1") + 
      geom_line(aes(y=Pheidole), colour="goldenrod3") + 
      geom_line(aes(y=Pyramica), colour="goldenrod4") + 
      geom_line(aes(y=Solenopsis), colour="indianred1") + 
      geom_line(aes(y=Stenamma), colour="indianred3") + 
      geom_line(aes(y=Strumigenys), colour="indianred4") + 
      geom_line(aes(y=Temnothorax), colour="royalblue1") + 
      geom_line(aes(y=Tetramorium), colour="royalblue4")
  dev.off()

  #--- within-subfamily diversity ---#
  pdf(file="Plots/DiversityIn_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=S)) +
      geom_line(aes(y=Cerapachyinae), colour="gray10") +
      geom_line(aes(y=Dolichoderinae), colour="gray60") +
      geom_line(aes(y=Formicinae), colour="blue") +
      geom_line(aes(y=Myrmicinae), colour="green") +
      geom_line(aes(y=Ponerinae), colour="red") 
  dev.off()


#######
## Most diverse genus/subfamily
#######

  #--- genus: most diverse vs rest ---#
  pdf(file="Plots/MostDivVsRest_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=S, colour="Total diversity")) + 
      geom_line(aes(y=S-SmaxDivGen, colour="All but most diverse gn")) +
      geom_line(aes(y=SmaxDivGen, colour="Most diverse gn")) +
      scale_colour_manual(name="", values=c('Total diversity'='black',
                                          'Most diverse genus'='green',
                                          'All but most diverse genus'='blue'))
  dev.off()

  #--- genus: most diverse vs rest ---#
  pdf(file="Plots/MostDivVsRest_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=S, colour="Total diversity")) + 
      geom_line(aes(y=S-SmaxDivSF, colour="All but most diverse subfamily")) +
      geom_line(aes(y=SmaxDivSF, colour="Most diverse subfamily")) +
      scale_colour_manual(name="", values=c('Total diversity'='black',
                                      'Most diverse subfamily'='green',
                                      'All but most diverse subfamily'='blue'))
  dev.off()


############
## Community composition patterns
############

  #--- proportion of species by genus ---#
  pdf(file="Plots/SppProp_gen.pdf", width=w, height=h)
    ggplot(gen.bars, aes(x=Elsamp, y=GennumSpp, fill=Genus)) + 
      geom_area(position="fill", colour="gray50") + facet_wrap(~Label) +
      labs(x="Elevation (m)", y="Proportion species composition")
  dev.off()

  #--- proportion of species by subfamily ---#
  pdf(file="Plots/SppProp_sf.pdf", width=w, height=h)
    ggplot(sf.bars, aes(x=Elsamp, y=SFnumSpp, fill=Subfamily)) + 
      geom_area(position="fill", colour="gray50") + facet_wrap(~Label) +
      labs(x="Elevation (m)", y="Proportion species composition")
  dev.off()


#######
## Beta diversity
#######

  #--- species: overall beta diversity ---#
  pdf(file="Plots/BetaSim_spp.pdf", width=w, height=h)
    ggplot(spSor.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sim", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- species: turnover component ---#
  pdf(file="Plots/BetaSor_spp.pdf", width=w, height=h)
    ggplot(spSor.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sor", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()
  
  #--- species: nestedness component ---#
  pdf(file="Plots/BetaSne_spp.pdf", width=w, height=h)
    ggplot(spSor.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sne", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- genus: overall beta diversity ---#
  pdf(file="Plots/BetaSim_gen.pdf", width=w, height=h)
    ggplot(genSor.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sim", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- genus: turnover component ---#
  pdf(file="Plots/BetaSor_gen.pdf", width=w, height=h)
    ggplot(genSor.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sor", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- genus: nestedness component ---#
  pdf(file="Plots/BetaSne_gen.pdf", width=w, height=h)
    ggplot(genSor.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sne", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- subfamily: overall beta diversity ---#
  pdf(file="Plots/BetaSim_sf.pdf", width=w, height=h)
    ggplot(sfSor.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sim", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- subfamily: turnover component ---#
  pdf(file="Plots/BetaSim_sf.pdf", width=w, height=h)
    ggplot(sfSor.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sor", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- subfamily: nestedness component ---#
  pdf(file="Plots/BetaSim_sf.pdf", width=w, height=h)
    ggplot(sfSor.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="sne", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()
