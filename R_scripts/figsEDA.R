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
  loadAll()

  #--- graphical parameters ---#
  w <- 15  # width of pdf (inches)
  h <- 12  # height of pdf (inches)


##########
## Overall diversity patterns
##########

  #--- species diversity ---#
  pdf(file="Plots/EDA/Richness/Diversity_spp.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=Ssimpint)) + facet_wrap(~Label) +
      geom_point() + labs(x="Elevation (m)", y="Number of species") +
      stat_smooth(se=FALSE, method="loess", span=1)
  dev.off()

  #--- genus diversity ---#
  pdf(file="Plots/EDA/Richness/Diversity_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=Gen)) + facet_wrap(~Label) +
      geom_point() + labs(x="Elevation (m)", y="Number of genera") +
      stat_smooth(se=FALSE, method="loess", span=1)
  dev.off()

  #--- subfamily diversity ---#
  pdf(file="Plots/EDA/Richness/Diversity_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=SF)) + facet_wrap(~Label) +
      geom_point() + labs(x="Elevation (m)", y="Number of subfamilies") +
      stat_smooth(se=FALSE, method="loess", span=1)
  dev.off()

  #--- all taxonomic levels ---#
  pdf(file="Plots/EDA/Richness/Diversity_ALL.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=Savgint, colour="Species")) +
      geom_line(aes(y=Gen, colour="Genera")) +
      geom_line(aes(y=SF, colour="Subfamilies")) +
      scale_colour_manual(name="Richness", values=c('Species'="black",
                                                    'Genera'="blue",
                                                    'Subfamilies'="red"))
  dev.off()

  #--- species by genera ---#
  pdf(file="Plots/EDA/Richness/SppModByGen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Gen, y=Savgint, colour=Label, group=Label)) + 
      geom_point(size=3) + labs(x="Number of genera", y="Number of species") +
      stat_smooth(method="loess", se=FALSE, span=1.25, size=1)
  dev.off()
  pdf(file="Plots/EDA/Richness/SppModByGen_LOG.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=log(Gen), y=log(Savgint), 
                         colour=Label, group=Label)) + 
      geom_point(size=3) + 
      labs(x="log(Number of genera)", y="log(Number of species)") +
      stat_smooth(method="lm", se=FALSE, span=1.25, size=1)    
  dev.off()

  #--- species by subfamily ---#
  pdf(file="Plots/EDA/Richness/SppModBySF.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=SF, y=Savgint, colour=Label, group=Label)) + 
      geom_point(size=3) + 
      labs(x="Number of subfamilies", y="Number of species") +
      stat_smooth(method="loess", se=FALSE, span=1.25, size=1)
  dev.off()
  pdf(file="Plots/EDA/Richness/SppModBySF_LOG.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=log(SF), y=log(Savgint), 
                         colour=Label, group=Label)) + 
      geom_point(size=3) + 
      labs(x="log(Number of subfamilies)", y="log(Number of species)") +
      stat_smooth(method="loess", se=FALSE, span=1.25, size=1)    
  dev.off()

  #--- genera by subfamily ---#
  pdf(file="Plots/EDA/Richness/GenModBySF.pdf", width=w, height=h)
  ggplot(tvars.df, aes(x=SF, y=Gen, colour=Label, group=Label)) + 
      geom_point(size=3) + 
      labs(x="Number of subfamilies", y="Number of genera") +
      stat_smooth(method="loess", se=FALSE, span=1.25, size=1)
  dev.off()
  pdf(file="Plots/EDA/Richness/GenModBySF_LOG.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=log(SF), y=log(Gen), 
                         colour=Label, group=Label)) + 
      geom_point(size=3) + 
      labs(x="log(Number of subfamilies)", y="log(Number of genera)") +
      stat_smooth(method="loess", se=FALSE, span=1.25, size=1)    
  dev.off()

  #--- within-genus diversity ---#
  pdf(file="Plots/EDA/Richness/DiversityIn_gen.pdf", width=w, height=h)
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
  pdf(file="Plots/EDA/Richness/DiversityIn_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=S)) +
      geom_line(aes(y=Cerapachyinae), colour="gray10") +
      geom_line(aes(y=Dolichoderinae), colour="gray60") +
      geom_line(aes(y=Formicinae), colour="blue") +
      geom_line(aes(y=Myrmicinae), colour="green") +
      geom_line(aes(y=Ponerinae), colour="red") 
  dev.off()

  #--- comparison of elevation of peaks ---#
  pdf(file="Plots/EDA/Richness/ElPeaks.pdf", width=w, height=h)
    ggplot(over.df, aes(x=Elpeak-GenPeakEl)) + geom_histogram()
  dev.off()

#######
## Most diverse genus/subfamily
#######

  #--- genus: most diverse vs rest ---#
  pdf(file="Plots/EDA/DominantTaxa/MostDivVsRest_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=S, colour="Total diversity")) + 
      geom_line(aes(y=S-SmaxDivGen, colour="All but most diverse gn")) +
      geom_line(aes(y=SmaxDivGen, colour="Most diverse gn")) +
      scale_colour_manual(name="", values=c('Total diversity'='black',
                                          'Most diverse gn'='green',
                                          'All but most diverse gn'='blue'))
  dev.off()

  #--- genus: most diverse predicting rest ---#
  pdf(file="Plots/EDA/DominantTaxa/MostDivPredRest_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=SmaxDivGen, y=S-SmaxDivGen)) +
      stat_smooth(aes(group=Label), se=F, method="lm", colour="gray") +
      stat_smooth(se=F, method="lm", colour="black", size=1) +  geom_point() +
      labs(x=expression('S'['most speciose genus']), 
           y=expression('S'['remaining genera']))
  dev.off()

  #--- genus: most diverse as proportion ---#
  pdf(file="Plots/EDA/DominantTaxa/MostDivProp_gen.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=SmaxDivGen/S)) + facet_wrap(~Label) +
      geom_hline(yintercept=c(0,1), linetype=3, colour="gray60") + 
      geom_point() + stat_smooth(se=FALSE, span=1.25, method="loess")
  dev.off()
  pdf(file="Plots/EDA/DominantTaxa/MostDivPropHist_gen.pdf", width=w, height=h)
    ggplot(over.df, aes(x=maxDivGen/Stot)) + geom_histogram(binwidth=0.025) +
      xlim(0,0.4) + labs(x=expression('S'['most speciose genus']/'S'['total']))
  dev.off()

  #--- subfamily: most diverse vs rest ---#
  pdf(file="Plots/EDA/DominantTaxa/MostDivVsRest_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp)) + facet_wrap(~Label) +
      geom_line(aes(y=S, colour="Total diversity")) + 
      geom_line(aes(y=S-SmaxDivSF, colour="All but most diverse subfamily")) +
      geom_line(aes(y=SmaxDivSF, colour="Most diverse subfamily")) +
      scale_colour_manual(name="", values=c('Total diversity'='black',
                                      'Most diverse subfamily'='green',
                                      'All but most diverse subfamily'='blue'))
  dev.off()

  #--- subfamily: most diverse predicting rest ---#
  pdf(file="Plots/EDA/DominantTaxa/MostDivPredRest_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=SmaxDivSF, y=S-SmaxDivSF)) +
      stat_smooth(aes(group=Label), se=F, method="lm", colour="gray") +
      stat_smooth(se=F, method="lm", colour="black", size=1) +  geom_point() +
      labs(x=expression('S'['most speciose subfamily']), 
           y=expression('S'['remaining subfamilies']))
  dev.off()

  #--- subfamily: most diverse as proportion ---#
  pdf(file="Plots/EDA/DominantTaxa/MostDivProp_sf.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=Elsamp, y=SmaxDivSF/S)) + facet_wrap(~Label) +
      geom_hline(yintercept=c(0,1), linetype=3, colour="gray60") + 
      geom_point() + stat_smooth(se=FALSE, span=1.25, method="loess")
  dev.off()
  pdf(file="Plots/EDA/DominantTaxa/MostDivPropHist_sf.pdf", width=w, height=h)
    ggplot(over.df, aes(x=maxDivSF/Stot)) + geom_histogram(binwidth=0.025) +
      xlim(0,1) + labs(x=expression('S'['most speciose sf']/'S'['total']))
  dev.off()


############
## Community composition patterns
############

  #--- proportion of species by genus ---#
  pdf(file="Plots/EDA/Composition/SppProp_gen.pdf", width=w, height=h)
    ggplot(gen.bars, aes(x=Elsamp, y=GennumSpp, fill=Genus)) + 
      geom_area(position="fill", colour="gray50") + facet_wrap(~Label) +
      labs(x="Elevation (m)", y="Proportion species composition")
  dev.off()

  #--- proportion of species by subfamily ---#
  pdf(file="Plots/EDA/Composition/SppProp_sf.pdf", width=w, height=h)
    ggplot(sf.bars, aes(x=Elsamp, y=SFnumSpp, fill=Subfamily)) + 
      geom_area(position="fill", colour="gray50") + facet_wrap(~Label) +
      labs(x="Elevation (m)", y="Proportion species composition")
  dev.off()


#######
## Beta diversity: All sites
#######

  #--- species: overall beta diversity ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaTotal_spp.pdf", width=w, height=h)
    ggplot(spSor.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Beta diversity", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- species: turnover component ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaTurnover_spp.pdf", width=w, height=h)
    ggplot(spSor.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Turnover", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()
  
  #--- species: nestedness component ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaNested_spp.pdf", width=w, height=h)
    ggplot(spSor.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Nestedness", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- genus: overall beta diversity ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaTotal_gen.pdf", width=w, height=h)
    ggplot(genSor.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Beta diversity", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- genus: turnover component ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaTurnover_gen.pdf", width=w, height=h)
    ggplot(genSor.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Turnover", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- genus: nestedness component ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaNested_gen.pdf", width=w, height=h)
    ggplot(genSor.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Nestedness", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- subfamily: overall beta diversity ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaTotal_sf.pdf", width=w, height=h)
    ggplot(sfSor.df, aes(x=El2, y=El1, colour=sor)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Beta diversity", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- subfamily: turnover component ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaTurnover_sf.pdf", width=w, height=h)
    ggplot(sfSor.df, aes(x=El2, y=El1, colour=sim)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Turnover", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()

  #--- subfamily: nestedness component ---#
  pdf(file="Plots/EDA/BetaDiversity/BetaNested_sf.pdf", width=w, height=h)
    ggplot(sfSor.df, aes(x=El2, y=El1, colour=sne)) + facet_wrap(~Label) +
      geom_point(size=2.75, shape=15) + 
      scale_colour_gradient(name="Nestedness", low="white", high="black") +
      labs(x="Elevation (m)", y="Elevation (m)")
  dev.off()


#######
## Beta diversity: Adjacent sites
#######

  #--- species: adjacent sites ---#
  pdf(file="Plots/EDA/BetaDiversity/AdjBeta_spp.pdf", width=w, height=h)
    ggplot(spSorAdj.df, aes(x=(El1 + El2)/2)) + facet_wrap(~Label) + 
      geom_point(aes(y=sor, colour="Total beta"), size=3) +
      stat_smooth(aes(y=sor), colour="black", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=sne, colour="Nestedness"), size=3) + 
      stat_smooth(aes(y=sne), colour="red", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=sim, colour="Turnover"), size=3) + 
      stat_smooth(aes(y=sim), colour="blue", size=1, 
                  method="loess", span=1.75, se=F) +
      scale_colour_manual(name="Sorensen Family", 
                          values=c("Total beta"="black",
                                   "Nestedness"="red",
                                   "Turnover"="blue"),
                          limits=c("Total beta", "Nestedness", "Turnover")) +
      ylim(0,1) + labs(x="Elevation (m)", y=expression(beta), main="Species")
  dev.off()

  #--- genus: adjacent sites ---#
  pdf(file="Plots/EDA/BetaDiversity/AdjBeta_gen.pdf", width=w, height=h)
    ggplot(genSorAdj.df, aes(x=(El1 + El2)/2)) + facet_wrap(~Label) + 
      geom_point(aes(y=sor, colour="Total beta"), size=3) +
      stat_smooth(aes(y=sor), colour="black", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=sne, colour="Nestedness"), size=3) + 
      stat_smooth(aes(y=sne), colour="red", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=sim, colour="Turnover"), size=3) + 
      stat_smooth(aes(y=sim), colour="blue", size=1, 
                  method="loess", span=1.75, se=F) +
      scale_colour_manual(name="Sorensen Family", 
                          values=c("Total beta"="black",
                                   "Nestedness"="red",
                                   "Turnover"="blue"),
                          limits=c("Total beta", "Nestedness", "Turnover")) +
      ylim(0,1) + labs(x="Elevation (m)", y=expression(beta), main="Genus")
  dev.off()

#--- subfamily: adjacent sites ---#
  pdf(file="Plots/EDA/BetaDiversity/AdjBeta_sf.pdf", width=w, height=h)
    ggplot(sfSorAdj.df, aes(x=(El1 + El2)/2)) + facet_wrap(~Label) + 
      geom_point(aes(y=sor, colour="Total beta"), size=3) +
      stat_smooth(aes(y=sor), colour="black", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=sne, colour="Nestedness"), size=3) + 
      stat_smooth(aes(y=sne), colour="red", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=sim, colour="Turnover"), size=3) + 
      stat_smooth(aes(y=sim), colour="blue", size=1, 
                  method="loess", span=1.75, se=F) +
      scale_colour_manual(name="Sorensen Family", 
                          values=c("Total beta"="black",
                                   "Nestedness"="red",
                                   "Turnover"="blue"),
                          limits=c("Total beta", "Nestedness", "Turnover")) +
      ylim(0,1) + labs(x="Elevation (m)", y=expression(beta), main="Subfamily")
  dev.off()



#######
## Beta diversity compared across taxonomic scales at adjacent sites
#######

  pdf(file="Plots/EDA/BetaDiversity/AdjBeta_SpG_Comp.pdf", width=w, height=h)
    ggplot(taxcomp.df, aes(x=(El1 + El2)/2)) + facet_wrap(~Label) +
      geom_hline(yintercept=0, linetype=2, colour="gray") + 
      geom_point(aes(y=SpG.sor, colour="Total beta"), size=3) +
      stat_smooth(aes(y=SpG.sor), colour="black", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=SpG.sne, colour="Nestedness"), size=3) + 
      stat_smooth(aes(y=SpG.sne), colour="red", size=1, 
                  method="loess", span=1.75, se=F) +
      geom_point(aes(y=SpG.sim, colour="Turnover"), size=3) + 
      stat_smooth(aes(y=SpG.sim), colour="blue", size=1, 
                  method="loess", span=1.75, se=F) +
      scale_colour_manual(name="Sorensen Family", 
                          values=c("Total beta"="black",
                                   "Nestedness"="red",
                                   "Turnover"="blue"),
                          limits=c("Total beta", "Nestedness", "Turnover")) +
      labs(x="Elevation (m)", y=expression(beta['Species'] - beta['Genus']))
  dev.off()



########
## Range size
########

ggplot(over.df, aes(x=Zone, y=mnRng, fill=Scale)) + geom_boxplot() + 
  geom_rug(aes(colour=Scale), sides="l") + 
  labs(x="", y="Mean elevational range (m)") + ylim(0, 1000)

ggplot(over.df, aes(x=Zone, y=mnRng/(MtnPeak-MtnBase), fill=Scale)) + 
  geom_boxplot() + geom_rug(aes(colour=Scale), sides="l") + 
  labs(x="", y="Mean elevational range (proportion of gradient)") + ylim(0, 0.5)

########
## Other doodles
########

ggplot(spSorAdj.df, aes(x=(El1 + El2)/2)) + facet_wrap(~Label) +
  geom_line(aes(y=sor), size=1) + 
  geom_ribbon(aes(ymin=ymin, ymax=sne), fill="red", alpha=0.5) +
  geom_ribbon(aes(ymin=sne, ymax=sne+sim), fill="blue", alpha=0.5)

ggplot(spSorAdj.df, aes(x=(El1 + El2)/2)) + facet_wrap(~Label) +
  geom_ribbon(aes(ymin=ymin, ymax=sne/sor, fill="Nestedness"), alpha=0.65) +
  geom_ribbon(aes(ymin=sne/sor, ymax=1, fill="Turnover"), alpha=0.65) +
  geom_line(aes(y=sne/sor), colour="black") + 
  labs(x="Elevation (m)", y="Proportion of beta diversity") + 
  scale_fill_manual(name="Beta Component",
                    values=c("Nestedness"="red", "Turnover"="blue"))

ggplot(over.df, aes(x=Latsamp, y=maxDivSF/Stot)) + geom_point() + ylim(0,1)
ggplot(over.df, aes(x=Latsamp, y=maxDivGen/Stot)) + geom_point() 

ggplot(tvars.df, aes(x=Elsamp, y=AvgOfRngObs, fill=Zone)) + 
  geom_ribbon(aes(ymin=AvgOfRngObs-StDevOfRngObs/sqrt(CountOfRngObs), 
                  ymax=AvgOfRngObs+StDevOfRngObs/sqrt(CountOfRngObs)), 
              alpha=0.5) + 
  geom_point() + 
  geom_segment(aes(y=AvgOfRngObs-StDevOfRngObs/sqrt(CountOfRngObs), 
                   yend=AvgOfRngObs+StDevOfRngObs/sqrt(CountOfRngObs),
                   xend=Elsamp)) + 
  geom_rug(sides="l") + 
  facet_wrap(~Label)

ggplot(over.df, aes(x=Zone, y=mnRng, fill=Scale)) + geom_boxplot()
ggplot(over.df, aes(x=Zone, y=mnRng/(MtnPeak-MtnBase), fill=Scale)) + geom_boxplot()
ggplot(over.df, aes(x=abs(Latsamp), y=mnRng, colour=Scale)) + geom_point() + 
  geom_segment(aes(xend=abs(Latsamp), y=mnRng+seRng, yend=mnRng-seRng))
ggplot(over.df, aes(x=abs(Latsamp), y=mnRng/(MtnPeak-MtnBase), colour=Scale)) + 
  geom_point() + geom_segment(aes(xend=abs(Latsamp), 
                                  y=(mnRng+seRng)/(MtnPeak-MtnBase), 
                                  yend=(mnRng-seRng)/(MtnPeak-MtnBase))) +
  facet_wrap(~Scale)

ggplot(spRng.df, aes(x=HighEl-LowEl)) + geom_histogram() + facet_wrap(~Transect)

ggplot(over.df, aes(x=abs(Latsamp), y=pr1el)) + geom_point()
ggplot(over.df, aes(x=Zone, y=pr1el, fill=Scale)) + geom_boxplot()


ggplot(over.df, aes(x=abs(Latsamp), y=sp.sor/Sites)) + geom_point() + ylim(0,.25)
ggplot(over.df, aes(x=abs(Latsamp), y=sp.sne/Sites)) + geom_point() + ylim(0,.25)
ggplot(over.df, aes(x=abs(Latsamp), y=sp.sim/Sites)) + geom_point() + ylim(0,.25)

ggplot(over.df, aes(x=abs(Latsamp), y=gen.sor)) + geom_point() + ylim(0,1)
ggplot(over.df, aes(x=abs(Latsamp), y=gen.sne)) + geom_point() + ylim(0,1)
ggplot(over.df, aes(x=abs(Latsamp), y=gen.sim)) + geom_point() + ylim(0,1)

ggplot(over.df, aes(x=abs(Latsamp), y=sf.sor)) + geom_point() + ylim(0,1)
ggplot(over.df, aes(x=abs(Latsamp), y=sf.sne)) + geom_point() + ylim(0,1)
ggplot(over.df, aes(x=abs(Latsamp), y=sf.sim)) + geom_point() + ylim(0,1)

ggplot(over.df, aes(x=Zone, y=sp.sor/Sites)) + geom_boxplot() + ylim(0,0.2)
ggplot(over.df, aes(x=Zone, y=sp.sne, fill=Scale)) + geom_boxplot() + ylim(0,1)
ggplot(over.df, aes(x=Zone, y=sp.sim, fill=Scale)) + geom_boxplot() + ylim(0,1)

ggplot(over.df, aes(x=Zone, y=gen.sor/Sites)) + geom_boxplot() + ylim(0,0.2)
ggplot(over.df, aes(x=Zone, y=gen.sne, fill=Scale)) + geom_boxplot() + ylim(0,1)
ggplot(over.df, aes(x=Zone, y=gen.sim, fill=Scale)) + geom_boxplot() + ylim(0,1)

ggplot(over.df, aes(x=Zone, y=sf.sor/Sites)) + geom_boxplot() + ylim(0,0.2)
ggplot(over.df, aes(x=Zone, y=sf.sne, fill=Scale)) + geom_boxplot() + ylim(0,1)
ggplot(over.df, aes(x=Zone, y=sf.sim, fill=Scale)) + geom_boxplot() + ylim(0,1)


ggplot(over.df, aes(x=Sites)) + facet_grid(.~Zone) +
  geom_point(aes(y=sp.sor), size=4) + 
  geom_point(aes(y=sp.sne), colour="red", size=4) + 
  geom_point(aes(y=sp.sim), colour="blue", size=4)

ggplot(over.df, aes(x=Zone, y=sp.sim/sp.sor)) + ylim(0,1) +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_boxplot() + geom_point() + 
  ggtitle("Gradient species turnover proportion")
ggplot(over.df, aes(x=Zone, y=gen.sim/gen.sor)) + ylim(0,1) +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_boxplot() + geom_point() + 
  ggtitle("Gradient genus turnover proportion")
ggplot(over.df, aes(x=Zone, y=sf.sim/sf.sor)) + ylim(0,1) +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_boxplot() + geom_point() + 
  ggtitle("Gradient subfamily turnover proportion") 

ggplot(over.df, aes(x=Zone, y=sp.sne/sp.sor)) + ylim(0,1) +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_boxplot() + geom_point() + 
  ggtitle("Gradient species nestedness proportion")
ggplot(over.df, aes(x=Zone, y=gen.sne/gen.sor)) + ylim(0,1) +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_boxplot() + geom_point() + 
  ggtitle("Gradient genus nestedness proportion")
ggplot(over.df, aes(x=Zone, y=sf.sne/sf.sor)) + ylim(0,1) +
  geom_hline(yintercept=0.5, linetype=2) +
  geom_boxplot() + geom_point() + 
  ggtitle("Gradient subfamily nestedness proportion")

ggplot(over.df, aes(x=Zone, y=sp.STB)) + geom_boxplot() + geom_point()
ggplot(over.df, aes(x=Zone, y=gen.STB)) + geom_boxplot() + geom_point()
ggplot(over.df, aes(x=Zone, y=sf.STB)) + geom_boxplot() + geom_point()

ggplot(over.df, aes(x=mnRng, y=sp.STB)) + geom_point()
ggplot(over.df, aes(x=abs(Latsamp), y=sp.STB)) + geom_point() + 
  stat_smooth(method="lm", se=FALSE, colour="black")
ggplot(over.df, aes(x=abs(Latsamp), y=gen.STB)) + geom_point()
ggplot(over.df, aes(x=abs(Latsamp), y=sf.STB)) + geom_point()

ggplot(over.df, aes(x=abs(Latsamp), y=genPred.r, colour=Scale)) + geom_point(size=4) + ylim(0,1)
ggplot(over.df, aes(x=abs(Latsamp), y=sfPred.r, colour=Scale)) + geom_point(size=4) + ylim(0,1)

ggplot(over.df, aes(x=Zone, y=genPred.r, fill=Scale)) + geom_boxplot()
ggplot(over.df, aes(x=Sites, y=genPred.r, colour=Zone)) + geom_point()

ggplot(over.df, aes(x=Zone, y=sfPred.r, fill=Scale)) + geom_boxplot()
ggplot(over.df, aes(x=Sites, y=sfPred.r, colour=Zone)) + geom_point()

ggplot(betaTax.df, aes(x=TaxLevel, y=Turnover/TotalBeta, fill=Zone)) +
  geom_boxplot()
