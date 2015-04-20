# Statistical analyses and summary statistics
# Tim Szewczyk
# Created 2015 April 17


#######
## Load libraries, functions, data
#######

library(ggplot2); theme_set(theme_bw()); library(grid); library(lme4)
library(xlsx); library(plyr); library(vegan); library(betapart)
source("R_scripts/FuncsGen.R")
loadAll()

  #--- graphical parameters ---#
  w <- 6  # width of pdf (inches)
  h <- 5  # height of pdf (inches)

  theme_is <- theme(axis.title.x=element_text(size=rel(1.5), vjust=-0.3),
                    axis.text.x=element_text(size=rel(1.5)),
                    axis.title.y=element_text(size=rel(1.5), vjust=1.5),
                    axis.text.y=element_text(size=rel(1.5)),
                    legend.title=element_text(size=rel(1.5)),
                    legend.key.size=unit(1, "cm"),
                    legend.text=element_text(size=rel(1.5))) 


#########
## Figure 1
#########

  #--- mean and median range size by latitude ---#
  pdf("ms/pubFigs/Figure1a.pdf", width=w*1.25, height=h)
    ggplot(over.df, aes(x=abs(Latsamp))) + 
      theme_is +
      geom_point(aes(y=sp.mnRng, shape="Mean"), size=4) + 
      geom_segment(aes(xend=abs(Latsamp), 
                       y=sp.mnRng+seRng, 
                       yend=sp.mnRng-seRng), size=1,
                   arrow=arrow(angle=90, length=unit(0.2, "cm"), ends="both")) +
      stat_smooth(aes(y=sp.mnRng), se=FALSE, method="lm", 
                  colour="black", size=1) +
      geom_point(aes(y=sp.medRng, shape="Median"), size=4) +
      stat_smooth(aes(y=sp.medRng), se=FALSE, method="lm", 
                  colour="black", size=1, linetype=2) +
      scale_shape_manual(name="", values=c("Mean"=19,
                                           "Median"=1)) + 
      labs(x="Degrees from equator", y="Elevational range (m)")
  dev.off()

  #--- mean range size on truncated mountains by latitude ---#
  pdf("ms/pubFigs/Figure1b.pdf", width=w*1.25, height=h)
    ggplot(over.df, aes(x=abs(Latsamp))) + 
      theme_is +
      geom_point(aes(y=sp.mnRng.2000, colour="2000m"), size=4) + 
      geom_segment(aes(xend=abs(Latsamp), 
                       y=sp.mnRng.2000+sp.seRng.2000, 
                       yend=sp.mnRng.2000-sp.seRng.2000,
                       colour="2000m"), size=1,
                   arrow=arrow(angle=90, length=unit(0.2, "cm"), ends="both")) +
      stat_smooth(aes(y=sp.mnRng.2000, colour="2000m"), 
                  se=FALSE, method="lm", size=1) +
      geom_point(aes(y=sp.mnRng.1800, colour="1800m"), size=4) + 
      geom_segment(aes(xend=abs(Latsamp), 
                       y=sp.mnRng.1800+sp.seRng.1800, 
                       yend=sp.mnRng.1800-sp.seRng.1800, 
                       colour="1800m"), size=1,
                   arrow=arrow(angle=90, length=unit(0.2, "cm"), ends="both")) +
      stat_smooth(aes(y=sp.mnRng.1800, colour="1800m"), 
                  se=FALSE, method="lm", size=1) +
      geom_point(aes(y=sp.mnRng.1600, colour="1600m"), size=4) + 
      geom_segment(aes(xend=abs(Latsamp), 
                       y=sp.mnRng.1600+sp.seRng.1600, 
                       yend=sp.mnRng.1600-sp.seRng.1600, 
                       colour="1600m"), size=1,
                   arrow=arrow(angle=90, length=unit(0.2, "cm"), ends="both")) +
      stat_smooth(aes(y=sp.mnRng.1600, colour="1600m"), 
                  se=FALSE, method="lm", size=1) +
      scale_colour_manual(name="Gradient \nTruncation",
                          values=c("2000m"="gray20",
                                   "1800m"="gray50",
                                   "1600m"="gray80")) +
      labs(x="Degrees from equator", y="Elevational range (m)")
  dev.off()

#########
## Figure 2
#########

  #--- sp.STB vs latitude ---#
  pdf("ms/pubFigs/Figure2a.pdf", width=w, height=h)
    ggplot(over.df, aes(x=abs(Latsamp), y=sp.STB)) + 
      theme_is +
      geom_point(size=4) +
      stat_smooth(se=FALSE, method="lm", colour="black", size=1) +
      labs(x="Degrees from equator", 
           y=expression(paste(beta[' st'])))
  dev.off()

  #--- turnover proportion by taxonomy and zone ---#
  pdf("ms/pubFigs/Figure2b.pdf", width=w*1.25, height=h)
    ggplot(betaTax.df, aes(x=TaxLevel, y=Turnover/TotalBeta, fill=Zone)) +
      ylim(0,1) + theme_is + 
      theme(axis.text.x=element_text(size=rel(2))) +
      geom_boxplot() + 
      scale_fill_manual(name="", values=c("white", "gray70")) +
      labs(x="", y=expression(paste('Proportion of ', beta,' due to turnover')))
  dev.off()


#########
## Figure 3 
#########

  #--- dominant genus predicting rest ---#
  pdf("ms/pubFigs/Figure3a.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=SmaxDivGen, y=S-SmaxDivGen)) +
      theme_is +
      stat_smooth(aes(group=Label), se=F, method="lm", 
                  colour="gray", size=1) +
      stat_smooth(se=F, method="lm", colour="black", size=1.5) +  
      geom_point(size=3) +
      labs(x="Richness of most speciose genus", 
           y=expression("Richness of remaining genera"))
  dev.off()

  #--- dominant genus predicting rest ---#
  pdf("ms/pubFigs/Figure3b.pdf", width=w, height=h)
    ggplot(tvars.df, aes(x=SmaxDivSF, y=S-SmaxDivSF)) +
      theme_is +
      stat_smooth(aes(group=Label), se=F, method="lm", 
                  colour="gray", size=1) +
      stat_smooth(se=F, method="lm", colour="black", size=1.5) +  
      geom_point(size=3) +
      labs(x="Richness of most speciose subfamily", 
           y=expression("Richness of remaining subfamilies"))
  dev.off()


#########
## Figure 4
#########

  #--- richness patterns of each taxonomic level ---#
