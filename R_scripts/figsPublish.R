# Statistical analyses and summary statistics
# Tim Szewczyk
# Created 2015 April 17


#######
## Load libraries, functions, data
#######

library(ggplot2); theme_set(theme_bw()); library(grid); library(gridExtra)
library(xlsx); library(plyr); library(vegan); library(betapart); library(lme4)
source("R_scripts/FuncsGen.R")
loadAll()

  #--- graphical parameters ---#
  w <- 6  # width of png (inches)
  h <- 5  # height of png (inches)

  theme_is <- theme(axis.title.x=element_text(size=20, vjust=-0.3),
                    axis.text.x=element_text(size=20),
                    axis.title.y=element_text(size=20, vjust=1.5),
                    axis.text.y=element_text(size=20),
                    legend.title=element_text(size=20, vjust=0.3),
                    legend.key=element_rect(colour=NA),
                    legend.key.size=unit(0.75, "cm"),
                    legend.text=element_text(size=20),
                    panel.grid=element_blank()) 


#########
## Figure 1
#########

  #--- 1a. mean and median range size by latitude ---#
  f1a <- ggplot(over.df, aes(x=abs(Latsamp))) + 
    theme_is + ylim(0,1100) +
    geom_point(aes(y=sp.mnRng, shape="Mean"), size=5) + 
    geom_segment(aes(xend=abs(Latsamp), 
                     y=sp.mnRng+seRng, 
                     yend=sp.mnRng-seRng), size=1,
                 arrow=arrow(angle=90, length=unit(0.2, "cm"), ends="both")) +
    stat_smooth(aes(y=sp.mnRng), se=FALSE, method="lm", 
                colour="black", size=1) +
    geom_point(aes(y=sp.medRng, shape="Median"), size=5) +
    stat_smooth(aes(y=sp.medRng), se=FALSE, method="lm", 
                colour="black", size=1, linetype=2) +
    annotate("text", x=12, y=1000, label="a.", size=10) + 
    scale_shape_manual(name="", values=c("Mean"=19,
                                         "Median"=1)) + 
    labs(x="Degrees from equator", y="Elevational range (m)")

  #--- 1b. mean range size on truncated mountains by latitude ---#
  f1b <- ggplot(over.df, aes(x=abs(Latsamp))) + 
    theme_is + ylim(0,1100) +
    stat_smooth(aes(y=sp.mnRng.2000, colour="2000m"), 
                se=FALSE, method="lm", size=1.5) +
    stat_smooth(aes(y=sp.mnRng.1800, colour="1800m"),
                se=FALSE, method="lm", size=1.5) +
    stat_smooth(aes(y=sp.mnRng.1600, colour="1600m"), 
                se=FALSE, method="lm", size=1.5) +
    geom_segment(aes(xend=abs(Latsamp), 
                     y=sp.mnRng.2000+sp.seRng.2000, 
                     yend=sp.mnRng.2000-sp.seRng.2000), 
                 colour="black", size=1,
                 arrow=arrow(angle=90, length=unit(0.22, "cm"), ends="both")) +
    geom_segment(aes(xend=abs(Latsamp), 
                     y=sp.mnRng.1800+sp.seRng.1800, 
                     yend=sp.mnRng.1800-sp.seRng.1800), 
                 colour="gray40", size=1,
                 arrow=arrow(angle=90, length=unit(0.22, "cm"), ends="both")) +
    geom_segment(aes(xend=abs(Latsamp), 
                     y=sp.mnRng.1600+sp.seRng.1600, 
                     yend=sp.mnRng.1600-sp.seRng.1600), 
                 colour="gray70", size=1,
                 arrow=arrow(angle=90, length=unit(0.22, "cm"), ends="both")) +
    geom_point(aes(y=sp.mnRng.2000, colour="2000m"), size=5) + 
    geom_point(aes(y=sp.mnRng.1800, colour="1800m"), size=5) + 
    geom_point(aes(y=sp.mnRng.1600, colour="1600m"), size=5) + 
    annotate("text", x=12, y=1000, label="b.", size=10) + 
    scale_colour_manual(name="Gradient \nTruncation",
                      values=c("2000m"="black",
                               "1800m"="gray45",
                               "1600m"="gray70")) +
    labs(x="Degrees from equator", y="Elevational range (m)")


  #--- save figure ---#
  fig1 <- arrangeGrob(f1a, f1b, ncol=2)
  ggsave("ms/pubFigs/Fig1.png", fig1, width=w*2.5, height=h, dpi=500)

#########
## Figure 2
#########

  #--- sp.STB vs latitude ---#
  fig2 <- ggplot(over.df, aes(x=abs(Latsamp))) + 
    theme_is +
    stat_smooth(aes(y=sp.STB), se=FALSE, 
                method="lm", size=1.5, colour="black") +
    stat_smooth(aes(y=gen.STB), se=FALSE, 
                method="lm", size=1.5, colour="black") +
    stat_smooth(aes(y=sf.STB), se=FALSE, 
                method="lm", size=1.5, colour="black") +
    stat_smooth(aes(y=sp.STB), se=FALSE, 
                method="lm", size=1, colour="black") +
    stat_smooth(aes(y=gen.STB), se=FALSE, 
                method="lm", size=1, colour="gray70") +
    stat_smooth(aes(y=sf.STB), se=FALSE, 
                method="lm", size=1, colour="white") +
    geom_point(aes(y=sp.STB, fill="Species"), size=4, pch=21) +
    geom_point(aes(y=gen.STB, fill="Genus"), size=4, pch=21) +
    geom_point(aes(y=sf.STB, fill="Subfamily"), size=4, pch=21) +
    scale_fill_manual(name="Taxonomic \nLevel",
                        values=c("Species"="black",
                                 "Genus"="gray70",
                                 "Subfamily"="white")) +
    labs(x="Degrees from equator", 
         y=expression(paste(beta[' st'])))
  
  #--- save figure ---#
  ggsave("ms/pubFigs/Fig2.png", fig2, width=w*1.25, height=h, dpi=500)


#########
## Figure 3  --- GET RID OF GRIDLINES
#########

  #--- turnover proportion by taxonomy and zone ---#
  fig3 <- ggplot(betaTax.df, aes(x=TaxLevel, y=Turnover/TotalBeta, fill=Zone)) +
    ylim(0,1) + theme_is + 
    theme(axis.text.x=element_text(size=rel(2))) +
    geom_hline(yintercept=0.5, linetype=2, colour="gray40") +
    geom_boxplot() + 
    annotate("text", label="Higher \nturnover", x=3.15, y=0.56, 
             angle=90, hjust=0) +
    annotate("text", label="Higher \nnestedness", x=3.15, y=0.47, 
             angle=90, hjust=1.1) +
    annotate("segment", x=3.46, xend=3.46, y=0.55, yend=0.9,
             arrow=arrow(angle=35, length=unit(0.22, "cm"), ends="last")) + 
    annotate("segment", x=3.46, xend=3.46, y=0.45, yend=0.1,
             arrow=arrow(angle=35, length=unit(0.22, "cm"), ends="last")) + 
    scale_fill_manual(name="", values=c("white", "gray70")) +
    labs(x="", y=expression(paste(beta,' proportion due to turnover')))
  
  
  #--- save figure ---#
  ggsave("ms/pubFigs/Fig3.png", fig3, width=w*1.25, height=h, dpi=500)


#########
## Figure 4
#########

  #--- richness patterns of each taxonomic level ---#
  fig4 <- ggplot(patt.barSUM, aes(x=Pattern, fill=Tax, y=num)) + 
    theme_is +
    geom_bar(stat="identity", position="dodge", colour="black") +
    scale_fill_manual(name="Taxonomic \nLevel", 
                      values=c("gray10", "gray70", "white")) +
    labs(x="Richness Pattern", y="Number of gradients")

  #--- save figure ---#
  ggsave("ms/pubFigs/Fig4.png", fig4, width=w*1.25, height=h, dpi=500)


#########
## Figure 5 
#########

  #--- dominant genus predicting rest ---#
  f5a <- ggplot(tvars.df, aes(x=SmaxDivGen, y=S-SmaxDivGen)) +
    theme_is +
    stat_smooth(aes(group=Label), se=F, method="lm", 
                colour="gray", size=1) +
    stat_smooth(se=F, method="lm", colour="black", size=1.5) +  
    geom_point(size=3) +
    annotate("text", x=5, y=190, label="a.", size=10) + 
    labs(x="Richness of most speciose genus", 
         y=expression("Richness of remaining genera"))

  #--- dominant genus predicting rest ---#
  f5b <- ggplot(tvars.df, aes(x=SmaxDivSF, y=S-SmaxDivSF)) +
    theme_is +
    stat_smooth(aes(group=Label), se=F, method="lm", 
                colour="gray", size=1) +
    stat_smooth(se=F, method="lm", colour="black", size=1.5) +  
    geom_point(size=3) +
    annotate("text", x=18, y=95, label="b.", size=10) + 
    labs(x="Richness of most speciose subfamily", 
         y=expression("Richness of remaining subfamilies"))

  #--- save figure ---#
  fig5 <- arrangeGrob(f5a, f5b, ncol=2)
  ggsave("ms/pubFigs/Fig5.png", fig5, width=w*2, height=h, dpi=500)

