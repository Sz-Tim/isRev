# Exploring genus patterns
# Tim Szewczyk
# Created 2015 Feb 22

# This script explores elevational patterns of ant diversity at the genus
# level across the globe. This includes:
# - How the number of genera changes with elevation
# - How the number of species within each genus changes with elevation



#######
## Load libraries, functions, data
#######

  library(ggplot2); theme_set(theme_bw()); library(xlsx); library(plyr)
  source("R_scripts/FuncsGen.R")
  spRng.df <- read.xlsx("Sheets/ranges_spp.xlsx", 1)  # Species ranges
  over.df <- read.xlsx("Sheets/datasetOverview.xlsx", 1)  # Dataset summaries
  ivars.df <- read.xlsx("Sheets/intVars.xlsx", 1)  # Elev's sampled, env var's
  Transects <- levels(spRng.df$Transect)  # Transects w/species range data
  nTrans <- nlevels(spRng.df$Transect)



#########
## Number of genera by elevation
#########

  ggplot(ivars.df, aes(x=Elsamp, y=Gen)) + geom_line() + facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Gen/GenTot)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Gen)) + geom_point() + facet_grid(.~Climate)


##########
## Species diversity within each genus
##########

  ###--- Count number of species by genus ---###
  genCt.df <- ddply(spRng.df, .(Subfamily, Genus), summarize, 
                    nSpp=length(unique(Binomial)))
  genCt.df <- genCt.df[order(genCt.df$nSpp, decreasing=TRUE),]

  # Column for number of species in each genus
  genToCount <- droplevels(genCt.df$Genus[1:20])
  newCols <- (length(ivars.df)+1):(length(ivars.df)+nlevels(genToCount))
  ivars.df[, newCols] <- NA
  names(ivars.df)[newCols] <- levels(genToCount)
 
  for(tr in 1:nTrans) {
  
    # Subset each transect
    rngTran <- droplevels(subset(spRng.df, spRng.df$Transect==Transects[tr]))
    tranRows <- which(ivars.df$Transect==Transects[tr]) 
    
    for(gen in 1:nlevels(genToCount)) {
      genus <- levels(genToCount)[gen]
      
      # Subset each genus
      if(genus %in% levels(rngTran$Genus)) {
        rngGen <- droplevels(subset(rngTran, rngTran$Genus==genus))
      
        # Count number of species at each elevation
        ivars.df[tranRows, genus] <- spp.band(range.df=rngGen, 
                                              els=ivars.df$Elband[tranRows])
      } else {
        ivars.df[tranRows, genus] <- rep(0, length(tranRows))
      }
    }
  }
  # Store updated dataframe
  
  
  ###--- Visualize diversity patterns ---###




#######
## Does the most diverse genus drive species richness patterns?
#######

  ###--- Determine most diverse genus ---###

  over.df$maxDivGen <- NA
  over.df$mostDivGen <- NA
  ivars.df$SmaxDivGen <- NA
  ivars.df$mostDivGen <- NA
  for(tr in 1:nTrans) {
    
    # Subset each transect
    rngTran <- droplevels(subset(spRng.df, spRng.df$Transect==Transects[tr]))
    maxS <- max(summary(rngTran$Genus))
    maxgen <- names(which.max(summary(rngTran$Genus)))
    
    # Store in over.df
    over.df[over.df$Transect==Transects[tr], "maxDivGen"] <- maxS
    over.df[over.df$Transect==Transects[tr], "mostDivGen"] <- maxgen
    
    # Store in ivars.df
    iTrRows <- ivars.df$Transect==Transects[tr]
    ivars.df[iTrRows, "mostDivGen"] <- maxgen
    ivars.df[iTrRows, "SmaxDivGen"] <- ivars.df[iTrRows, maxgen]
  }
  write.xlsx(over.df, file="Sheets/datasetOverview.xlsx")
  write.xlsx(ivars.df, file="Sheets/intVars.xlsx")
  
  
  ###--- Visualize ---###
  mean(over.df$maxDivGen/over.df$Stot, na.rm=TRUE) - 
    2*se(over.df$maxDivGen/over.df$Stot)
  mean(over.df$maxDivGen/over.df$Stot, na.rm=TRUE)
  mean(over.df$maxDivGen/over.df$Stot, na.rm=TRUE) + 
    2*se(over.df$maxDivGen/over.df$Stot) 
  ggplot(over.df, aes(x=maxDivGen/Stot)) + geom_density() + xlim(0,1)
  
  ggplot(ivars.df, aes(x=Elsamp)) + facet_wrap(~Transect) +
    geom_line(aes(y=S, colour="Total diversity")) + 
    geom_line(aes(y=S-SmaxDivGen, colour="All but most diverse gn")) +
    geom_line(aes(y=SmaxDivGen, colour="Most diverse gn")) +
    scale_colour_manual(name="", values=c('Total diversity'='black',
                                          'Most diverse gn'='green',
                                          'All but most diverse gn'='blue'))
  ggplot(ivars.df, aes(x=SmaxDivGen, y=S-SmaxDivGen, colour=Elband)) +
    geom_point() + facet_wrap(~Transect)

  summary(lm(ivars.df$S ~ ivars.df$Myrmicinae))
  summary(lm(ivars.df$S ~ ivars.df$Formicinae))
  summary(lm(ivars.df$S-ivars.df$SmaxDivGen ~ ivars.df$SmaxDivGen))
  plot(ivars.df$S ~ ivars.df$Myrmicinae)
  plot(ivars.df$S ~ ivars.df$Formicinae)
  plot(ivars.df$S-ivars.df$SmaxDivGen ~ ivars.df$SmaxDivGen)
  