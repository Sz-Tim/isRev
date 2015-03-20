# Exploring subfamily patterns
# Tim Szewczyk
# Created 2015 Feb 21

# This script explores elevational patterns of ant diversity at the subfamily
# level across the globe. This includes:
# - How the number of subfamilies changes with elevation
# - How the number of species within each subfamily changes with elevation



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
  tvars.df <- droplevels(ivars.df[ivars.df$Transect %in% Transects, ])



##########
## Species diversity within each subfamily
##########

  ###--- Count number of species by subfamily ---###

  # Column for number of species in each subfamily
  sfToCount <- levels(spRng.df$Subfamily)
  newCols <- (length(ivars.df)+1):(length(ivars.df)+length(sfToCount))
  ivars.df[, newCols] <- NA
  names(ivars.df)[newCols] <- sfToCount

  for(tr in 1:nTrans) {
    
    # Subset each transect
    rngTran <- droplevels(subset(spRng.df, spRng.df$Transect==Transects[tr]))
    tranRows <- which(ivars.df$Transect==Transects[tr]) 
    
    for(sf in 1:nlevels(spRng.df$Subfamily)) {
      subfam <- levels(spRng.df$Subfamily)[sf]

      # Subset each subfamily
      rngSF <- droplevels(subset(rngTran, rngTran$Subfamily==subfam))
      
      # Count number of species at each elevation
      ivars.df[tranRows, subfam] <- spp.band(range.df=rngSF, 
                                     els=ivars.df$Elband[tranRows])
    }
  }
  # Store updated dataframe
  write.xlsx(ivars.df, file="Sheets/intVars.xlsx")


  ###--- Visualize diversity patterns ---###

  # Informative
  ggplot(ivars.df, aes(x=Elsamp, y=Amblyoponinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Cerapachyinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Dolichoderinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Ecitoninae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Formicinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Myrmicinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Ponerinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Proceratiinae)) + geom_line() + 
    facet_wrap(~Transect)

  # Too rare to be informative
  ggplot(ivars.df, aes(x=Elsamp, y=Agroecomyrmecinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Ectatomminae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Heteroponerinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Myrmeciinae)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Pseudomyrmecinae)) + geom_line() + 
    facet_wrap(~Transect)



#######
## Does the most diverse subfamily drive species richness patterns?
#######

  ###--- Determine most diverse subfamily ---###

  over.df$maxDivSF <- NA
  over.df$mostDivSF <- NA
  ivars.df$SmaxDivSF <- NA
  ivars.df$mostDivSF <- NA
  for(tr in 1:nTrans) {

    # Subset each transect
    rngTran <- droplevels(subset(spRng.df, spRng.df$Transect==Transects[tr]))
    maxS <- max(summary(rngTran$Subfamily))
    maxsf <- names(which.max(summary(rngTran$Subfamily)))
    
    # Store in over.df
    over.df[over.df$Transect==Transects[tr], "maxDivSF"] <- maxS
    over.df[over.df$Transect==Transects[tr], "mostDivSF"] <- maxsf
    
    # Store in ivars.df
    iTrRows <- ivars.df$Transect==Transects[tr]
    ivars.df[iTrRows, "mostDivSF"] <- maxsf
    ivars.df[iTrRows, "SmaxDivSF"] <- ivars.df[iTrRows, maxsf]
  }
  write.xlsx(over.df, file="Sheets/datasetOverview.xlsx")
  write.xlsx(ivars.df, file="Sheets/intVars.xlsx")


  ###--- Visualize ---###
  mean(over.df$maxDivSF/over.df$Stot, na.rm=TRUE) - 
    2*se(over.df$maxDivSF/over.df$Stot)
  mean(over.df$maxDivSF/over.df$Stot, na.rm=TRUE)
  mean(over.df$maxDivSF/over.df$Stot, na.rm=TRUE) + 
    2*se(over.df$maxDivSF/over.df$Stot) 
  ggplot(over.df, aes(x=maxDivSF/Stot)) + geom_density() + xlim(0,1)



#############
## Proportion of species in each subfamily
#############

  sf.bars <- tvars.df[,c(1:3, 46:59)]
  sf.bars <- reshape(sf.bars, 
                     varying=as.character(sfToCount),
                     v.names="SFnumSpp",
                     timevar="Subfamily",
                     times=as.character(sfToCount),
                     direction="long")
  rownames(sf.bars) <- NULL
  sf.bars <- sf.bars[!is.na(sf.bars$Elsamp),]
  write.csv(sf.bars, file="Sheets/relDiversity_sf.csv")
