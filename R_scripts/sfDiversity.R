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


#########
## Number of subfamilies by elevation
#########

  ggplot(ivars.df, aes(x=Elsamp, y=SF)) + geom_line() + facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=SF/SFTot)) + geom_line() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=SF)) + geom_point() + facet_grid(.~Climate)


##########
## Species diversity within each subfamily
##########

  ###--- Count number of species by subfamily ---###

  # Column for number of species in each subfamily
  ivars.df$Agroecomyrmecinae <- rep(NA, nrow(ivars.df))
  ivars.df$Amblyoponinae <- rep(NA, nrow(ivars.df))
  ivars.df$Cerapachyinae <- rep(NA, nrow(ivars.df))
  ivars.df$Dolichoderinae <- rep(NA, nrow(ivars.df))
  ivars.df$Ecitoninae <- rep(NA, nrow(ivars.df))
  ivars.df$Ectatomminae <- rep(NA, nrow(ivars.df))
  ivars.df$Formicinae <- rep(NA, nrow(ivars.df))
  ivars.df$Heteroponerinae <- rep(NA, nrow(ivars.df))
  ivars.df$Myrmeciinae <- rep(NA, nrow(ivars.df))
  ivars.df$Myrmicinae <- rep(NA, nrow(ivars.df))
  ivars.df$Ponerinae <- rep(NA, nrow(ivars.df))
  ivars.df$Proceratiinae <- rep(NA, nrow(ivars.df))
  ivars.df$Pseudomyrmecinae <- rep(NA, nrow(ivars.df))
  ivars.df$Unknown <- rep(NA, nrow(ivars.df))

  for(tr in 1:nTrans) {
    
    # Subset each transect
    rngTran <- droplevels(subset(spRng.df, spRng.df$Transect==Transects[tr]))
    tranRows <- which(ivars.df$Transect==Transects[tr]) 
    
    for(sf in 1:nlevels(spRng.df$Subfamily)) {
      subfam <- levels(spRng.df$Subfamily)[sf]

      # Subset each subfamily
      rngSF <- droplevels(subset(rngTran, rngTran$Subfamily==subfam))
      
      # Count number of species at each elevation
      ivars.df[tranRows, 45+sf] <- spp.band(range.df=rngSF, 
                                     els=ivars.df$Elband[tranRows])
    }
  }

  # Store updated dataframe
  write.xlsx(ivars.df, file="Sheets/intVars.xlsx")

  ###--- Visualize diversity patterns ---###

  ggplot(ivars.df, aes(x=Elsamp, y=Agroecomyrmecinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Amblyoponinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Cerapachyinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Dolichoderinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Ecitoninae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Ectatomminae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Formicinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Heteroponerinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Myrmeciinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Myrmicinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Ponerinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Proceratiinae)) + geom_point() + 
    facet_wrap(~Transect)
  ggplot(ivars.df, aes(x=Elsamp, y=Pseudomyrmecinae)) + geom_point() + 
    facet_wrap(~Transect)



#######
## Does the most diverse subfamily drive species richness patterns?
#######