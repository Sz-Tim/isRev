# General functions for Insectes Sociaux review
# Tim Szewczyk
# Created: 2015 Feb 21


# load all dataframes
loadAll <- function() {
  
  require(plyr)
  
  #--- data sources ---#
  spRng.df <<- read.xlsx("Sheets/ranges_spp.xlsx", 1)  # Species ranges
  genRng.df <<- read.xlsx("Sheets/ranges_gen.xlsx", 1)  # Genus ranges
  sfRng.df <<- read.xlsx("Sheets/ranges_sf.xlsx", 1)  # Subfam ranges
  spSor.df <<- read.csv("Sheets/spp_Sorensen.csv") # Species beta diversity
  genSor.df <<- read.csv("Sheets/gen_Sorensen.csv") # Genus beta diversity
  sfSor.df <<- read.csv("Sheets/sf_Sorensen.csv") # Subfam beta diversity
  spSorAdj.df <<- read.csv("Sheets/spp_SorAdjSites.csv") # Spp beta adjacent
  genSorAdj.df <<- read.csv("Sheets/gen_SorAdjSites.csv") # Genus beta adjacent
  sfSorAdj.df <<- read.csv("Sheets/sf_SorAdjSites.csv") # Subfam beta adjacent
  over.df <<- read.xlsx("Sheets/datasetOverview.xlsx", 1)  # Dataset summaries
  ivars.df <<- read.xlsx("Sheets/intVars.xlsx", 1)  # Elev's sampled, env var's
  gen.bars <<- read.csv("Sheets/relDiversity_gen.csv")  # Richness by genus
  sf.bars <<- read.csv("Sheets/relDiversity_sf.csv")  # Richness by sf
  traits.df <<- read.csv("Sheets/occ_traits.txt") # Genus level traits
  nestSum.df <<- ddply(traits.df, .(Transects, Elsamp, NestingSite), summarize,
                      nSpp=length(unique(Binomial)))
  feedSum.df <<- ddply(traits.df, .(Transects, Elsamp, Specialization),
                      summarize, nSpp=length(unique(Binomial)))
  taxcomp.df <- data.frame(spSorAdj.df[,2:4])
  taxcomp.df$SpG.sim <- spSorAdj.df$sim - genSorAdj.df$sim
  taxcomp.df$SpG.sne <- spSorAdj.df$sne - genSorAdj.df$sne
  taxcomp.df$SpG.sor <- spSorAdj.df$sor - genSorAdj.df$sor
  
  #--- useful summary objects ---#
  Transects <<- levels(spRng.df$Transect)  # Transects w/species range data
  nTrans <<- nlevels(spRng.df$Transect)
  tvars.df <<- droplevels(ivars.df[ivars.df$Transect %in% Transects, ])
  Labels <<- as.character(unique(tvars.df$Label))
}


# Standard error of the mean
se <- function(x) { 
  if(length(x) > 1) {
    SE <- sd(x, na.rm=TRUE)/sqrt(length(x)) 
  } else { SE <- 0 }
  return(SE)
}

# Counting the number of species in each elevational band
spp.band <- function(range.df, els) {
  
  spp.count <- rep(NA, length(els))
  for(i in 1:length(els)) {
    sp.pres <- subset(range.df, ( (range.df$LowEl < (els[i] + 100)) & 
                                    (range.df$HighEl >= els[i]) ))
    sp.pres <- droplevels(sp.pres)
    spp.count[i] <- nlevels(sp.pres$Binomial)
  }
  return(spp.count)
}	


# Counting the number of genera in each elevational band
gen.band <- function(range.df, els) {
  
  gen.count <- rep(NA, length(els))
  for(i in 1:length(els)) {
    sp.pres <- subset(range.df, ( (range.df$LowEl < (els[i] + 100)) & 
                                    (range.df$HighEl >= els[i]) ))
    sp.pres <- droplevels(sp.pres)
    gen.count[i] <- nlevels(sp.pres$Genus)
  }
  return(gen.count)
}

# Counting the number of subfamilies in each elevational band
sf.band <- function(range.df, els) {
  
  sf.count <- rep(NA, length(els))
  for(i in 1:length(els)) {
    sp.pres <- subset(range.df, ( (range.df$LowEl < (els[i] + 100)) & 
                                    (range.df$HighEl >= els[i]) ))
    sp.pres <- droplevels(sp.pres)
    sf.count[i] <- nlevels(sp.pres$Subfamily)
  }
  return(sf.count)
}

# Making a row for each species occurrence at each elevation
band.bar <- function(range.df, els) {
  
  trans <- NULL
  elevations <- NULL
  subf <- NULL
  gen <- NULL
  sp <- NULL
  rng <- NULL
  el.lo <- NULL
  el.hi <- NULL
  
  for(i in 1:length(els)) {
    sp.pres <- subset(range.df, ( (range.df$LowEl < (els[i] + 100)) & 
                                    (range.df$HighEl >= els[i]) ))
    sp.pres <- droplevels(sp.pres)
    trans <- c(trans, as.character(sp.pres$Transect))
    elevations <- c(elevations, rep(els[i], length(sp.pres$Transect)))
    subf <- c(subf, as.character(sp.pres$Subfamily))
    gen <- c(gen, as.character(sp.pres$Genus))
    sp <- c(sp, as.character(sp.pres$Binomial))
    rng <- c(rng, (sp.pres$HighEl - sp.pres$LowEl))
    el.lo <- c(el.lo, sp.pres$LowEl)
    el.hi <- c(el.hi, sp.pres$HighEl)
  }
  stud <- data.frame(trans, elevations, subf, gen, sp, rng, el.lo, el.hi)
  return(stud)
}


