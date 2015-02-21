# General functions for Insectes Sociaux review
# Tim Szewczyk
# Created: 2015 Feb 21



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


