# script to derive region with combined impact

# load balticgrid_state
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")
  baltdat <- as.data.frame(balticgrid)
  baltdat$state[is.na(baltdat$state) & !(is.na(baltdat$depth))] <- 1  
  baltdat$surface_SAR[is.na(baltdat$surface_SAR) & !(is.na(baltdat$depth))] <- 0  
 
# first cut the edge 
  baltdat2 <- data.frame(longitude = coordinates(balticgrid)[,1], latitude = coordinates(balticgrid)[,2])
  baltdat<-cbind(baltdat,baltdat2)
  baltdat$knip<- rep(1,nrow(baltdat))
  baltdat$knip[baltdat$longitude < 10.3 & baltdat$latitude > 57.3]<-NA
  baltdat$knip[baltdat$longitude < 10 & baltdat$latitude > 56.5]<-NA
  baltdat$knip[baltdat$longitude > 10 & baltdat$longitude<12 & baltdat$latitude > 57.8]<-NA
  baltdat<-subset(baltdat,!(is.na(baltdat$knip)))  

# area with combined effects
  baltcomb <- subset(baltdat,baltdat$surface_SAR > 0.1 & baltdat$min_oxygen < 3.2)
  baltcomb <- subset(baltcomb,baltcomb$state < 0.9)
  regcomb <- sum(baltcomb$area_sqkm)

  # area with fishing only effects
  baltfish <- subset(baltdat,baltdat$surface_SAR > 0.1 & baltdat$min_oxygen >= 3.2)
  baltfish <- subset(baltfish,baltfish$state < 0.9)
  regfish <- sum(baltfish$area_sqkm)
  
  # area with fishing only effects
  baltox <- subset(baltdat,baltdat$surface_SAR < 0.1 & baltdat$min_oxygen < 3.2)
  baltox <- subset(baltox,baltox$state < 0.9)
  regox <- sum(baltox$area_sqkm)
  
# whole area
  regw <- sum(baltdat$area_sqkm[!(is.na(baltdat$state))])
  c(regfish/regw, regox/regw, regcomb/regw) 
 trel <- sum(regfish,regox,regcomb)
   c(regfish/trel, regox/trel,regcomb/trel)
   c(mean(baltfish$state),mean(baltox$state),mean(baltcomb$state))
   
  