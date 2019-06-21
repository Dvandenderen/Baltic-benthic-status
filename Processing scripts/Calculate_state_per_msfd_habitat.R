# script to derive state per habitat type

# load balticgrid_state
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")

  baltdat <- as.data.frame(balticgrid)
  baltdat$state[is.na(baltdat$state) & !(is.na(baltdat$depth))] <- 1  
  baltdat$state_susp[is.na(baltdat$state_susp) & !(is.na(baltdat$depth))] <- 1 
  baltdat$state_biot[is.na(baltdat$state_biot) & !(is.na(baltdat$depth))] <- 1 

# calculate for the whole baltic region
  
# first cut the edge 
  baltdat2 <- data.frame(longitude = coordinates(balticgrid)[,1], latitude = coordinates(balticgrid)[,2])
  baltdat<-cbind(baltdat,baltdat2)
  baltdat$knip<- rep(1,nrow(baltdat))
  baltdat$knip[baltdat$longitude < 10.3 & baltdat$latitude > 57.3]<-NA
  baltdat$knip[baltdat$longitude < 10 & baltdat$latitude > 56.5]<-NA
  baltdat$knip[baltdat$longitude > 10 & baltdat$longitude<12 & baltdat$latitude > 57.8]<-NA
  baltdat<-subset(baltdat,!(is.na(baltdat$knip)))  
  
# now get average state
  whole <- c(mean(baltdat$state,na.rm=T),mean(baltdat$state_susp, na.rm=T),mean(baltdat$state_biot,na.rm=T))
  whole <- data.frame("whole region",whole[1],whole[2],whole[3],1,1)
  colnames(whole) <- c("msfd","state","susp_state","biot_state","fraction area","fraction_2dec")
  
# now per habitat
  baltdat <- subset(baltdat,!(is.na(baltdat$depth)))
  habitat <- aggregate(baltdat$state,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitat_susp <- aggregate(baltdat$state_susp,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitat_biot <- aggregate(baltdat$state_biot,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitat_tot <- cbind(habitat,habitat_susp[,2],habitat_biot[,2])
    
# calculate total surface area
  totarea <- aggregate(baltdat$area_sqkm,by=list(baltdat$msfd_habitat),FUN="sum",na.rm=T)
  totarea$frac<-totarea$x/(sum(totarea$x))
  totarea$frac2 <- round(totarea$frac, digits = 2)
  habitat_tot <- cbind(habitat_tot,totarea$frac,totarea$frac2)
  msfd_imp <- subset(habitat_tot,(habitat_tot$Group.1 %in% c("Circalittoral mixed sediment","Circalittoral mud",
                                                     "Offshore circalittoral mud","Circalittoral sand",
                                                     "Infralittoral mixed sediment","Offshore circalittoral mixed sediment")))
  msfd_imp <- as.data.frame(msfd_imp)
  colnames(msfd_imp) <- c("msfd","state","susp_state","biot_state","fraction area","fraction_2dec")
  msfd_imp$msfd <- as.character(msfd_imp$msfd)
  
  #calculate weighted per surface area, average for the other regions
  remreg <- subset(habitat_tot,!(habitat_tot$Group.1 %in% c("Circalittoral mixed sediment","Circalittoral mud",
                                                    "Offshore circalittoral mud","Circalittoral sand",
                                                    "Infralittoral mixed sediment","Offshore circalittoral mixed sediment")))
  other <- remreg[,5]*remreg[,2:4]
  other <- colSums(other)/sum(remreg[,5])
  other <- data.frame("others",other[1],other[2],other[3],sum(remreg[,5]),sum(remreg[,6]))
  colnames(other) <- c("msfd","state","susp_state","biot_state","fraction area","fraction_2dec")
  
  statehabitat <- rbind(whole,msfd_imp,other)
  statehabitat <- statehabitat[order(-statehabitat$fraction_2dec),] 
  
# table to:
  setwd("C:/Users/pdvd/Online for git/Baltic/Output")
  save(statehabitat,file="msfd habitat state.RData")  
  