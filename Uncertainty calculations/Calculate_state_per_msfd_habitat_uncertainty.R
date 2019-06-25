# script to derive state per habitat type

# load balticgrid_state
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")

  baltdat <- as.data.frame(balticgrid)
  baltdat$Q25_state[is.na(baltdat$Q25_state) & !(is.na(baltdat$depth))] <- 1  
  baltdat$Q50_state[is.na(baltdat$Q50_state) & !(is.na(baltdat$depth))] <- 1  
  baltdat$Q75_state[is.na(baltdat$Q75_state) & !(is.na(baltdat$depth))] <- 1  
  baltdat$onof_state1[is.na(baltdat$onof_state1) & !(is.na(baltdat$depth))] <- 1 
  baltdat$onof_state2[is.na(baltdat$onof_state2) & !(is.na(baltdat$depth))] <- 1 
      
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
  whole <- c(mean(baltdat$Q25_state,na.rm=T),mean(baltdat$Q50_state, na.rm=T),mean(baltdat$Q75_state, na.rm=T),mean(baltdat$onof_state1,na.rm=T),mean(baltdat$onof_state2,na.rm=T))
  whole <- data.frame("whole region",whole[1],whole[2],whole[3],whole[4],whole[5],1,1)
  colnames(whole) <-  c("msfd","stateQ25","stateQ50","stateQ75","stateonof1","stateonof2","fraction area","fraction_2dec")
  
# now per habitat
  baltdat <- subset(baltdat,!(is.na(baltdat$depth)))
  habitatQ25 <- aggregate(baltdat$Q25_state,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitatQ50 <- aggregate(baltdat$Q50_state,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitatQ75 <- aggregate(baltdat$Q75_state,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitatOnof1 <- aggregate(baltdat$onof_state1,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitatOnof2 <- aggregate(baltdat$onof_state2,by=list(baltdat$msfd_habitat),FUN="mean",na.rm=T)
  habitat_tot <- cbind(habitatQ25,habitatQ50[,2],habitatQ75[,2],habitatOnof1[,2],habitatOnof2[,2])
    
# calculate total surface area
  totarea <- aggregate(baltdat$area_sqkm,by=list(baltdat$msfd_habitat),FUN="sum",na.rm=T)
  totarea$frac<-totarea$x/(sum(totarea$x))
  totarea$frac2 <- round(totarea$frac, digits = 2)
  habitat_tot <- cbind(habitat_tot,totarea$frac,totarea$frac2)
  msfd_imp <- subset(habitat_tot,(habitat_tot$Group.1 %in% c("Circalittoral mixed sediment","Circalittoral mud",
                                                     "Offshore circalittoral mud","Circalittoral sand",
                                                     "Infralittoral mixed sediment","Offshore circalittoral mixed sediment")))
  msfd_imp <- as.data.frame(msfd_imp)
  colnames(msfd_imp) <-  c("msfd","stateQ25","stateQ50","stateQ75","stateonof1","stateonof2","fraction area","fraction_2dec")
  msfd_imp$msfd <- as.character(msfd_imp$msfd)
  
  #calculate weighted per surface area, average for the other regions
  remreg <- subset(habitat_tot,!(habitat_tot$Group.1 %in% c("Circalittoral mixed sediment","Circalittoral mud",
                                                    "Offshore circalittoral mud","Circalittoral sand",
                                                    "Infralittoral mixed sediment","Offshore circalittoral mixed sediment")))
  other <- remreg[,7]*remreg[,2:6]
  other <- colSums(other)/sum(remreg[,7])
  other <- data.frame("others",other[1],other[2],other[3],other[4],other[5],sum(remreg[,7]),sum(remreg[,8]))
  colnames(other) <- c("msfd","stateQ25","stateQ50","stateQ75","stateonof1","stateonof2","fraction area","fraction_2dec")
  
  statehabitat <- rbind(whole,msfd_imp,other)
  statehabitat <- statehabitat[order(-statehabitat$fraction_2dec),] 
  
# table to:
  setwd("C:/Users/pdvd/Online for git/Baltic/Output")
  save(statehabitat,file="msfd habitat state_uncertainty.RData")  
  