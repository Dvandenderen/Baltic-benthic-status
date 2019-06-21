
# get the distribution of state over the whole area

  # load balticgrid_state
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")
  
  baltdat <- as.data.frame(balticgrid)
  baltdat$state[is.na(baltdat$state) & !(is.na(baltdat$depth))] <- 1  
  
  # first cut the edge 
  baltdat2 <- data.frame(longitude = coordinates(balticgrid)[,1], latitude = coordinates(balticgrid)[,2])
  baltdat<-cbind(baltdat,baltdat2)
  baltdat$knip<- rep(1,nrow(baltdat))
  baltdat$knip[baltdat$longitude < 10.3 & baltdat$latitude > 57.3]<-NA
  baltdat$knip[baltdat$longitude < 10 & baltdat$latitude > 56.5]<-NA
  baltdat$knip[baltdat$longitude > 10 & baltdat$longitude<12 & baltdat$latitude > 57.8]<-NA
  baltdat<-subset(baltdat,!(is.na(baltdat$knip)))  
  
  state <- data.frame(baltdat$state,baltdat$area_sqkm)
  state <- state[complete.cases(state[ , 1]),]
  state <- state[order(-state[,1]),] 
  state$cum <- cumsum(state[,2])
  state$cum <- state$cum/sum(state[,2])
  
# include a breakpoint to improve clarity
  xb <- min(which(state[,3] >0.40))
  state[min(which(state <0.9)),3]
  state[min(which(state <0.5)),3]   
  
# figures to:
  setwd("C:/Users/pdvd/Online for git/Baltic/Output")
  
  pdf("Distribution benthic state.pdf",width=5,height=4.4)
  plot(state[xb:28237,1]~state[xb:28237,3],xaxt="n",yaxt="n",xlab="fraction of Baltic Sea",
       ylab="benthic state",type="l",lwd=3) 
  axis(1,c(.40,.60,.78,.86,1),labels = c(0,.60,.78,.86,1))
  axis.break(1,.50,style="slash",brw=0.05) 
  axis(2,c(0,0.5,0.9,1),las=1)
  lines(c(.78,.78),c(-10,0.9),lty=3,col="blue",lwd=2)  
  lines(c(-10,.78),c(0.9,0.9),lty=3,col="blue",lwd=2)
  lines(c(.86,.86),c(-10,0.5),lty=3,col="red",lwd=2)  
  lines(c(-10,.86),c(0.5,0.5),lty=3,col="red",lwd=2)
  dev.off()
  
  