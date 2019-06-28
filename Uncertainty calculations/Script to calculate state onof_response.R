# get balticgrid
  setwd("C:/Users/pdvd/Online for git/Baltic/Data")
  load("balticgrid.RData")

# all negative oxygen to zero 
  balticgrid@data$oxygenautumn[balticgrid@data$oxygenautumn < 0]  <- 0
  balticgrid@data$oxygenwinter[balticgrid@data$oxygenwinter < 0]  <- 0
  balticgrid@data$oxygensummer[balticgrid@data$oxygensummer < 0]  <- 0
  balticgrid@data$oxygenspring[balticgrid@data$oxygenspring < 0]  <- 0
  balticgrid@data$min_oxygen[balticgrid@data$min_oxygen < 0]  <- 0

# get the relevant data
  rdat <-as.data.frame(balticgrid)
  rdat <-rdat[complete.cases(rdat[ , c("depth", "Bsalinity","exposu")]),]
  rdat$surface_SAR[is.na(rdat$surface_SAR)] <- 0
  rdat <- subset(rdat, rdat$surface_SAR > 0)
  rdat$depth[rdat$depth > -0.5] <- -0.5
  Depth<-log(abs(rdat$depth-1))
  Salinity<-rdat$Bsalinity/10
  Stress<-log(rdat$expos+1)

# load the longevity model outcome coefficients 
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data/")
  load("Model_output_whole_community.Rdata")

# combine all data 
  longclasses <- c(10^-100,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  nb <- length(longclasses)-1

# get longevity estimates from the sampling coefficients
  library(rje)

  b0<-modcf[1,3] ; b1<-modcf[2,3]
  b2<-modcf[3,3] ; b3<-modcf[4,3]
  b4<-modcf[5,3] ; b5<-modcf[6,3]
  b6<-modcf[7,3] ; b7<-modcf[8,3]
  
  datnew <- matrix(data=NA, ncol=nb+1, nrow=nrow(rdat))
  for(i in 1:nb){
    llst<-log(longclasses[i])
    llend <- log(longclasses[i+1])
    datnew[,i] <-expit(b0+b5*llend*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llend+b7*llend*Depth)-
      expit(b0+b5*llst*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llst+b7*llst*Depth)
  }
  
  datnew[,15] <- 1- rowSums(datnew[,1:14])

# get fishing conditions
  griddat <- data.frame(squares=rdat$squares,SAR=rdat$surface_SAR)

# number of classes
  longclasses <- c(1:15)

  state_avg <- matrix(data=NA,nrow=nrow(rdat),ncol=1)
  Recov <- 5.31/longclasses 
  depBT <- griddat$SAR * 0.06
  
# calculate state for the whole community
  dat <-as.data.frame(matrix(data=NA,nrow=nrow(rdat),ncol=15))
  for(i in 1:15){
    dat[,i]<-datnew[,i]*(1-depBT/Recov[i])
  }  
  dat[dat<0] <- 0
  state_avg <- rowSums(dat)

  state_fish <- data.frame(griddat$squares, state_avg)

# load balticgrid outcome
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")  
   balticgrid@data <- balticgrid@data[,-c(28,29)]
  
# combine balticgrid with calculated state 
  balticgrid@data <- cbind(balticgrid@data, state_fish[match(balticgrid@data$squares, state_fish$griddat.squares),c(2)])
  colnames(balticgrid@data)[28] <- "onof_state1"
  balticgrid@data$onof_state1[balticgrid@data$min_oxygen < 0.3] <- 0
  balticgrid@data$onof_state2 <- balticgrid@data$onof_state1
  balticgrid@data$onof_state2[balticgrid@data$min_oxygen < 0.5] <- 0
  save(balticgrid,file="balticgrid_state.RData")  
  