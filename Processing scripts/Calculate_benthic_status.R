
# calculate impact (since the temporal model is slow it is estimated in matlab, script is to prepare the files 
# for matlab)

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
  rdat <- subset(rdat,rdat$min_oxygen <= 3.2 |  rdat$surface_SAR > 0)
  rdat$depth[rdat$depth > 0]<- -0.5
  Depth<-log(abs(rdat$depth-1))
  Salinity<-rdat$Bsalinity/10
  Stress<-log(rdat$expos+1)

# load the model outcome  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("Model_output_whole_community.RData")
  
  samp <- matrix(data=NA, nrow= 1,ncol = 8)
  samp[1,] <- modcf[,3]
  b0<-samp[1,1] ; b1<-samp[1,2]
  b2<-samp[1,3] ; b3<-samp[1,4]
  b4<-samp[1,5] ; b5<-samp[1,6]
  b6<-samp[1,7] ; b7<-samp[1,8]
  
# combine all data 
  longclasses <- c(10^-100,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  nb <- length(longclasses)-1
  datlong <- matrix(data=100, nrow(rdat),nb+1)

# run the model 
  library(rje)
  
  datnew <- matrix(data=NA, ncol=nb+1, nrow=nrow(rdat))
  for(i in 1:nb){
      llst<-log(longclasses[i])
      llend <- log(longclasses[i+1])
      datnew[,i] <-expit(b0+b5*llend*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llend+b7*llend*Depth)-
      expit(b0+b5*llst*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llst+b7*llend*Depth)
  }
    
  datnew[,15] <- 1- rowSums(datnew[,1:14])

  benth_matlab <- cbind(rdat[,c(1,11:14,16)], datnew)
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data/")
  write.csv(benth_matlab,file="whole_comm_matlab.csv")
  
  
  