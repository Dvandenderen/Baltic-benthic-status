
# script to derive the main results

# get balticgrid
  setwd("C:/Users/pdvd/Online for git/Baltic/Data")
  load("balticgrid.RData")

# get baltic state
  library(R.matlab)
  path <- "C:/Users/pdvd/Online for git/Baltic/Processed data"
  pathname <- file.path(path, "state.mat")
  state <- readMat(pathname)
  state <- unlist(state$state)
  colnames(state) <- c("state","state_susp","state_biot")
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  locat <- read.csv("whole_comm_matlab.csv", header=T, sep=",")
  state <- data.frame(locat$squares,state)
  
# combine balticgrid with calculated state
  balticgrid@data <- cbind(balticgrid@data, state[match(balticgrid@data$squares, state$locat.squares),c(2:4)])

# get median longevities
  Depth <- balticgrid@data$depth
  Depth[Depth > -0.5] <- -0.5
  Depth<-log(abs(Depth-1))
  Salinity<-balticgrid@data$Bsalinity/10
  Stress<-log(balticgrid@data$expos+1)
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("Model_output_whole_community.RData")
  samp <- matrix(data=NA, nrow= 1,ncol = 8)
  samp[1,] <- modcf[,3]
  b0<-samp[1,1] ; b1<-samp[1,2]
  b2<-samp[1,3] ; b3<-samp[1,4]
  b4<-samp[1,5] ; b5<-samp[1,6]
  b6<-samp[1,7] ; b7<-samp[1,8]
  
  library(rje)
  medLong <- exp((logit(0.5)-b0-b6*Depth*Salinity-b4*Stress-b3*Depth-b2*Salinity)/(b1+b5*Salinity+b7*Depth))
  medLong[medLong<1]<-1
  medLong[medLong>15]<-15
  balticgrid@data$medLong<-medLong
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("Model_output_suspensionF.Rdata")
  samp <- matrix(data=NA, nrow= 1,ncol = 8)
  samp[1,] <- modcf_susp[,3]
  b0<-samp[1,1] ; b1<-samp[1,2]
  b2<-samp[1,3] ; b3<-samp[1,4]
  b4<-samp[1,5] ; b5<-samp[1,6]
  b6<-samp[1,7] ; b7<-samp[1,8]
  
  library(rje)
  medLong_susp <- exp((logit(0.5)-b0-b6*Depth*Salinity-b4*Stress-b3*Depth-b2*Salinity)/(b1+b5*Salinity+b7*Depth))
  medLong_susp[medLong_susp<1]<-1
  medLong_susp[medLong_susp>15]<-15
  balticgrid@data$medLong_susp<-medLong_susp
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("Model_output_bioturbators.Rdata")
  samp <- matrix(data=NA, nrow= 1,ncol = 7)
  samp[1,] <- modcf_biot[,3]
  b0<-samp[1,1] ; b1<-samp[1,2]
  b2<-samp[1,3] ; b3<-samp[1,4]
  b4<-samp[1,5] ; b5<-samp[1,6]
  b6<-samp[1,7]
  
  library(rje)
  medLong_biot <- exp((logit(0.5)-b0-b5*Depth*Salinity-b4*Stress-b3*Depth-b2*Salinity)/(b1+b6*Depth))
  medLong_biot[medLong_biot<1]<-1
  medLong_biot[medLong_biot>15]<-15
  balticgrid@data$medLong_biot<-medLong_biot
  
  save(balticgrid,file="balticgrid_state.RData")
  
  