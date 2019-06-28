
# get uncertainty of baltic state temporal model
# I only included grids that are with oxygen concentrations <3.2 
# the model is a bit slow (to run 1500 x)

  library(R.matlab)
  path <- "C:/Users/pdvd/Online for git/Baltic/Uncertainty calculations/"
  pathname <- file.path(path, "state_uncertainty_tempmodel.mat")
  state <- readMat(pathname)
  state_temp <- unlist(state$state)
  state_temp <- as.data.frame(state_temp)
  state_temp <- as.matrix(state_temp)

# get c-squares of the grids
  csq <- read.csv(file = "griddata_uncertainty_temporal_model.csv",header=T)
  state_temp <- data.frame(csq[,2],state_temp)

# to get uncertainty in the grids that are solely fished I use the equation from Pitcher

# get balticgrid
  setwd("C:/Users/pdvd/Online for git/Baltic/Data")
  load("balticgrid.RData")
  
# all negative oxygen to zero 
  balticgrid@data$oxygenautumn[balticgrid@data$oxygenautumn < 0]  <- 0
  balticgrid@data$oxygenwinter[balticgrid@data$oxygenwinter < 0]  <- 0
  balticgrid@data$oxygensummer[balticgrid@data$oxygensummer < 0]  <- 0
  balticgrid@data$oxygenspring[balticgrid@data$oxygenspring < 0]  <- 0
  balticgrid@data$min_oxygen[balticgrid@data$min_oxygen < 0]  <- 0
  
# get maximum oxygen concentrations
  balticgrid@data<-transform(balticgrid@data, max = pmax(balticgrid@data$oxygenwinter,balticgrid@data$oxygenspring,
                                                         balticgrid@data$oxygensummer,balticgrid@data$oxygenautumn))
  colnames(balticgrid@data)[18]<-"max_oxygen"
  
# get the relevant data
  rdat <-as.data.frame(balticgrid)
  rdat <-rdat[complete.cases(rdat[ , c("depth", "Bsalinity","exposu")]),]
  rdat$surface_SAR[is.na(rdat$surface_SAR)] <- 0
  rdat <- subset(rdat,rdat$min_oxygen >= 3.2 & rdat$surface_SAR > 0)
  rdat$depth[rdat$depth > -0.5] <- -0.5
  Depth<-log(abs(rdat$depth-1))
  Salinity<-rdat$Bsalinity/10
  Stress<-log(rdat$expos+1)

# load the longevity model outcome coefficients 
  setwd("C:/Users/pdvd/Online for git/Baltic/Uncertainty calculations")
  load("sampling_coefficients.Rdata")
  
# combine all data 
  longclasses <- c(10^-100,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  nb <- length(longclasses)-1
  datlong <- array(data=100, c(1500, nrow(rdat),nb+1))
  
# get longevity estimates from the sampling coefficients
  library(rje)
  
  for (j in 1: 1500){
    b0<-samp[j,1] ; b1<-samp[j,2]
    b2<-samp[j,3] ; b3<-samp[j,4]
    b4<-samp[j,5] ; b5<-samp[j,6]
    b6<-samp[j,7] ; b7<-samp[j,8]
    
    datnew <- matrix(data=NA, ncol=nb+1, nrow=nrow(rdat))
    for(i in 1:nb){
      llst<-log(longclasses[i])
      llend <- log(longclasses[i+1])
      datnew[,i] <-expit(b0+b5*llend*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llend+b7*llend*Depth)-
        expit(b0+b5*llst*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llst+b7*llst*Depth)
    }
    
    datnew[,15] <- 1- rowSums(datnew[,1:14])
    datlong[j,,] <- datnew
  }

# get fishing conditions
  griddat <- data.frame(squares=rdat$squares,SAR=rdat$surface_SAR)
  
# number of classes
  longclasses <- c(1:15)
  
# load r-number and depletion rate from the resampling
  par <- read.csv("sampling_dep_rec.csv",header=T,sep=",")
  par <- par[,2:3]
  
  state_avg <- matrix(data=NA,nrow=nrow(rdat),ncol=1500)
  
  for (j in 1:1500){
    Recov <- par[j,2]/longclasses 
    depBT <- griddat$SAR * par[j,1]
    
    # calculate state for the whole community
    dat <-as.data.frame(matrix(data=NA,nrow=nrow(rdat),ncol=15))
    
    for(i in 1:15){
      dat[,i]<-datlong[j,,i]*(1-depBT/Recov[i])
    }  
    dat[dat<0] <- 0
    state_avg[,j] <- rowSums(dat)
  }
  
  state_notemp <- data.frame(griddat$squares, state_avg)

# for the manuscript I calculate the 25th and 75th quantile 
# and their difference
  library(matrixStats)
  probs <- c(0.05,0.5 ,0.95)
  
  # Row quantiles
  q <- rowQuantiles(as.matrix(state_notemp[,2:1501]), probs = probs)
  qt <- rowQuantiles(as.matrix(state_temp[,2:1501]), probs = probs)
  
  quant <- rbind(q,qt)
  squares <- c(as.character(state_notemp[,1]),as.character(state_temp[,1]))
  state_quant <- data.frame(squares,quant)
  
  plot(state_quant[,4]-state_quant[,2])
  
# load balticgrid outcome
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")
  balticgrid@data <- balticgrid@data[,-c(24:29)]

# combine balticgrid with calculated state at 5th, 50th and 95th quantile
  balticgrid@data <- cbind(balticgrid@data, state_quant[match(balticgrid@data$squares, state_quant$squares),c(2:4)])
  names(balticgrid@data)[24] <- "Q05_state"
  names(balticgrid@data)[25] <- "Q50_state"
  names(balticgrid@data)[26] <- "Q95_state"
  
# now include all regions with very low oxygen conditions
  # get maximum oxygen concentrations
  balticgrid@data<-transform(balticgrid@data, max = pmax(balticgrid@data$oxygenwinter,balticgrid@data$oxygenspring,
                                                         balticgrid@data$oxygensummer,balticgrid@data$oxygenautumn))
  colnames(balticgrid@data)[27]<-"max_oxygen"
  balticgrid@data$Q25_state[balticgrid@data$max_oxygen<0.1] <- 0
  balticgrid@data$Q50_state[balticgrid@data$max_oxygen<0.1] <- 0
  balticgrid@data$Q75_state[balticgrid@data$max_oxygen<0.1] <- 0
  save(balticgrid,file="balticgrid_state.RData") 
    