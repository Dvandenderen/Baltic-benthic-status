# calculate uncertainty in impact (since the temporal model is slow 
# it is estimated in matlab for all grids with oxygen limitation, all other grids are
# using the equilibrium solution of Pitcher (see eq. 1 manuscript)

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
  rdat <- subset(rdat,rdat$min_oxygen < 3.2 | rdat$surface_SAR > 0)
  rdat <- subset(rdat,rdat$min_oxygen < 3.2 & rdat$max_oxygen > 0.1)
  rdat$depth[rdat$depth > -0.5] <- -0.5
  Depth<-log(abs(rdat$depth-1))
  Salinity<-rdat$Bsalinity/10
  Stress<-log(rdat$expos+1)

# load the model outcome  
  setwd("C:/Users/pdvd/Online for git/Baltic/Uncertainty calculations/")
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
  
# save the array for matlab (This array is not on github as it is BIG)
  library(R.matlab)
  writeMat(con= "longevity_resampled",x = datlong)
  
# get fishing and oxygen conditions
  griddat <- data.frame(squares=rdat$squares,SAR=rdat$surface_SAR,oxyautumn = rdat$oxygenautumn, 
                        oxyspring= rdat$oxygenspring, oxysummer= rdat$oxygensummer, oxywinter = rdat$oxygenwinter)
  write.csv(griddat,"griddata_uncertainty_temporal_model.csv")
