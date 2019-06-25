
# illustration to show the interactive effects between hypoxia and trawling intensity

  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("Model_output_whole_community.Rdata")
  
# Calculate impact from continuous longevity composition
  b0 <- modcf[1,3]    ### intercept
  b1 <- modcf[2,3]      ### ln(longevity)
  b2 <- modcf[3,3]      ## Salinity
  b3 <- modcf[4,3]      ### depth
  b4 <- modcf[5,3]     ### exposure
  b5 <- modcf[6,3]     ## ln(longevity):Salinity
  b6 <- modcf[7,3]      ## Salinity: Depth
  b7 <- modcf[8,3]      ## ln(longevity): Depth

# input parameter
  Depth <- log(12+1)
  Salinity <- 26/10
  Stress <- log(30000+1)
  longclasses <- c(10^-100,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  nb <- length(longclasses)-1
  datlong <- matrix(data=NA, ncol=nb+1, nrow=1)

# calculate longevities
  library(rje)
  for(i in 1:nb){
    llst<-log(longclasses[i])
    llend <- log(longclasses[i+1])
    datlong[,i]<-expit(b0+b5*llend*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llend+b7*llend*Depth)-
      expit(b0+b5*llst*Salinity+b6*Depth*Salinity+b4*Stress+b3*Depth+b2*Salinity+b1*llst+b7*llst*Depth)
  }
  
  datlong[,15] <- 1- sum(datlong[,1:14])
  colnames(datlong) <-c("ll1","ll2","ll3","ll4","ll5",
                        "ll6","ll7","ll8","ll9","ll10",
                        "ll11","ll12","ll13","ll14","ll15")
  
  plot(cumsum(datlong[1:14]),ylim=c(0,1),xlim=c(0,15),xlab="longevity", ylab="Biomass proportion",las=1)

# run model to check interactive effect

# model input
  Meansurface <- seq(0,10,0.5)
  deprate <- 0.06
  oxygenwinter <- 4
  oxygenspring <- 4
  oxygensummer  <- c(0.4,0.5,0.7,4)
  oxygenautumn <- 4
  longclasses <- c(1:15)
  Recov <-  (5.31/longclasses) / 365
  K <- 1
  t<- 1:365 # number of days in a year
  yearstime <- 100 # number of years to run
  state <- matrix(data=NA,ncol=4,nrow=length(Meansurface))
  start <- rep(NA,length=(yearstime*365)+1);start[1]<-1
  
  for (q in 1:4){
    oxygenW  <-  rep(oxygenwinter,92)
    oxygenS  <-  rep(oxygenspring,91)
    oxygenSU <- rep(oxygensummer[q],91)
    oxygenA  <- rep(oxygenautumn,91)
    oxygen   <- c(oxygenW,oxygenS,oxygenSU,oxygenA)
    oxygen_tot   <- rep(oxygen,100)
    Odef  <- 0.01 * (1+oxygen_tot-0.5)^-6

# fishing random events
    for (i in 1:length(Meansurface)){
        SAR <- Meansurface[i]
        SAR[is.na(SAR)] <- 0
    
        Fishing <- c()
        for (year in 1:yearstime){
            idx <- floor(SAR)
            idx[(SAR-idx) > runif(1,0,1)] <- idx +1
            Fishing_idx <- sample(t, idx)
            FishY <- rep(0,365)
            FishY[Fishing_idx] <- 1
            Fishing<- c(Fishing,FishY)
        }
  
        Pall <- matrix(rep(start,15),ncol=15)
      
        for (p in 1:(yearstime*365)){
            Fish <- Fishing[p]*deprate
            Oxi <- Odef[p]
            Pall[p+1,c(1:15)] <- Pall[p,c(1:15)] + Recov[c(1:15)]*Pall[p,c(1:15)] *((K-Pall[p,c(1:15)])/K) - Fish*Pall[p,c(1:15)] - Oxi*Pall[p,c(1:15)]
            Pall[p+1,c(1:15)][Pall[p+1,c(1:15)]<0.001] <- 0.001 
            }
    
        Pallavg <- colMeans(Pall[round(0.5*nrow(Pall)):nrow(Pall),1:15])
  
# take the average state over the last 50 years and multiply with the initial fraction of biomass
        state[i,q] <-  sum(datlong[1,]*Pallavg)
  } 
  }

# save figure
  setwd("C:/Users/pdvd/Online for git/Baltic/Output")
  pdf("Interaction illustration.pdf",width=5,height=4.5)
  
  par(mar = c(5, 6, 4, 5) + 0.1)
  plot(state[,1]~Meansurface,ylim=c(0,1),type="l", col="red",lwd=2,xaxt="n",yaxt="n",
       xlab="Fishing intensity (per year)",ylab="Community state")
  lines(state[,2]~Meansurface, col="blue",lwd=2,lty=3)
  lines(state[,3]~Meansurface, col="black",lwd=2,lty=5)
  lines(state[,4]~Meansurface, col="purple",lwd=2,lty=4)
  axis(1,c(0,5,10))
  axis(2,c(0,0.5,1),las=1)
  legend(6,0.9,legend=c(4,0.7,0.5,0.4),col=c("purple","black","blue","red"),lty=c(4,5,3,1),
         box.lty=0)

  dev.off()