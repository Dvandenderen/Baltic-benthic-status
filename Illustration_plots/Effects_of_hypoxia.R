
# illustration of the effects of oxygen appendix 2

# get the 100 years and take the average
  K <- 1
  r1 <- (5.31/1)/365
  r10 <- (5.31/10)/365
  t<- 1:365
  yearstime <- 100
  start <- rep(NA,length=(yearstime*365)+1);start[1]<-1
  conc <- seq(from= 0.01,to= 3.5,length.out = 60)
  dat <- matrix(data=NA,nrow=60,ncol=2)
  
  for (i in 1:length(conc)){
    oxygenW  <- rep(4,92)
    oxygenS  <- rep(4,91)
    oxygenSU <- rep(conc[i],91)
    oxygenA  <- rep(4,91)
    oxygen   <- c(oxygenW,oxygenS,oxygenSU,oxygenA)
    oxygen_tot   <- rep(oxygen,yearstime)
    Odef  <- 0.01 * (1+oxygen_tot-0.5)^-6
    
    P_one <- start
    P_ten <- start
    
    for (p in 1:(yearstime*365)){
      Fish <- 0
      Oxi <- Odef[p]
      P_one[p+1] <- P_one[p] + r1*P_one[p] *((K-P_one[p])/K) - Fish*P_one[p] - Oxi*P_one[p]
      P_one[p+1][P_one[p+1]<0.001] <- 0.001
      P_ten[p+1] <- P_ten[p] + r10*P_ten[p] *((K-P_ten[p])/K) - Fish*P_ten[p] - Oxi*P_ten[p]
      P_ten[p+1][P_ten[p+1]<0.001] <- 0.001 
    }
    
    Pall <- data.frame(P_one,P_ten)
    dat[i,1]<- mean(Pall[round(0.5*yearstime*365):(yearstime*365),1])
    dat[i,2]<- mean(Pall[round(0.5*yearstime*365):(yearstime*365),2])
  }
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Output")
  pdf("Hypoxia average.pdf",width=5,height=4.5)  
  par(mar = c(5, 6, 4, 5) + 0.1)
  plot(dat[,1]~conc,lty=1,lwd=2,type="l",yaxt="n",xlab="Oxygen concentration",
       ylab="Benthic state", xaxt="n")
  legend("bottomright",legend =c(1,10),lwd=2,lty=c(1,3),title="longevity",bty = "n")
  axis(1,c(0,0.5,1,1.5,2,2.5,3))
  axis(2,c(0,0.5,1),las=1)
  lines(dat[,2]~conc,lty=3,lwd=2)
  dev.off()
    
# set parameters for one year
  K <- 1
  r1 <- (5.31/1)/365
  r10 <- (5.31/10)/365
  t<- 1:365
  yearstime <- 1
  conc <- c(0.3,0.4,0.5,1)
  
  pdf("Hypoxia one year.pdf",width=6,height=4.5)  
  par(mar=c(2,5,2,1))
  par(mfrow=c(2,2))
  
  for (i in 1:4){
    oxygenW  <- rep(4,92)
    oxygenS  <- rep(4,91)
    oxygenSU <- rep(conc[i],91)
    oxygenA  <- rep(4,91)
    oxygen   <- c(oxygenW,oxygenS,oxygenSU,oxygenA)
    oxygen_tot   <- rep(oxygen,yearstime)
    Odef  <- 0.01 * (1+oxygen_tot-0.5)^-6
    
    Pall <- 1
    for (p in 1:(yearstime*365)){
      Fish <- 0
      Oxi <- Odef[p]
      Pall[p+1] <- Pall[p] + r1*Pall[p] *((K-Pall[p])/K) - Fish*Pall[p] - Oxi*Pall[p]
      Pall[p+1][Pall[p+1]<0.001] <- 0.001 
    }
    
    plot(Pall,type="l",lwd=2,xlab="Days",ylab="Community biomass",las=1,ylim=c(0,1),
         main= bquote(.(conc[i]) ~ ml*O[2]*L^{-1}), yaxt="n")
    axis(2,c(0,0.5,1),las=1)
    
    Pall <- 1
    for (p in 1:(yearstime*365)){
      Fish <- 0
      Oxi <- Odef[p]
      Pall[p+1] <- Pall[p] + r10*Pall[p] *((K-Pall[p])/K) - Fish*Pall[p] - Oxi*Pall[p]
      Pall[p+1][Pall[p+1]<0.001] <- 0.001 
    }
    
    lines(Pall,lwd=2,lty=3)
  }
  dev.off()
  