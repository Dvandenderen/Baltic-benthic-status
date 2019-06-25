 
# illustration of the cumulative biomass per longevity

  rm(list=ls())
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Data")
  load("benthic_data_gogina.RData")

# select stations
    
  Bsal <- subset(Bstations,Bstations$Bsalinity > 25 & Bstations$Bsalinity < 27)
  Bsal <- subset(Bsal,Bsal$surface_SAR < 0.1)
  Bsal <- subset(Bsal,Bsal$depth < -10  &  Bsal$depth > -13)
  Bsal <- subset(Bsal,Bsal$expos > 28000 & Bsal$expos < 52000)
  
  setwd("C:/Users/pdvd/Online for git/Baltic/Output/")
  pdf("Biomass_longevity.pdf",width=5,height=4.5)

# make plot
  par(mar = c(5, 6, 4, 5) + 0.1)
  
  stat1 <- Bsal[1,]
  x <- c(1,3,10)
  y <- c(stat1$`L<1`,stat1$`L<1`+stat1$`L1-3`,stat1$`L<1`+stat1$`L1-3`+stat1$`L3-10`)
  
  plot(x,y,xlim=c(0,15),ylim=c(0,1),xaxt="n",yaxt="n",ylab="Cum. biomass proportions",
       xlab="Longevity (yr)",lwd=2,pch=4)
  axis(1,c(0,5,10,15))
  axis(2,c(0,0.5,1),las=1)
  
  stat1 <- Bsal[2,]
  x <- c(1,3,10)
  y <- c(stat1$`L<1`,stat1$`L<1`+stat1$`L1-3`,stat1$`L<1`+stat1$`L1-3`+stat1$`L3-10`)
  points(x,y,lwd=2,pch=8,col="blue")
  
  stat1 <- Bsal[3,]
  x <- c(1,3,10)
  y <- c(stat1$`L<1`,stat1$`L<1`+stat1$`L1-3`,stat1$`L<1`+stat1$`L1-3`+stat1$`L3-10`)
  points(x,y,lwd=2,pch=15,col="red")
  
  stat1 <- Bsal[4,]
  x <- c(1,3,10)
  y <- c(stat1$`L<1`,stat1$`L<1`+stat1$`L1-3`,stat1$`L<1`+stat1$`L1-3`+stat1$`L3-10`)
  points(x,y,lwd=2,pch=16,col="green")
  
  stat1 <- Bsal[5,]
  x <- c(1,3,10)
  y <- c(stat1$`L<1`,stat1$`L<1`+stat1$`L1-3`,stat1$`L<1`+stat1$`L1-3`+stat1$`L3-10`)
  points(x,y,lwd=2,pch=17,col="orange")
  
  x <- c(1,1,1,1,1,3,3,3,3,3,10,10,10,10,10)
  y <- c(Bsal$`L<1`[1:5],Bsal$`L<1`[1:5]+Bsal$`L1-3`[1:5],Bsal$`L<1`[1:5]+Bsal$`L1-3`[1:5]+Bsal$`L3-10`[1:5])
  
  for (i in 1:(length(y))){
    if (y[i] < 1e-3){ y[i] <- 1e-3}
    if (y[i] > 0.999){y[i] <- 0.999}
  }
  
  lx <- log(x)
  dat <- data.frame(y,lx)
  
  mod1 <- glm(y ~ lx, family="binomial",data = dat)
  b0 <- mod1$coefficients[1]
  b1 <- mod1$coefficients[2]
  
  fit <- seq(0.01,15,0.1)
  lfit <- log(fit)
  
  library(rje)
  pred <- expit(b0+b1*lfit)
  lines(fit,pred)
  
  dev.off()