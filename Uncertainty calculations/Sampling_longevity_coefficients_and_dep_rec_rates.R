
# sampling of uncertainty of model coeficients and depletion + recovery 

# load the model outcome  
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("Model_output_whole_community.RData")

# now re-sample the longevity estimations
  sdev <- (modcf[,3]-modcf[,1])/2 # calculate sd (95% confidence is 2x standard deviation)
  
  dist <- matrix(data=NA, nrow = 1000, ncol=8)
  dist[,1] <- seq(modcf[1,3]-sdev[1]*4,modcf[1,3]+sdev[1]*4, length.out = 1000)
  dist[,2] <- seq(modcf[2,3]-sdev[2]*4,modcf[2,3]+sdev[2]*4, length.out = 1000)
  dist[,3] <- seq(modcf[3,3]-sdev[3]*4,modcf[3,3]+sdev[3]*4, length.out = 1000)
  dist[,4] <- seq(modcf[4,3]-sdev[4]*4,modcf[4,3]+sdev[4]*4, length.out = 1000)
  dist[,5] <- seq(modcf[5,3]-sdev[5]*4,modcf[5,3]+sdev[5]*4, length.out = 1000)
  dist[,6] <- seq(modcf[6,3]-sdev[6]*4,modcf[6,3]+sdev[6]*4, length.out = 1000)
  dist[,7] <- seq(modcf[7,3]-sdev[7]*4,modcf[7,3]+sdev[7]*4, length.out = 1000)
  dist[,8] <- seq(modcf[8,3]-sdev[8]*4,modcf[8,3]+sdev[8]*4, length.out = 1000)

  samp <- matrix(data=NA, nrow= 1500,ncol = 8)
  dat <- rnorm(1500, mean = modcf[1,3], sd = sdev[1])
  for (i in 1:1500){
    samp[i,] <-  dist[which.min(abs(dist[,1] - dat[i])),]
  }

  colnames(samp) <- rownames(modcf)
  setwd("C:/Users/pdvd/Online for git/Baltic/Uncertainty calculations/")
  save(samp,file="sampling_coefficients.Rdata")

##############
# re-sample depletion rates d from the density distribution
  dat <- matrix(data=NA,ncol=2,nrow=1500)

# get the distribution of d
  library(logitnorm)
  quant <- (c(0.02, 0.06, 0.16))
  theta <- twCoefLogitnormN(quant, perc = c(0.05, 0.50, 0.95))
  #x <- seq(0,1, length.out=10000) 
  #d <- dlogitnorm(x, mu=theta[1], sigma=theta[2])
  #plot(d~x,type="l")
  #cumd <- cumsum(d)/sum(d)*100
  #plot(cumd)
  #(x[which.min(abs(cumd - 5))])
  #(x[which.min(abs(cumd - 50))])
  #(x[which.min(abs(cumd - 95))])

# generate random numbers
  nb  <- 1500
  dat[,1] <- rlogitnorm(nb, mu=theta[1], sigma=theta[2])

##############
# re-sample H from the density distribution (r = H/longevity)
  med <- 5.31 # see Hiddink et al. 2019 J Applied Ecology
  lower <- 2.43 # 2.5 
  upper <- 11.44 # 97.5

  x <- c(lower,med,upper)
  x <- log(x) # log normal distribution
  sdev <- (x[2]-x[1])/2 # calculate sd (95% confidence is 2x standard deviation)
  
  #x <- seq(log(1), log(30), by = .001)
  #a <- dnorm(x = x, mean = log(5.31), sd = sdev)
  #plot(a)
  #cuma <- cumsum(a)/sum(a)*100
  #exp(x[which.min(abs(cuma - 2.5))])
  #exp(x[which.min(abs(cuma - 50))])
  #exp(x[which.min(abs(cuma - 97.5))])
  
# generate random numbers
  nb <- 1500
  dat[,2] <- exp(rnorm(nb, mean = log(5.31), sd = sdev)) 

# combine d and r 
  colnames(dat) <- c("d_rate","r_number")
  setwd("C:/Users/pdvd/Online for git/Baltic/Uncertainty calculations/")
  write.csv(dat,"sampling_dep_rec.csv")

