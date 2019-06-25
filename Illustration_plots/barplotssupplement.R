##########################################################################################
###
###   Histogram of environmental conditions (Supplementary Information)
###
##########################################################################################

setwd("C:/Users/pdvd/Online for git/Baltic/Data")
load("balticgrid.RData")
test<-as.data.frame(balticgrid)
test2<-data.frame(longitude = coordinates(balticgrid)[,1], latitude = coordinates(balticgrid)[,2])
test<-cbind(test,test2)
test$knip<-test$state
test$knip[test$longitude < 10.3 & test$latitude > 57.3]<-NA
test$knip[test$longitude < 10 & test$latitude > 56.5]<-NA
test$knip[test$longitude > 10 & test$longitude<12 & test$latitude > 57.8]<-NA
test$depth[test$depth > -0.5]<- -0.5
tmp03 <-test

load("benthic_data_gogina.RData")
Bstations<-transform(Bstations, min = pmin(Bstations$OxyigenWint,Bstations$OxyigenSpring,Bstations$OxyigenSummer,Bstations$OxyigenAutumn))
colnames(Bstations)[31]<-"Oxygen"
Bstations$Oxygen[Bstations$Oxygen < 0] <- 0

tmp3 <- Bstations
tmp3 <- subset(tmp3,tmp3$surface_SAR < 0.1)
tmp3 <- subset(tmp3,tmp3$Oxygen > 3.2)

#######################################
hist(x, breaks = "Sturges",
     freq = NULL, probability = !freq,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, col = NULL, border = NULL,
     main = paste("Histogram of" , xname),
     xlim = range(breaks), ylim = NULL,
     xlab = xname, ylab,
     axes = TRUE, plot = TRUE, labels = FALSE,
     nclass = NULL, warn.unused = TRUE,...)

##################################################################
tiff(filename = "C:/Users/pdvd/Online for git/Baltic/Output/SM_Fig_Histogram_Environment.tiff",
     width = 6.7, height = 6.7, units = "in", res=300, compression =  "lzw")
par(mar=c(2,5,2,1))
par(mfrow=c(3,2))
#######################################
####   depth
h <- hist(tmp3$depth, breaks = 20, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, main="Depth locations (m)", xlab="",
     xlim=c(-500,0),ylim=c(0,0.6),axes=T,yaxt="n")
axis(2,c(0,0.3,0.6),las=1)

h <- hist(tmp03$depth, breaks = 50, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, main="Depth (m)", xlab="",
     xlim=c(-500,0),ylim=c(0,0.6),axes=T,yaxt="n")
axis(2,c(0,0.3,0.6),las=1)
#######################################
####  Salinity
h <- hist(tmp3$Bsalinity, breaks = 20, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h,main="Salinity locations (ppt)", xlab="Percentage",
     xlim=c(0,40),ylim=c(0,0.5),axes=T,yaxt="n")
axis(2,c(0,0.25,0.5),las=1)

h <- hist(tmp03$salinityint, breaks = 20, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h,main="Salinity (ppt)", xlab="Percentage",
     xlim=c(0,40),ylim=c(0,0.5),axes=T,yaxt="n")
axis(2,c(0,0.25,0.5),las=1)

#######################################
####   wave stress
####   
h <- hist(log10(tmp3$expos+1), breaks = 20, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h,main="Wave expos. locations", xlab="Bed stress (N/m2)",
     xlim=c(0,6),ylim=c(0,0.5),axes=T,yaxt="n")
axis(2,c(0,0.25,0.5),las=1)

h <- hist(log10(tmp03$exposu+1), breaks = 30, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h,main="Wave exposure", xlab="Bed stress (N/m2)",
     xlim=c(0,6),ylim=c(0,0.5),axes=T,yaxt="n")
axis(2,c(0,0.25,0.5),las=1)

dev.off()





