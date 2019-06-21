
# script to make maps for manuscript

# load balticgrid_state
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  load("balticgrid_state.RData")

# code to derive Baltic map
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  worldMap <- getMap(resolution = "high")
  worldMap@data$id = rownames(worldMap@data)
  worldMap.points = tidy(x = worldMap, region = "id")
  worldMap.points <- subset(worldMap.points, worldMap.points$lat < 70 & worldMap.points$lat > 50)
  worldMap.points <- subset(worldMap.points, worldMap.points$long < 35 & worldMap.points$long > 5)
  worldMap.points <- worldMap.points [order(worldMap.points$order),] 
  
# cut off balticgrid in the edge 
  baltdat <- as.data.frame(balticgrid)
  baltdat2 <- data.frame(longitude = coordinates(balticgrid)[,1], latitude = coordinates(balticgrid)[,2])
  baltdat<-cbind(baltdat,baltdat2)
  baltdat$knip<- rep(1,nrow(baltdat))
  baltdat$knip[baltdat$longitude < 10.3 & baltdat$latitude > 57.3]<-NA
  baltdat$knip[baltdat$longitude < 10 & baltdat$latitude > 56.5]<-NA
  baltdat$knip[baltdat$longitude > 10 & baltdat$longitude<12 & baltdat$latitude > 57.8]<-NA
  baltdat<-subset(baltdat,!(is.na(baltdat$knip)))  
  
# figures to:
  setwd("C:/Users/pdvd/Online for git/Baltic/Output")

# map of median longevity whole community
  quat<-c(2.5,3,3.5,4,4.5,5,6,10,15)
  quat<-c(0.1,2.5,3,3.5,4,5,6,8,15.1)
  baltdat$cat<- cut(baltdat$medLong,c(quat))
  baltdat$cat<-as.factor(baltdat$cat)
  sealand <- c("#ffffd9","#edf8b1","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58")

  pdf("median_longevity_community.pdf",width=5.8,height=5.9)
  Longi <- ggplot()+ geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45) +
           scale_colour_manual(values=sealand)
  Longi <- Longi+geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
           coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  Longi <- Longi +  theme(plot.background=element_blank(),
           panel.background=element_blank(),
           axis.text.y   = element_text(size=16),
           axis.text.x   = element_text(size=16),
           axis.title.y  = element_text(size=16),
           axis.title.x  = element_text(size=16),
           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
           legend.text   = element_text(size=11),
           legend.title  = element_text(size=11))+
           scale_x_continuous(breaks=c(10,15,20,25))+
           scale_y_continuous(breaks=c(54,58,62,66))
  Longi <- Longi +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(Longi)
  dev.off()
  
# map of median longevity suspension feeders
  baltdat$cat<- cut(baltdat$medLong_susp,c(quat))
  baltdat$cat<-as.factor(baltdat$cat)
  
  pdf("median_longevity_suspfeeders.pdf",width=5.8,height=5.9)
  LSusp <- ggplot()+ geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45) +
           scale_colour_manual(values=sealand)
  LSusp <- LSusp+geom_polygon(data =  worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
           coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  LSusp <- LSusp +  theme(plot.background=element_blank(),
           panel.background=element_blank(),
           axis.text.y   = element_text(size=16),
           axis.text.x   = element_text(size=16),
           axis.title.y  = element_text(size=16),
           axis.title.x  = element_text(size=16),
           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
           legend.text   = element_text(size=11),
           legend.title  = element_text(size=11))+
           scale_x_continuous(breaks=c(10,15,20,25))+
           scale_y_continuous(breaks=c(54,58,62,66))
  LSusp <- LSusp +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(LSusp)
  dev.off()
  
# map of median longevity bioturbators
  baltdat$cat<- cut(baltdat$medLong_biot,c(quat))
  baltdat$cat<-as.factor(baltdat$cat)
  
  pdf("median_longevity_bioturbators.pdf",width=5.8,height=5.9)
  LBiot <- ggplot()+ geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45) +
           scale_colour_manual(values=sealand)
  LBiot <- LBiot+geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
           coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  LBiot <- LBiot +  theme(plot.background=element_blank(),
           panel.background=element_blank(),
           axis.text.y   = element_text(size=16),
           axis.text.x   = element_text(size=16),
           axis.title.y  = element_text(size=16),
           axis.title.x  = element_text(size=16),
           panel.border  = element_rect(colour = "grey", size=.5,fill=NA),
           legend.text   = element_text(size=11),
           legend.title  = element_text(size=11))+
           scale_x_continuous(breaks=c(10,15,20,25))+
           scale_y_continuous(breaks=c(54,58,62,66))
  LBiot <- LBiot +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(LBiot)
  dev.off()
  
  
  
  
  