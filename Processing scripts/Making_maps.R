
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
  
# get predicted benthic state
  baltdat$state[is.na(baltdat$state) & !(is.na(baltdat$depth))] <- 1
  quat<-c(-1,0.001,0.1,0.2,0.4,0.6,0.8,1.1)
  baltdat$cat<- as.factor(cut(1-baltdat$state,quat,right=F))
  colscale <- c("#efedf5","#dadaeb","#bcbddc","#9e9ac8","#807dba","#6a51a3","#54278f","#3f007d")
  
  pdf("community state.pdf",width=5.8,height=5.9)
  figmap <- ggplot() + geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45,na.rm=T) +
            scale_colour_manual(values=colscale,na.value = "white",name  ="State",
            labels=c("1", "0.9-1","0.8-0.9","0.6-0.8","0.4-0.6","0.2-0.4","<0.2",""))
  figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
            coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  figmap <- figmap +  theme(plot.background=element_blank(),
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
  figmap <- figmap +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(figmap)
  dev.off()
  
# get minimum oxygen concentration
  quat<-c(-1,0.01,0.5,1,2,3,4,15)
  baltdat$cat<- as.factor(cut(baltdat$min_oxygen,quat,right=FALSE))
  bluegreen <- c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  
  pdf("Minimum oxygen.pdf",width=5.8,height=5.9)
  figmap <- ggplot() + geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45,na.rm=T) +
            scale_colour_manual(values=bluegreen,na.value = "#f7fcfd",name  ="Oxygen conc. \n ml O2 L-1",
            labels=c("0", "0-0.5","0.5-1","1-2","2-3","3-4",">4"))
  figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
            coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  figmap <- figmap +  theme(plot.background=element_blank(),
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
  figmap <- figmap +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(figmap)
  dev.off()
  
# get fishing abrasion
  quat<-c(-1,0,0.1,0.5,1,5,10,100)
  baltdat$cat<- as.factor(cut(baltdat$surface_SAR,quat,right=FALSE))
  bluegreen <- c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  
  pdf("Fishing abrasion.pdf",width=5.8,height=5.9)
  figmap <- ggplot() + geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45,na.rm=T) +
            scale_colour_manual(values=bluegreen,na.value = "#f7fcfd",name  ="Fishing intensity \n (per year)",
            labels=c("0-0.1", "0.1-0.5","0.5-1","1-5","5-10",">10","no fishing"))
  figmap <- figmap +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
            coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  figmap <- figmap +  theme(plot.background=element_blank(),
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
  figmap <- figmap +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(figmap)
  dev.off()
  
# depth 
  baltdat$depth[baltdat$depth > -0.5]<- -0.5
  baltdat$depth <- abs(baltdat$depth)
  baltdat$depth <-as.numeric(baltdat$depth)
  quat<-c(-1,20,40,60,100,200,300,1000)
  baltdat$cat<- as.factor(cut(baltdat$depth,quat,right=FALSE))
  bluegreen <- c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  
  pdf("Depth.pdf",width=5.8,height=5.9)
  Depth <- ggplot() + geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45,na.rm=T) +
           scale_colour_manual(values=bluegreen,na.value = "#f7fcfd",name  ="Depth (meters)",
           labels=c("< 20", "20-40","40-60","60-100","100-200","200-300",">300"))
  Depth <- Depth +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
           coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  Depth <- Depth +  theme(plot.background=element_blank(),
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
  Depth <- Depth +   guides(colour = guide_legend(override.aes = list(size=5)))
  print(Depth)
  dev.off()

# wave exposure at the seabed
  pdf("wave exposure seabed.pdf",width=5.8,height=5.9)
  Wave <- ggplot() + geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=log10(exposu+1)),shape=15,size=.45,na.rm=T) +
          scale_colour_gradient(low="light blue", high= "dark blue",na.value = "white",name="Wave exposure",
          breaks=c(1,3,5), labels=c(10, 1000,100000))
  Wave <- Wave +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
          coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  Wave <- Wave +  theme(plot.background=element_blank(),
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
  print(Wave)
  dev.off()

# Salinity at the seabed
  pdf("Salinity.pdf",width=5.8,height=5.9)
  Salinity <- ggplot() + geom_point(data=baltdat, aes(x=longitude, y=latitude, colour=Bsalinity),shape=15,size=.45,na.rm=T) +
              scale_colour_gradient2(low="red", mid = "light blue", high= "blue",midpoint=10,
              na.value = "white",name="Salinity (ppt)")
  Salinity <- Salinity +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
              coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  Salinity <- Salinity +  theme(plot.background=element_blank(),
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
  print(Salinity)
  dev.off()
  
# now plot MSFD habitats 
  tr <-baltdat
  tr<-subset(tr,!(is.na(tr$msfd_habitat)))
  tr$cat <- as.character(tr$msfd_habitat)
  tr$cat[!(as.factor(tr$msfd_habitat) %in% c("Circalittoral mixed sediment","Circalittoral mud",
                                        "Offshore circalittoral mud","Circalittoral sand",
                                        "Infralittoral mixed sediment",
                                        "Offshore circalittoral mixed sediment"))]<- "Others"
  
  msfdcol <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69")
  
  msfdhab <- ggplot() + geom_point(data=tr, aes(x=longitude, y=latitude, colour=factor(cat)),shape=15,size=.45,na.rm=T) +
    scale_colour_manual(values=msfdcol,na.value = "#f7fcfd",name  ="Main habitats")
  msfdhab <- msfdhab +  geom_polygon(data = worldMap.points, aes(x = long, y = lat, group = group),color="dark grey",fill="light grey")+
    coord_map(xlim=c(8, 28), ylim=c(53, 66.5))
  msfdhab <- msfdhab +  theme(plot.background=element_blank(),
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
  msfdhab<- msfdhab +   guides(colour = guide_legend(override.aes = list(size=5)))
  
  
  