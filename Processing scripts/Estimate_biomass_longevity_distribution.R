rm(list=ls())

# script to estimate the longevity biomass composition as a function of environmental conditions
# (only included models that are presented in the manuscript)

  setwd("C:/Users/pdvd/Online for git/Baltic/Data")
  load("benthic_data_gogina.Rdata")

# include minimum oxygen in the year
  Bstations <- transform(Bstations, min = pmin(Bstations$OxyigenWint,Bstations$OxyigenSpring,Bstations$OxyigenSummer,Bstations$OxyigenAutumn))
  colnames(Bstations)[31]<-"Min_oxygen"
  Bstations$Min_oxygen[Bstations$Min_oxygen < 0] <- 0 
  
# get longevity categories seperate for each station 
  ID        <-rep(Bstations$Cell_code,3)
  Depth     <-rep(log(abs(Bstations$depth-1)),3)
  SAR       <-rep(Bstations$surface_SAR,3)
  Gravel    <-rep(Bstations$gravel/100,3)
  Sand      <-rep(Bstations$sand/100,3)
  Mud       <-rep(Bstations$mud/100,3)
  Rock      <-rep(Bstations$rock,3)
  Cumb      <-c(Bstations$L.1,(Bstations$L.1+Bstations$L1.3),(Bstations$L.1+Bstations$L1.3+Bstations$L3.10))
  Cumb_sus  <-c(Bstations$SU_L.1,(Bstations$SU_L.1+Bstations$SU_L1.3),(Bstations$SU_L.1+Bstations$SU_L1.3+Bstations$SU_L3.10))
  Cumb_bt   <-c(Bstations$BT_L.1,(Bstations$BT_L.1+Bstations$BT_L1.3),(Bstations$BT_L.1+Bstations$BT_L1.3+Bstations$BT_L3.10))
  Longevity <-c(rep(1,nrow(Bstations)),rep(3,nrow(Bstations)),rep(10,nrow(Bstations)))
  Stress    <-rep(log(Bstations$expos+1),3)
  Salinity  <-rep(Bstations$Bsalinity/10,3)
  Oxygen    <-rep(Bstations$Min_oxygen,3) 

  Master    <-data.frame(ID,Depth,SAR,Gravel,Sand,Mud,Rock,Cumb,Cumb_sus,Cumb_bt,
                         Longevity,Stress,Salinity,Oxygen)
  Master$ll <-log(Master$Longevity)

# select all stations with relatively undisturbed conditions
  dat_undist<-subset(Master,Master$SAR <0.1) # all stations fished on average >= 0.1 per year are excluded
  dat_undist<-subset(dat_undist,dat_undist$Oxygen > 3.2) # all stations with oxygen conditions <= 3.2 in one season are excluded

# WHOLE BENTHIC COMMUNITY
#########################
  dat_whole <-dat_undist[complete.cases(dat_undist[ , "Cumb"]),]
  
# add a small number to values very close to 0 and 1 
  for (i in 1:(nrow(dat_whole))){
    if (dat_whole$Cumb[i] < 1e-3){ dat_whole$Cumb[i] <- 1e-3}
    if (dat_whole$Cumb[i] > 0.999){dat_whole$Cumb[i] <- 0.999}
  } 

# load library for mixed models 
  library(lme4)
  
# test different models (in all models, the random effect is very small (singular fit) but in 
# principle has to be included (hence singular fit is ignored ->> "action = ignore")
  mod1   <-  glmer(Cumb ~ ll + Salinity + Depth + Stress + (1 | ID), 
                   data=dat_whole, family=binomial,  
                   control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000),
                                        check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
  mod2 <- update(mod1, ~ . + ll*Salinity)
  mod3 <- update(mod1, ~ . + ll*Salinity + Depth*Salinity)
  mod4 <- update(mod1, ~ . + ll*Salinity + Depth*Salinity + ll*Depth) ## <-- best model
  mod5 <- update(mod1, ~ . + ll*Salinity + Depth*Salinity + ll*Depth - Stress)

  # get confidence interval assuming normality
  modcf  <- confint(mod4, method="Wald")
  modcf <- matrix(data = unlist(modcf),nrow=9,ncol=2)
  modcf <- cbind(modcf[2:9,],t(coef(mod4)$ID[1,1:8]))
  colnames(modcf) <- c("lower2.5","upper97.5","mean")
  rownames(modcf) <- colnames(coef(mod4)$ID[1,])
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  save(modcf,file="Model_output_whole_community.Rdata")

# suspension feeders
#########################
  susp <-dat_undist[complete.cases(dat_undist[ , "Cumb_sus"]),]
  
# add a small number to values very close to 0 and 1 
  for (i in 1:(nrow(susp))){
    if (susp$Cumb_sus[i] < 1e-3){ susp$Cumb_sus[i] <- 1e-3}
    if (susp$Cumb_sus[i] > 0.999){susp$Cumb_sus[i] <- 0.999}
  } 
  
# load library for mixed models 
  library(lme4)
  
# test different models (in all models, the random effect is very small (singular fit) but in 
# principle has to be included (hence singular fit is ignored ->> "action = ignore")
  mod1   <-  glmer(Cumb_sus ~ ll + Salinity + Depth + Stress + (1 | ID), 
                   data=susp, family=binomial,  
                   control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000),
                                        check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
  mod2 <- update(mod1, ~ . + ll*Salinity)
  mod3 <- update(mod1, ~ . + ll*Salinity + Depth*Salinity)
  mod4 <- update(mod1, ~ . + ll*Salinity + Depth*Salinity + ll*Depth) ## <-- best model
  mod5 <- update(mod1, ~ . + ll*Salinity + Depth*Salinity + ll*Depth - Stress)

# get confidence interval assuming normality
  modcf_susp  <- confint(mod4, method="Wald")
  modcf_susp <- matrix(data = unlist(modcf_susp),nrow=9,ncol=2)
  modcf_susp <- cbind(modcf_susp[2:9,],t(coef(mod4)$ID[1,1:8]))
  colnames(modcf_susp) <- c("lower2.5","upper97.5","mean")
  rownames(modcf_susp) <- colnames(coef(mod4)$ID[1,])
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  save(modcf_susp,file="Model_output_suspensionF.Rdata") 
  
# bioturbators
#########################
  biot <-dat_undist[complete.cases(dat_undist[ , "Cumb_bt"]),]
  
# add a small number to values very close to 0 and 1 
  for (i in 1:(nrow(biot))){
    if (biot$Cumb_bt[i] < 1e-3){ biot$Cumb_bt[i] <- 1e-3}
    if (biot$Cumb_bt[i] > 0.999){biot$Cumb_bt[i] <- 0.999}
  } 
  
# load library for mixed models 
  library(lme4)
  
# test different models (in all models, the random effect is very small (singular fit) but in 
# principle has to be included (hence singular fit is ignored ->> "action = ignore")
  mod1   <-  glmer(Cumb_bt ~ ll + Salinity + Depth + Stress + (1 | ID), 
                   data=biot, family=binomial,  
                   control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000),
                                        check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
  mod2 <- update(mod1, ~ . + ll*Salinity)
  mod3 <- update(mod1, ~ . + ll*Depth)
  mod4 <- update(mod1, ~ . + Depth*Salinity + ll*Depth) ## <-- best model
  mod5 <- update(mod1, ~ . + ll*Salinity + ll*Depth)

# get confidence interval assuming normality
  modcf_biot  <- confint(mod4, method="Wald")
  modcf_biot <- matrix(data = unlist(modcf_biot),nrow=8,ncol=2)
  modcf_biot <- cbind(modcf_biot[2:8,],t(coef(mod4)$ID[1,1:7]))
  colnames(modcf_biot) <- c("lower2.5","upper97.5","mean")
  rownames(modcf_biot) <- colnames(coef(mod4)$ID[1,])
  setwd("C:/Users/pdvd/Online for git/Baltic/Processed data")
  save(modcf_biot,file="Model_output_bioturbators.Rdata")
  
  
  
  