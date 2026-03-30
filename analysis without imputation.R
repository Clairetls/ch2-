
library(tidyverse)
library(lme4)
library(RODBC)
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

tblbirdid<-sqlFetch(swdb, 'tblBirdID', stringsAsFactors=F)



telomere_28_5 <- read.csv("telomere_28_5.csv")
# haem <- read_csv("haematocrit_28_5.csv")
provi <- read.csv("provisioning_28_5.csv", sep=";")
physio<-read.csv('physio_28_5.csv')


ars<-read.csv('ars_365.csv')
ars<-ars[,-c(1)]
ars<-ars%>% 
  arrange(BirdID, chickyear)%>%
  rename(occasionyear=chickyear)


tblbirdid$birthyear<-as.numeric(str_sub(tblbirdid$BirthDate, 1,4))
tblbirdid<-tblbirdid[,c('BirdID', 'birthyear')]
ars<-left_join(ars, tblbirdid, by='BirdID')
ars$age<-ars$occasionyear-ars$birthyear
ars$age[ars$age==-1]<-0


# select columns for each trait - body mass is in a different script 
# bodymass
telo1 <- telomere_28_5[,c('BirdID','RTL','Whodunnit', 'birthyear',
                          'occasionyear','age_year',"Status",
                          'newlifespan','TQ','SexEstimate','newstat',
                          'avg_invert','FieldPeriodID', 'PlateID')] 

buffy <- haem[,c('BirdID','BuffyCoat','Observer', 'birthyear',
                 'occasionyear','age',"Status",
                 'newlifespan','TQ','CatchTime','SexEstimate','newstat',
                 'avg_invert','FieldPeriodID')] 

provi1 <- provi[,c('BirdID','birthyear','occasionyear','age','newlifespan',
                   'prate','Observer', "Status",'TQ','SexEstimate','newstat',
                   'avg_invert','FieldPeriodID','BroodSize','nr_helpers','WatchType')] 

fatscore <- physio[,c('BirdID','FatScore','Observer','birthyear',
                      'occasionyear','age_year',"Status",
                      'newlifespan','TQ','SexEstimate','newstat',
                      'avg_invert','FieldPeriodID', "CatchTime", 'BodyMass')] 

buffy <- buffy %>% filter(!is.na(BuffyCoat) & SexEstimate == 0) 

physio<-physio[,-c(1)]

#birds with eggs 
eggbound<-sqlQuery(swdb, 'SELECT tblCatches.CatchID, tblCatches.BirdID, tblCatches.EggPresent
FROM tblCatches
WHERE (((tblCatches.EggPresent)=-1));
', stringsAsFactors==F)

#theres only 39 on cousin 

#remove from phys dataset 

phys_clean<-filter(physio, physio$CatchID %!in% eggbound$CatchID)


bodymass<-phys_clean[,c(1,2,3,11,12,22,25,26,28,29,31,33,35,36,37,38)]


###############################################

#getting relative lifetime fitness 


r<-log(0.9223668)  #value from leslie matrix from other script
e<-exp(1)

relativefit<-ars%>%
  group_by(BirdID)%>%
  mutate(w1=ars*e^(-r*age))

relativefit<-as.data.frame(relativefit)


lifetime_w<-data.frame()

for(i in unique(relativefit$BirdID)){
  onebird<-filter(relativefit, relativefit$BirdID==i)
  w1<-max(cumsum(onebird$w1))
  onerow<-data.frame(BirdID=i, w=w1)
  lifetime_w<-rbind(lifetime_w,onerow)
}

lrs<-ars%>%
  group_by(BirdID)%>%
  summarise(lrs=sum(ars))           

##############################################
#RTL

life_w<-read.csv('lifetimefitness.csv')
life_w<-life_w[,-c(1)]


telo_w<-left_join(telo1, life_w, by='BirdID')
telo_w<-left_join(telo_w, lrs, by='BirdID')


telo_w$w1[is.na(telo_w$w1)]<-0
telo_w$lrs[is.na(telo_w$lrs)]<-0



#########################
library(glmmTMB)
library(glmm)
library(lmerTest)

telo_mod<-telo_w

telo_mod$age_year[telo_mod$age_year>=12]<-12

telo_mod$logw<-log(telo_mod$w)

plate53<-filter(telo_mod, telo_mod$PlateID==53)

telo_mod<-filter(telo_mod, telo_mod$PlateID!=53)

telo_byage<-split(telo_mod, telo_mod$age_year)

telobyage <- lapply(telo_byage, function(x) {
  x$RTL_z <- scale(x$RTL)
  x$w1_z<-scale(x$w1)
  return(x)
})

# teloage0<-telobyage[[1]]
# 
test<-glmmTMB(w1_z ~ RTL_z, data=telobyage[[1]], family = gaussian(), ziformula = ~1)
summary(test)$coefficient$cond[2,c(1,4)]

modelfunc<-function(x){
  model<-glmmTMB(w1_z ~ RTL_z, data=x, family=gaussian(), ziformula = ~1)
  return(summary(model)$coefficient$cond[2,c(1,4)])
}

modelfunc2<-function(x){
  model<-glmmTMB(w1_z~RTL_z, data=x, family=gaussian(), ziformula = ~1)
  return(model)
}

modelfunc3<-function(x){
  model<-glmmTMB(lrs~RTL_z+SexEstimate, data=x, family=poisson(), 
                 ziformula= ~1)
  return(summary(model)$coefficient[2,c(1,4)])
}

modelfunc4<-function(x){
  model<-glmmTMB(lrs~RTL_z+SexEstimate, data=x, family=poisson(), 
                 ziformula=~1)
  return(model)
}



# age0mod<-glmmTMB(lrs~RTL_z+SexEstimate, data=telobyage[[1]], family=poisson(), 
#                  ziformula=~1)

telo_coeff<-lapply(telobyage, modelfunc) #weird results 

telo_coeff2<-lapply(telobyage, modelfunc2)

telo_coeff3<-lapply(telobyage, modelfunc3)  #modelling with LRS 

telo_mods4<-lapply(telobyage, modelfunc4)  #modelling with LRS 


t_coeff<-as.data.frame(do.call(rbind,telo_coeff))

t_coeff$age<-c(0:12)
plot(t_coeff$age, t_coeff$Estimate)



library(cowplot)
ggplot(t_coeff, aes(x=age, y=Estimate))+geom_point()+stat_smooth(method='lm') +labs(title='Telomere Length')+theme_cowplot()

age1model<-telo_coeff2[[2]]

names(telo_coeff2)<-c('teloage0','teloage1','teloage2','teloage3','teloage4','teloage5',
                     'teloage6','teloage7','teloage8','teloage9','teloage10','teloage11','teloage12')

summary(telo_coeff2[[1]])

############
#checking model fit 
library(DHARMa)

simulateResiduals(telo_coeff2[[8]], plot = T)

age1model
t_coeff<-as.data.frame(do.call(rbind,telo_coeff))

simulateResiduals(telo_coeff2[[12]], plot=T)

t_coeff$age<-c(0:12)
plot(t_coeff$age, t_coeff$Estimate)


telo_mod$age_year<-as.factor(telo_mod$age_year)

ggplot(telo_mod, aes(y = w, colour = age_year))+geom_histogram() 



#########################################################

#tarsus 









