#testing whether linear model works 

library(RODBC)
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

tblbirdid<-sqlFetch(swdb, 'tblBirdID', stringsAsFactors=F)


#why is the data missing
#catch at random? 
#age: determines condition, exploration, catch rates 
#but body mass can also determine if body mass is missing (if it is alive?)
#also personality of bird 
#what kind of missing is this 
#if the missing is dependent on age (and maybe age only?) 
#it could be missing at random or missing not at random. 

# install.packages('psych')
library(psych)
library(Amelia)
library(mice)
library(tidyverse)

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


#get lrs 
lrs<-ars%>%
  group_by(BirdID)%>%
  summarise(lrs=sum(ars))


physio<-physio[,-c(1)]

bodymass<-physio[,c(1,2,11,12,22,25,26,28,29,31,33,35,36,37,38)]

# missingdf<-physio[,c('newlifespan','age_year','BodyMass',
#                      'RightTarsus','avg_invert')]

hist(bodymass$age_year)
hist(bodymass$lifespan)


# bodymass$bodymass_z<-scale(bodymass$BodyMass)
bm<-left_join(bodymass, ars, by=c('BirdID', 'occasionyear'))
bm$age_year[bm$age_year>=14]<-14



#get relative lifetime fitness 
r<-0.9223668  #value from leslie matrix from other script
e<-exp(1)

relativefit<-ars%>%
  group_by(BirdID)%>%
  mutate(w1=ars*e^(-r*age), 
         w2=0.5*ars*e^(-r*age))



relativefit<-as.data.frame(relativefit)

lifetime_w<-data.frame()

for(i in unique(relativefit$BirdID)){
  onebird<-filter(relativefit, relativefit$BirdID==i)
  w1<-max(cumsum(onebird$w1))
  half_w<-max(cumsum(onebird$w2))
  onerow<-c(i, w1, half_w)
  lifetime_w<-rbind(lifetime_w,onerow)
}


colnames(lifetime_w)<-c("BirdID",'w1','half_w')


write.csv(lifetime_w, "lifetimefitness.csv")


#merge relative fitness data with body mass dataframe 

bm_lrs<-left_join(bm, lifetime_w, by='BirdID')
bm_lrs<-left_join(bm_lrs, lrs, by="BirdID")

##

library(glmmTMB)
library(lme4)
library(lmerTest)
library(brms)
library(MCMCglmm)
  

bm_lrs$w1[is.na(bm_lrs$w1)]<-0
bm_lrs$half_w[is.na(bm_lrs$half_w)]<-0
bm_lrs$lrs[is.na(bm_lrs$lrs)]<-0



bmlrsbyage<-split(bm_lrs, bm_lrs$age_year)


bmlrsbyage <- lapply(bmlrsbyage, function(x) {
  x$bodymass_z <- scale(x$BodyMass)
  x$w1_z<-scale(x$w1_z)
  x$halfw_z<-scale(x$half_w)
  return(x)
})


bm_w1_model<-function(x){
  model<-glmmTMB(w1_z~bodymass_z+SexEstimate.x, data=x, family=gaussian(), 
            ziformula=~1)
  return(summary(model)$coefficient$cond[2,c(1,4)])
}


bmw1_coeff<-lapply(bmlrsbyage, bm_w1_model)

w1<-as.data.frame(do.call(rbind,bmw1_coeff))
plot(w1)

#############################################
#half w 

bm_halfw_model<-function(x){
  model<-glmmTMB(halfw_z~bodymass_z+SexEstimate.x, data=x, family=gaussian(), 
                 ziformula=~1)
  return(summary(model)$coefficient$cond[2,c(1,4)])
}


bm_halfw_coeff<-lapply(bmlrsbyage, bm_halfw_model)

halfw<-as.data.frame(do.call(rbind,bm_halfw_coeff))
plot(halfw)


###########################################
#lrs 
lifetimerepro<-lrs

bmlrs_model<-function(x){
  model<-glmmTMB(lrs~bodymass_z+SexEstimate.x, data=x, family="poisson", 
                 ziformula=~1)
  return(summary(model)$coefficient$cond[2,c(1,4)])
}


bmlrs_coeff<-lapply(bmlrsbyage, bmlrs_model)

lrs_coeff<-as.data.frame(do.call(rbind,bmlrs_coeff))
plot(lrs_coeff[,1])


######################################
#ages binned 

bm_lrs2<-bm_lrs%>%
  mutate(age_cat=case_when(age_year<3 ~ 'Young', 
                           age_year>=3 & age_year<=4 ~'Prime', 
                           age_year>=5 & age_year <=7 ~ 'PostPrime', 
                           age_year>=8 & age_year <=12 ~ "Old", 
                           age_year>=13 ~ " Very Old"))



bmlrsbyagecat<-split(bm_lrs2, bm_lrs2$age_cat)

#scale variables 
bmlrsbyagecat <- lapply(bmlrsbyagecat, function(x) {
  x$bodymass_z <- scale(x$BodyMass)
  # x$w1_z<-scale(x$w1_z)
  x$halfw_z<-scale(x$half_w)
  return(x)
})

#
w1agecat_model<-function(x){
  model<-glmmTMB(w1_z~bodymass_z+SexEstimate.x, data=x, family=gaussian(), 
                 ziformula=~1)
  return(summary(model)$coefficient$cond[2,c(1,4)])
}


w1agecat_coeff<-lapply(bmlrsbyagecat, w1agecat_model)

w1_cat<-as.data.frame(do.call(rbind,w1agecat_coeff))
w1_cat<-c(w1_cat[5,1],w1_cat[4,1],w1_cat[3,1],w1_cat[2,1],w1_cat[1,1] )
plot(w1_cat)


#############################################
#lrs

lrs_cat_mods<-lapply(bmlrsbyagecat, bmlrs_model)

lrscat_coeff<-as.data.frame(do.call(rbind,lrs_cat_mods))
lrscatplot<-c(lrscat_coeff[5,1],lrscat_coeff[4,1],lrscat_coeff[3,1],
              lrscat_coeff[2,1],lrscat_coeff[1,1] )

plot(lrscatplot)




#how many birds have more than one observation per age? 
bleh<-bm%>%
  group_by(BirdID, age_year)%>%
  summarise(total=length(BirdID))
#conclusion: no need to fit bird id in models 



#assigned all ARS with NA as 0 because no data in pedigree means no ars. 
bm$ars[is.na(bm$ars)]<-0
bmbyage<-split(bm, bm$age_year)


#scale within the df 
trial <- lapply(bmbyage, function(x) {
  x$bodymass_z <- scale(x$BodyMass)
  return(x)
})


#check that scaled correctly 
testdf<-bmbyage[[1]]
testdf$bodymass_z<-scale(testdf$BodyMass)








bm$age_year<-as.factor(bm$age_year)



#lmer per age 
age1df<-trial[[1]]
age1df<-filter(age1df, age1df$birthyear>1996)
age1df$SexEstimate<-as.factor(age1df$SexEstimate)



# CHANGE ALL SEX ESTIMATE TO SEXESTIMATE.X

bmage1<-glm(ars~bodymass_z*SexEstimate.x+(1 | occasionyear), age1df, family='poisson')
summary(bmage1)

 # 0.02248644 sig 
#0.4442 sig


View(trial[[2]])
trial[[2]]$SexEstimate.x<-as.factor(trial[[2]]$SexEstimate.x)
bmage2<-glm(ars~bodymass_z*SexEstimate.x+(1|occasionyear), trial[[2]],family='poisson')
summary(bmage2)

#0.02879 not sig 
#0.08814 not sig 

trial[[3]]$SexEstimate.x<-as.factor(trial[[3]]$SexEstimate.x)
bmage3<-glm(ars~bodymass_z*SexEstimate.x, trial[[3]], family='poisson')
summary(bmage3)

#0.03967 not sig 
0.06718 #not sig 

trial[[4]]$SexEstimate.x<-as.factor(trial[[4]]$SexEstimate.x)
bmage4<-glm(ars~bodymass_z*SexEstimate.x, trial[[4]], family='poisson')
summary(bmage4)

#-0.09463 not sig 
-0.1792  #not sig 


trial[[5]]$SexEstimate.x<-as.factor(trial[[5]]$SexEstimate.x)
bmage5<-glm(ars~bodymass_z*SexEstimate.x, trial[[5]], family='poisson')
summary(bmage5)

#-0.01115  not sig 
-0.03752  #not sig

trial[[6]]$SexEstimate.x<-as.factor(trial[[6]]$SexEstimate.x)
bmage6<-glm(ars~bodymass_z*SexEstimate.x, trial[[6]], family='poisson')
summary(bmage6)

# 0.04574   not sig 
0.12983   #  not sig

trial[[7]]$SexEstimate.x<-as.factor(trial[[7]]$SexEstimate.x)
bmage7<-glm(ars~bodymass_z*SexEstimate.x, trial[[7]], family='poisson')
summary(bmage7)

#-0.01435 not sig 
#-0.03410 not sig 

trial[[8]]$SexEstimate.x<-as.factor(trial[[8]]$SexEstimate.x)
bmage8<-glm(ars~bodymass_z*SexEstimate.x, trial[[8]], family='poisson')
summary(bmage8)

#-0.0005231 not sig
-0.001406 # not sig

trial[[9]]$SexEstimate.x<-as.factor(trial[[9]]$SexEstimate.x)
bmage9<-glm(ars~bodymass_z*SexEstimate.x, trial[[9]], family='poisson')
summary(bmage9)

#0.007164 not sig 
0.03213    # not sig 

trial[[10]]$SexEstimate.x<-as.factor(trial[[10]]$SexEstimate.x)
bmage10<-glm(ars~bodymass_z*SexEstimate.x, trial[[10]], family='poisson')
summary(bmage10)

#0.000969   not sig 
0.0047     #not sig 

trial[[11]]$SexEstimate.x<-as.factor(trial[[11]]$SexEstimate.x)
bmage11<-glm(ars~bodymass_z*SexEstimate.x, trial[[11]], family='poisson')
summary(bmage11)
# -0.03083 not sig
-0.1384 # not sig 

trial[[12]]$SexEstimate.x<-as.factor(trial[[12]]$SexEstimate.x)
bmage12<-glm(ars~bodymass_z*SexEstimate.x, trial[[12]], family='poisson')
summary(bmage12)

#-0.3631 not sig 
-2.752      # not sig 

trial[[13]]$SexEstimate.x<-as.factor(trial[[13]]$SexEstimate.x)
bmage13<-glm(ars~bodymass_z*SexEstimate.x, trial[[13]], family='poisson')
summary(bmage13)

#-0.02002 not sig
-0.06731 # not sig

trial[[14]]$SexEstimate.x<-as.factor(trial[[14]]$SexEstimate.x)
bmage14<-glm(ars~bodymass_z*SexEstimate.x, trial[[14]], family='gaussian')
summary(bmage14)

# 0.05523  not sig 

#warning for poisson model suggesting data was linearly separable 

test<-data.frame(estimate=c(0.02248644, 0.02879, 0.03967, -0.09463, -0.01115, 
                      0.04574, -0.01435,-0.0005231,0.007164,0.000969,
                      -0.03083,-0.3631,-0.02002, 0.05523), 
           age=c(1:14))


test2<-data.frame(estimate=c(0.4442, 0.08814, 0.06718, -0.1792, -0.03752,
                             0.12983, -0.03410, -0.001406, 0.03213, 0.0047, 
                             -0.1384, -0.06731, -2.752,0.05523), 
                  age=c(1:13))

# test<-lmer(ars~age_year:bodymass_z+(1|BirdID), data = bm,
#            control=lmerControl())

#for separate models per age 
plot(test$age, test$estimate)

plot(test2$age, test2$estimate)


######################################################
#trying to bin ages into 5 categories to increase sample size 
#bin age 1-2, 3-4 as prime, 5-7 as starting decline, 8-12, and 13+ 

View(bm)
bm$age_year<-as.numeric(bm$age_year)

bm2<-bm%>%
  mutate(age_cat=case_when(age_year<3 ~ 'Young', 
                           age_year>=3 & age_year<=4 ~'Prime', 
                           age_year>=5 & age_year <=7 ~ 'PostPrime', 
                           age_year>=8 & age_year <=12 ~ "Old", 
                           age_year>=13 ~ " Very Old"))

bm_bycat<-split(bm2, bm2$age_cat)

bm_vold<-bm_bycat[[1]]
bm_old<-bm_bycat[[2]]
bm_postprime<-bm_bycat[[3]]
bm_prime<-bm_bycat[[4]]
bm_young<-bm_bycat[[5]]

bm_young$bodymass_z<-scale(bm_young$BodyMass)
bm_prime$bodymass_z<-scale(bm_prime$BodyMass)
bm_postprime$bodymass_z<-scale(bm_postprime$BodyMass)
bm_old$bodymass_z<-scale(bm_old$BodyMass)
bm_vold$bodymass_z<-scale(bm_vold$BodyMass)


#young
youngmod<-glm(ars~bodymass_z+SexEstimate, bm_young, family='poisson')
summary(youngmod)

#0.24635 (significant)

#prime
primemod<-glm(ars~bodymass_z+SexEstimate, bm_prime, family='poisson')
summary(primemod)

#0.01094    


postprimemod<-glm(ars~bodymass_z+SexEstimate, bm_postprime, family='poisson')
summary(postprimemod)

#0.11412    


oldmod<-glm(ars~bodymass_z+SexEstimate, bm_old, family='poisson')
summary(oldmod)

#-0.07761

voldmod<-glm(ars~bodymass_z+SexEstimate, bm_vold,family='poisson')
summary(voldmod)

#0.8085   (significant)

y<-c(0.24635, 0.01094,0.11412,-0.07761,0.8085)
x<-c(1,2,3,4,5)

plot(x,y)








#lizzie's code for phenotypic selection per age class 
## Univariate with interaction between age class and antler form
# absFCH11 <- MCMCglmm(ABS ~ Age + Form:AgeClass + LastSeen -1, 
#                      random=~Year+Code,rcov=~units,family="poisson",
#                      prior=prior1,data=DataFormABS,pedigree=pedigree,
#                      nitt=305000,thin=100,burnin=5000)






testbayes<-brm(ars~bodymass_z*age_year+(1|BirdID), 
               data = bm, family=gaussian())






#maybe ilmerTest#maybe i should add the rows that are bird alive but not caught 
missingness<-ifelse(is.na(missingdf)==TRUE, 0,1)
missdata<-data.frame(missingdf,missingness)
pairs.panels(missdata, ellipses=F, method = 'spearman')


#find out which birds are missing what years 
#create rows for them (major field season)
#fill in major field season 


ped<-read.csv('updated pedigree.csv', sep=';')
ped<-filter(ped, ped$GenDadConfidence>= 80 & ped$GenMumConfidence >=80)
oldped<-read.csv('C:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/repro_28_5.csv')
length(unique(oldped$BirdID))
length(unique(ped$GeneticFather))
