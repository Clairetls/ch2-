bm_impu<-read.csv('bm_impu.csv')
bm_impu<-bm_impu[,-c(1)]

library(tidyverse)
library(mice)

bm_impu$SexEstimate<-as.factor(bm_impu$SexEstimate)

####
bm_impu$rownr<-row.names(bm_impu)
complete_tars<-filter(bm_impu, !is.na(bm_impu$RightTarsus))
sample_tars<-complete_tars[sample(1:nrow(complete_tars), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 
sample_tars2<-complete_tars[sample(1:nrow(complete_tars), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 



#make predictor matrix for imputation 
cv_pred<-make.predictorMatrix(bm_impu)

exclude <- c('birthyear', 'Observer', 'occasionyear')

cv_pred[, exclude] <- 0

cv_pred[c('BodyMass','RightTarsus'), "BirdID"] <- -2  # Random intercept group indicator
cv_pred["BirdID",c('BodyMass','RightTarsus')] <- -2  

diag(cv_pred) <- 0

cv_method<-make.method(cv_pred)

cv_method <- c(
  BirdID = "",              # ID — exclude
  birthyear = "",           # exclude
  occasionyear = "",        # exclude
  age_year = "",            # derived
  newlifespan = "",         # fixed per bird
  newFPID = "",          # continuous
  newstat = "polyreg",      # categorical (factor with >2 levels)
  BodyMass = "2l.lmer",        # continuous - random forest/also can use pmm
  RightTarsus = "2l.lmer",      # continuous
  Observer = "",            # character — exclude or factor if needed
  SexEstimate = "logreg",   # categorical - 2 levels factor
  CatchTime_mins = "pmm",   # numeric  these are fine 
  new_bug = "pmm",           # numeric  this also fine
  rownr=''
)

cv_func<-function(x){
  bm_impu[c(x),'RightTarsus']<-NA 
  impu <- mice(bm_impu, method = cv_method, predictorMatrix = cv_pred, m = 5) #needs imputation.r to run 
  completeimpu<-complete(impu, action='long', include=T, all=T)
  onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
  return(onepoint)
}


# cv_others<-function(x, impudata=impudata, trait='trait', cv_method=cv_method, cv_pred=cv_pred){
#   cvdf<-data.frame()
#   impudata[c(x),trait]<-NA 
#   impu <- mice(impudata, method = cv_method, predictorMatrix = cv_pred, m = 5) #needs imputation.r to run 
#   completeimpu<-complete(impu, action='long', include=T, all=T)
#   onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
#   cvdf<-rbind(onepoint, cvdf)
#   return(cvdf)
# }






tarspts<-list()

tarspts<-lapply(sample_tars$rownr, FUN = cv_func)  #should i use lapply instead?? 

tarspts2<-lapply(sample_tars2$rownr, FUN = cv_func)  #should i use lapply instead?? 

# assign names
tarspts<-do.call(rbind.data.frame, tarspts)
tarspts2<-do.call(rbind.data.frame, tarspts2)


tars_op<-sample_tars[,c('BirdID', 'RightTarsus', 'rownr')]
colnames(tars_op)[colnames(tars_op)=='RightTarsus']<-'og_tars'


tarspts<-filter(tarspts, tarspts$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

tarspts<-left_join(tarspts, tars_op, by=c("BirdID",'rownr'))

ggplot(tarspts, aes(x=og_tars, y=RightTarsus))+geom_point()+stat_smooth(method='lm')

cor(tarspts$og_tars, tarspts$RightTarsus)
#0.7332804

aaa<-lm(og_tars~RightTarsus, tarspts)
summary(aaa)

# 0.7142075
# 0.6594864
####################################

tars_op2<-sample_tars2[,c('BirdID', 'RightTarsus', 'rownr')]
colnames(tars_op2)[colnames(tars_op2)=='RightTarsus']<-'og_tars'


tarspts2<-filter(tarspts2, tarspts2$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

tarspts2<-left_join(tarspts2, tars_op2, by=c("BirdID",'rownr'))

ggplot(tarspts2, aes(x=og_tars, y=RightTarsus))+geom_point()+stat_smooth(method='lm')

cor(tarspts2$og_tars, tarspts2$RightTarsus)
# 0.7400665




############# telomeres #############################

cv_others <- function(samplepts=samplepts, impudata=impudata, trait='trait', cv_method=cv_method, cv_pred=cv_pred) {
  
  results <- lapply(samplepts, function(x) {
    
    impudata[c(x),trait]<-NA 
    impu <- mice(impudata, method = cv_method, predictorMatrix = cv_pred, m = 5) #needs imputation.r to run 
    completeimpu<-complete(impu, action='long', include=T, all=T)
    onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
    return(onepoint)
  })
  
  cvpoints<-do.call(rbind.data.frame, results)
  return(cvpoints)
}


View(teloimpu)


teloimpu$rownr<-row.names(teloimpu)
complete_telo<-filter(teloimpu, !is.na(teloimpu$RTL))
sample_telo<-complete_telo[sample(1:nrow(complete_telo), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 
sample_telo2<-complete_telo[sample(1:nrow(complete_telo), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 



telo_pred<-make.predictorMatrix(teloimpu)

teloexclude <- c('birthyear', 'FPID','occasionyear')

telo_pred[, teloexclude] <- 0

telo_pred[c('RTL'), "BirdID"] <- -2  # Random intercept group indicator
telo_pred["BirdID",c('RTL')] <- -2  

diag(telo_pred) <- 0

telo_method<-make.method(telo_pred)

telo_method <- c(
  BirdID = "",              # ID — exclude
  birthyear = "",           # exclude
  occasionyear = "",        # exclude
  age_year = "",            # derived
  newlifespan = "",         # fixed per bird
  FPID = "",          # continuous
  newstat = "polyreg",      # categorical (factor with >2 levels)
  RTL = "2l.lmer",      # continuous
  Whodunnit='logreg',        #categorical - 2 levels factor (im not sure if this will work actually)
  SexEstimate = "logreg",   # categorical - 2 levels factor
  new_bug = "pmm",           # numeric  this also fine
  rownr=''
)

telopoints<-cv_others(samplepts = sample_telo$rownr, impudata = teloimpu, trait = 'RTL', cv_method = telo_method, cv_pred=telo_pred)




teloop<-sample_telo[,c('BirdID', 'RTL', 'rownr')]
colnames(teloop)[colnames(teloop)=='RTL']<-'og_rtl'


telopoints<-filter(telopoints, telopoints$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

telopoints<-left_join(telopoints, teloop, by=c("BirdID",'rownr'))

ggplot(telopoints, aes(x=og_rtl, y=RTL))+geom_point()+stat_smooth(method='lm')

cor(telopoints$og_rtl, telopoints$RTL)

#poor prediction as most variables have weak relationship with RTL 

ggplot(telopoints, aes(x=occasionyear, y=RTL))+geom_point()+stat_smooth(method='lm')
ggplot(telopoints, aes(x=age_year, y=RTL))+geom_point()+stat_smooth(method='lm')
ggplot(telopoints, aes(x=newlifespan, y=RTL))+geom_point()+stat_smooth(method='lm')
ggplot(telopoints, aes(x=Whodunnit, y=RTL))+geom_point()+stat_smooth(method='lm')
ggplot(telopoints, aes(x=new_bug, y=RTL))+geom_point()+stat_smooth(method='lm')
ggplot(telopoints, aes(x=newstat, y=RTL))+geom_point()+stat_smooth(method='lm')
ggplot(telopoints, aes(x=SexEstimate, y=RTL))+geom_point()+stat_smooth(method='lm')
#solution?? 
#power analysis and just run analysis 

##################################################################################
#cross validation for provisioning rate 
# provimpu2<-provimpu

provimpu<-read.csv('provimpu.csv')
provimpu<-provimpu[,-c(1)]

provimpu$rownr<-row.names(provimpu)

provi_pred<-make.predictorMatrix(provimpu)
provi_pred[c('prate'), "BirdID"] <- -2  # Random intercept group indicator
provi_pred["BirdID",c('prate')] <- -2 

#values in predictor: 0 means not used in imputation and 1 means used.
provi_exclude <- c('birthyear', 'Observer', 'occasionyear')

#originally tried to include nr helpers and brood size as predictors, but seems to not impute
#perhaps too many NAs and not enough variation. 


provi_pred[, provi_exclude] <- 0

#method for imputation 
provi_method<-make.method(provi_pred)

provi_method <- c(
  BirdID = "",              # ID — exclude
  birthyear = "",           # exclude
  occasionyear = "",        # exclude
  age_year = "",            # derived
  newlifespan = "",         # fixed per bird
  FPID = "",          # continuous
  newstat = "polyreg",      # categorical (factor with >2 levels)
  prate = "2l.lmer",      # continuous
  Observer = "",            # character — exclude or factor if needed
  SexEstimate = "logreg",   # categorical - 2 levels factor
  new_bug = "pmm",           # numeric  this also fine,  
  BroodSize='polyreg', 
  nr_helpers='polyreg',   
  WatchType='polyreg',  
  rownr=''
)


complete_prov<-filter(provimpu, !is.na(provimpu$prate))
sample_prov<-complete_prov[sample(1:nrow(complete_prov), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 
sample_prov2<-complete_prov[sample(1:nrow(complete_prov), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 

# provimpu$prate

str(provimpu)

provimpu$SexEstimate<-as.factor(provimpu$SexEstimate)
provimpu$nr_helpers<-as.factor(provimpu$nr_helpers)
provimpu$BroodSize<-as.factor(provimpu$BroodSize)


provpoints<-cv_others(samplepts = sample_prov$rownr, impudata = provimpu, trait = 'prate', cv_method = provi_method, cv_pred=provi_pred)
provpoints2<-cv_others(samplepts = sample_prov2$rownr, impudata = provimpu, trait = 'prate', cv_method = provi_method, cv_pred=provi_pred)


 provop<-sample_prov[,c('BirdID', 'prate', 'rownr')]
colnames(provop)[colnames(provop)=='prate']<-'og_prov'


provpoints<-filter(provpoints, provpoints$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

provpoints<-left_join(provpoints, provop, by=c("BirdID",'rownr'))

ggplot(provpoints, aes(x=og_prov, y=prate))+geom_point()+stat_smooth(method='lm')

cor(provpoints$og_prov, provpoints$prate)



provop2<-sample_prov2[,c('BirdID', 'prate', 'rownr')]
colnames(provop2)[colnames(provop2)=='prate']<-'og_prov'


provpoints2<-filter(provpoints2, provpoints2$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

provpoints2<-left_join(provpoints2, provop2, by=c("BirdID",'rownr'))

ggplot(provpoints2, aes(x=og_prov, y=prate))+geom_point()+stat_smooth(method='lm')

cor(provpoints2$og_prov, provpoints2$prate)

###############################################
#power analysis 


if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}

devtools::install_github("DejanDraschkow/mixedpower") 


library(mixedpower)
library(squidSim)
library(pwr)

telo<-read.csv('telomere_28_5.csv')
telo<-telo[,-c(1)]
telobyage<-telo%>%
  group_by(age_year)%>%
  summarise(count=n())   #add up age 12 and above 

#will need to add 

#telomeres 
#RTL association to fitness according to janets paper: 0.06
pwr.f2.test(u = 1, v= , f2=0.06, sig.level = 0.05, power = 0.8)

#provisioning rate 
prov<-read.csv('provisioning_28_5.csv', sep=';')
prov<-prov[,-c(1)]


provbyage<-prov%>%
  group_by(age)%>%
  summarise(count=n())  #add up age 12 and above. 

pwr.f2.test(u = 2, v= , f2=0.29, sig.level = 0.05, power = 0.8)



