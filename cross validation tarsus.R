bm_impu<-read.csv('bm_impu.csv')
bm_impu<-bm_impu[,-c(1)]

library(tidyverse)
library(mice)

bm_impu$SexEstimate<-as.factor(bm_impu$SexEstimate)

####
bm_impu$rownr<-row.names(bm_impu)
complete_tars<-filter(bm_impu, !is.na(bm_impu$RightTarsus))
sample_tars<-complete_tars[sample(1:nrow(complete_tars), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 




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
  # CatchTime_mins = "pmm",   # numeric  these are fine 
  new_bug = "pmm",           # numeric  this also fine
  rownr=''
)

cv_func<-function(x){
  cv_samples[c(x),'BodyMass']<-NA 
  impu <- mice(cv_samples, method = cv_method, predictorMatrix = cv_pred, m = 5) #needs imputation.r to run 
  completeimpu<-complete(impu, action='long', include=T, all=T)
  onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
  return(onepoint)
}


