imputed_bm<-read.csv('imputed_bm.csv')
imputed_bm<-imputed_bm[,-c(1)]

otherdf<-read.csv('bm_impu.csv')

physio<-read.csv('physio_28_5.csv')
physio<-physio[,-c(1)]

#--------original model-----------


samplesize<-physio%>%
  filter(!is.na(BodyMass))%>%
  group_by(BirdID)%>%
  summarize(obs=n())

manyobs<-samplesize%>%
  filter(obs>3)


#vector of bird id with multiple obs 
manyobs<-manyobs$BirdID


cv_samples<-filter(imputed_bm,  imputed_bm$BirdID %in% manyobs)


#50 points maybe would be a good 
#make function 
#for i in bird ID, pick bm not NA, remove that from dataset 
set.seed(1)
cv_samples$rownr<-row.names(cv_samples)
forfiltering<-filter(cv_samples, !is.na(cv_samples$BodyMass))
to_remove<-forfiltering[sample(1:nrow(forfiltering), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 

#give x is to_remove$rownr 
#apply over df 


#make predictor matrix for imputation 
cv_pred<-make.predictorMatrix(cv_samples)

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
  cv_samples[c(x),'BodyMass']<-NA 
  impu <- mice(cv_samples, method = cv_method, predictorMatrix = cv_pred, m = 10) #needs imputation.r to run 
  completeimpu<-complete(impu, action='long', include=T, all=T)
  onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
  return(onepoint)
}


#repeat for 50 points?? 
#plot the graph of imputed (mean and error bars) vs original 


idk<-list()

idk<-mapply(to_remove$rownr, FUN = cv_func)

trial_dfs<-split(idk, colnames(idk))

trial_dfs<-lapply(trial_dfs, function(x){as.data.frame(x)})

rownames(trial_dfs[[1]])<-c(rownames(idk))

what<-lapply(trial_dfs, FUN=as.data.frame)


idk2<-as.data.frame(idk)

hist(bm_clean$newlifespan, bin=16)





