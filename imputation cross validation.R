library(tidyverse)
library(mice)

######################################

#testing on body mass imputation 
imputed_bm<-read.csv('imputed_bm.csv')
imputed_bm<-imputed_bm[,-c(1)]

bm_impu<-read.csv('bm_impu.csv')  #dataset to be imputed 
bm_impu<-bm_impu[,-c(1)]

physio<-read.csv('physio_28_5.csv')
physio<-physio[,-c(1)]

#cross validation 
#remove observed data point, use proposed model to predict outcome for the excluded data point
#repeat this for a number of data points then run correlation 


#--------original model-----------

#get nr birds with high sample size 
samplesize<-physio%>%   
  filter(!is.na(BodyMass))%>%
  group_by(BirdID)%>%
  summarize(obs=n())

manyobs<-samplesize%>%
  filter(obs>3)


#vector of bird id with multiple obs 
# manyobs<-manyobs$BirdID


#testing on a smaller sample size 
cv_samples<-filter(bm_impu,  bm_impu$BirdID %in% manyobs)


#test on the full sample size 
cv_samples2<-bm_impu


#50 points maybe would be a good 
#make function 
#for i in bird ID, pick bm not NA, remove that from dataset 
set.seed(1)
cv_samples$rownr<-row.names(cv_samples)
forfiltering<-filter(cv_samples, !is.na(cv_samples$BodyMass))
to_remove<-forfiltering[sample(1:nrow(forfiltering), 50,replace=F),]#dataset of rows to remove, remove rows one by one? 


#give x is to_remove$rownr 
#apply over df 


#redo over the whole dataset 
cv_samples2$rownr<-as.numeric(row.names(cv_samples2))
cv_samples2_complete<-filter(cv_samples2, !is.na(cv_samples2$BodyMass))
sampledf2<-cv_samples2_complete[sample(1:nrow(cv_samples2_complete), 50,replace=F),]
sampledf3<-cv_samples2_complete[sample(1:nrow(cv_samples2_complete), 50,replace=F),]
sampledf4<-cv_samples2_complete[sample(1:nrow(cv_samples2_complete), 50,replace=F),]

cv_samples2$SexEstimate<-as.factor(cv_samples2$SexEstimate)


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
  impu <- mice(cv_samples, method = cv_method, predictorMatrix = cv_pred, m = 5) #needs imputation.r to run 
  completeimpu<-complete(impu, action='long', include=T, all=T)
  onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
  return(onepoint)
}



cv_func2<-function(x){
  cv_samples2[c(x),'BodyMass']<-NA 
  impu <- mice(cv_samples2, method = cv_method, predictorMatrix = cv_pred, m = 5) #needs imputation.r to run 
  completeimpu<-complete(impu, action='long', include=T, all=T)
  onepoint<-filter(completeimpu, completeimpu$rownr==x)  #extract data from just row number 
  return(onepoint)
}



#repeat for 50 points?? 
#plot the graph of imputed (mean and error bars) vs original 


idk<-list()

idk<-lapply(to_remove$rownr, FUN = cv_func)  #should i use lapply instead?? 
# assign names
#melt

#bigger df 
cv2<-lapply(sampledf2$rownr, FUN=cv_func2)
cv3<-lapply(sampledf3$rownr, FUN = cv_func2)

cv_points<-do.call(rbind.data.frame, idk)

#add original observation to NA 
# to_remove$.id<-to_remove$rownr
# to_remove$.imp<-0

originalpoints<-to_remove[,c('BirdID', 'BodyMass', 'rownr')]

cv_points<-filter(cv_points, cv_points$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

cv_points<-left_join(cv_points, originalpoints, by=c("BirdID",'rownr'))

#then correlate 
ggplot(cv_points, aes(x=BodyMass.y, y=BodyMass.x))+geom_point()+stat_smooth(method='lm')

cor(cv_points$BodyMass.x, cv_points$BodyMass.y)
aaa<-lm(BodyMass.x~BodyMass.y, cv_points)
summary(aaa)
#######################################
#bigger df 

cv_points2<-do.call(rbind.data.frame, cv2)
cv_pts3<-do.call(rbind.data.frame, cv3)

#add original observation to NA 
# to_remove$.id<-to_remove$rownr
# to_remove$.imp<-0

originalpoints2<-sampledf2[,c('BirdID', 'BodyMass', 'rownr')]
colnames(originalpoints2)[colnames(originalpoints2)=='BodyMass']<-'Original_BM'

#rep
originalpoints3<-sampledf3[,c('BirdID', 'BodyMass', 'rownr')]
colnames(originalpoints3)[colnames(originalpoints3)=='BodyMass']<-'Original_BM'

cv_points2<-filter(cv_points2, cv_points2$.imp !=0)
# cv_points<-rbind(cv_points, to_remove)%>%arrange(BirdID, .imp)

cv_pts3<-filter(cv_pts3, cv_pts3$.imp !=0)


cv_points2<-left_join(cv_points2, originalpoints2, by=c("BirdID",'rownr'))

cv_pts3<-left_join(cv_pts3, originalpoints3, by=c('BirdID','rownr'))

#then correlate 
ggplot(cv_points2, aes(x=Original_BM, y=BodyMass))+geom_point()+stat_smooth(method='lm')

cor(cv_points2$BodyMass, cv_points2$Original_BM)
getr2<-lm(BodyMass~Original_BM, cv_points2)


ggplot(cv_pts3, aes(x=Original_BM, y=BodyMass))+geom_point()+stat_smooth(method='lm')

cor(cv_pts3$BodyMass, cv_pts3$Original_BM)
getr3<-lm(BodyMass~Original_BM, cv_pts3)


############ i can probably streamline this.. ###############################









################################################################

trial_dfs<-split(idk, colnames(idk))

trial_dfs<-lapply(trial_dfs, function(x){as.data.frame(x)})

rownames(trial_dfs[[1]])<-c(rownames(idk))

what<-lapply(trial_dfs, FUN=as.data.frame)


idk2<-as.data.frame(idk)



#try it on the whole dataset 





