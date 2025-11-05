
#imputation of traits 

library(RODBC)
library(psych)
library(Amelia)
library(mice)
library(tidyverse)
library(pan)

"%!in%" <-Negate("%in%")

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)

physio<-read.csv('physio_28_5.csv')
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

lastseen<-read.csv('lastseen.csv', sep=';', stringsAsFactors = F)
lastseen<-filter(lastseen, lastseen$Island=='CN')


 #the old model 
#body mass ~ age + lifespan + right tarsus + avg_bug + sex + newstat+ 
#catch time + +(1|BirdID) + (1|observer)+ (1|birthyear)

#maybe no need impute on birth year ? 


#view missingness 
# md.pattern(bodymass)



#body mass 

#get max lifespan in years 
bodymass$lifespan_years<-round(bodymass$newlifespan/365)

# create rows for max lifespan 


#?? 

#remove birds that don't have any body mass estimates, but removing NAs for body mass should do the job
bodymass2<-filter(bodymass, !is.na(bodymass$BodyMass))


# i<-1042
#created rows for ages with no body mass measured 
bm_filled<-data.frame()
for(i in unique(bodymass2$BirdID)){
  onebird<-filter(bodymass2, bodymass2$BirdID==i)
  lifespan_years=unique(onebird$lifespan_years)
  missingages<-setdiff(c(1:lifespan_years),onebird$age_year)
  birthyear<-unique(onebird$birthyear)
  newlifespan<-unique(onebird$newlifespan)
  SexEstimate<-unique(onebird$SexEstimate)
  onebird<-onebird%>%add_row(BirdID=i, age_year=missingages, birthyear=birthyear, newlifespan=newlifespan, 
                             lifespan_years=lifespan_years, occasionyear=missingages+birthyear, SexEstimate=SexEstimate)
  bm_filled<-rbind(bm_filled, onebird)
}


bm_filled<-bm_filled%>%
  arrange(BirdID,age_year)


#max age is 20, but remove any rows created beyond age 17. no body mass older than 17. 

bm_filled<-filter(bm_filled, bm_filled$age_year<=17)

#field period fill as the main breeding season of that year 

fps<-readxl::read_excel('fps.xlsx')
fps$PeriodEnd<-as.Date(fps$PeriodEnd, "%Y-%m-%d")
fps$PeriodStart<-as.Date(fps$PeriodStart, "%Y-%m-%d")
fps$seasonlength<-fps$PeriodEnd-fps$PeriodStart
fps<-fps%>%
  group_by(PeriodYear)%>%
  mutate(season=case_when(seasonlength == max(seasonlength)~"main",
                          seasonlength != max(seasonlength)~"winter"))
fps<-as.data.frame(fps)

#need to fill in now 
#ng g how 
fpsmain <- fps %>% arrange(season) %>% distinct(PeriodYear,.keep_all = T) %>% 
  dplyr::select(FieldPeriodID,PeriodYear) %>% rename(newFPID = FieldPeriodID)
bm_filled_2 <- merge(bm_filled,fpsmain,by.x="occasionyear",by.y="PeriodYear",all.x=T ) %>%
  mutate(FPID = case_when(!is.na(FieldPeriodID) ~FieldPeriodID, is.na(FieldPeriodID) ~ newFPID ))

#then join everything with sys_status per field period to get status 
colnames(lastseen)[colnames(lastseen)=='PeriodYear']<-'occasionyear'
lastseen<-lastseen%>%
  select(BirdID, Status, FieldPeriodID,occasionyear)
colnames(lastseen)[colnames(lastseen)=='Status']<-'filledstatus'
colnames(lastseen)[colnames(lastseen)=='FieldPeriodID']<-'FPID'



#fill missing statuses in 

bm_filled_stat <- merge(bm_filled_2,lastseen,by=c('BirdID', "FPID"),all.x=T ) %>%
  mutate(newstatus = case_when(!is.na(Status) ~Status, is.na(Status) ~ filledstatus ))



#join with avg bug avail 

avg_bug<-read.csv('mean_insect_2024.csv')  #mean by season 
avg_bug<-avg_bug[,-c(1)]
yearly_bug<-avg_bug%>%
  group_by(occasionyear)%>%
  summarize(avg_invert=mean(avg_invert, na.rm=T))

colnames(avg_bug)<-c('occasionyear', 'FPID', 'bug_amount')


colnames(bm_filled_stat)[colnames(bm_filled_stat)=='occasionyear.x']<-'occasionyear'
test <- merge(bm_filled_stat,avg_bug,by=c('occasionyear','FPID'),all.x=T ) %>%
  mutate(new_bug = case_when(!is.na(avg_invert) ~avg_invert, is.na(avg_invert) ~ bug_amount ))


#the original bug data is based on season. 
#the filled ones are yearly averages 
#perhaps i should use bug amount? 


#there are birds with birth year assigned in the 70s - dont know why
weirdborbs<-c(2729,2732,2728,2858,2855,2859,2751,2740,2741,2859,2747,427)
what<-test%>%
  filter(BirdID %in% weirdborbs)
#the ages cannot be right.. 

#remove from data
bm_clean<-test%>%
  filter(BirdID %!in% weirdborbs)



#columns to use for imputation are newFPID filledstatus and newbug

#then bin everything from 14 on 

# bm_clean$age_year[bm_clean$age_year>=14]<-14
bm_clean$newstat[bm_clean$filledstatus %in% c("BrF", "BrM","BrU")]<-"Dom"
bm_clean$newstat[bm_clean$filledstatus %in% c("H")]<-"Help"
bm_clean$newstat[bm_clean$filledstatus %!in% c("BrF", "BrM","BrU",'H')]<-"Sub"

#now can use newstat column for imputation

###
bm_clean <-bm_clean %>%
  arrange(BirdID,age_year)

bm_clean<-bm_clean%>%
  select(-c(lifespan,Status,FieldPeriodID,occasionyear.y))

bm_impu<-bm_clean%>%
  select(BirdID, birthyear,occasionyear, age_year, newlifespan, newFPID, newstat, BodyMass, RightTarsus, 
         Observer, SexEstimate, CatchTime_mins, new_bug)

#chat gpt code 

#view missingness 
md.pattern(bm_impu, rotate.names = T)

completecase_bm<-cc(bm_impu) #complete cases 

# missingsex<-filter(bm_impu, is.na(bm_impu$SexEstimate))

bm_impu<-filter(bm_impu, bm_impu$age_year>0)  #remove age 0 

# Setup predictor matrix and method

bm_pred<-make.predictorMatrix(bm_impu)

#values in predictor: 0 means not used in imputation and 1 means used.
exclude <- c('birthyear', 'Observer', 'occasionyear')

bm_pred[, exclude] <- 0


#can combine 
bm_pred[c('BodyMass','RightTarsus'), "BirdID"] <- -2  # Random intercept group indicator
bm_pred["BirdID",c('BodyMass','RightTarsus')] <- -2  # Random intercept group indicator
# bm_pred[c('BodyMass','RightTarsus'), "newFPID"] <- -2
# bm_pred["newFPID",c('BodyMass','RightTarsus')] <- -2  # Random intercept group indicator
# bm_pred[c('BodyMass','RightTarsus'), "occasionyear"] <- -2
# bm_pred["occasionyear",c('BodyMass','RightTarsus')] <- -2
# bm_pred[c('BodyMass','RightTarsus'), "Observer"] <- -2  #set observer to 0 
# bm_pred["Observer",c('BodyMass','RightTarsus')] <- -2 
# bm_pred[c('BodyMass','RightTarsus'), "birthyear"] <- -2
# bm_pred["birthyear",c('BodyMass','RightTarsus')] <- -2

#

#matrix so needs mirroring. 







#method for imputation 
bm_method<-make.method(bm_pred)

bm_method <- c(
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
  new_bug = "pmm"           # numeric  this also fine 
)

bm_impu$newstat<-as.factor(bm_impu$newstat)
bm_impu$BirdID<-as.integer(bm_impu$BirdID)
bm_impu$SexEstimate<-as.factor(bm_impu$SexEstimate)


#imputation 
imp <- mice(bm_impu, method = bm_method, predictorMatrix = bm_pred, m = 5)
# str(bm_impu)


#checking ID and observations 
what<-bm%>%group_by(BirdID)%>%summarize(bmnr=length(BodyMass))
tf<-eh%>%group_by(bmnr)%>%summarise(length=n())
#731 birds with only 1 observation, 347 birds with only 2 observations 



#this pulls the iterations of the imputation 
imputed_data <- complete(imp, c(1:5))

#check missingness pattern of data 
md.pattern(imputed_data, rotate.names = T)

na_bodymass<-filter(imputed_data, is.na(imputed_data$BodyMass))


completed_data<-complete(imp, action='long', include=T, all=T)
#this pulls all of the dataframes, 0 is original data 
completed_data$.imp<-as.factor(completed_data$.imp)


#visual inspection of imputation 
ggplot(completed_data, aes(x=BodyMass, colour=.imp)) + geom_density()


impstats<-completed_data%>%
  group_by(.imp)%>%
  summarise(mean=mean(BodyMass, na.rm=T), 
            SD=sd(BodyMass,na.rm=T))


bwplot(imp)
stripplot(imp,pch = c(21, 20), cex = c(1, 1.5))


fit <- with(imp, glm(ici(BodyMass) ~ age_year + newlifespan,
                     family = gaussian))
ps <- rep(rowMeans(sapply(fit$analyses, fitted.values)),
          imp$m + 1)
xyplot(imp, BodyMass ~ age_year | as.factor(.imp),
       xlab = "age",
       ylab = "bm", pch = c(1, 19), col = mdc(1:2), alpha=0.4)





#############







