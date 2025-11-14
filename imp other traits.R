# fat score imputation 

library(gsl)

colnames(bodymass) # these are columns needed for each trait

# bodymass, fat score, tarsus, TL, provisioning rate, female buffycoat

telomere_28_5 <- read_csv("telomere_28_5.csv")
haem <- read_csv("haematocrit_28_5.csv")
provi <- read.csv("provisioning_28_5.csv", sep=";")

# select columns for each trait - body mass is in a different script 
# bodymass
telo1 <- telomere_28_5[,c('BirdID','RTL','Whodunnit', 'birthyear',
                          'occasionyear','age_year',"Status",
                          'newlifespan','TQ','SexEstimate','newstat',
                          'avg_invert','FieldPeriodID')] 

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

tarsus <- bodymass 

# remove na measurements of each trait 
# bodymass2
telo1 <- telo1 %>% filter(!is.na(RTL)) %>% mutate(lifespan_years = round(newlifespan/365))
buffy <- buffy %>% filter(!is.na(BuffyCoat) & SexEstimate == 0) %>% 
  mutate(lifespan_years = round(newlifespan/365), age_year = age)%>%
  select(-age)

provi1 <- provi1 %>% filter(!is.na(prate)) %>% 
  mutate(lifespan_years = round(newlifespan/365), age_year = age,
         avg_invert = as.numeric(gsub(",",".", avg_invert)), 
         prate = as.numeric(gsub(",",".", prate)))%>%
  select(-age)
  
fatscore <- fatscore %>% filter(!is.na(FatScore)) %>% 
  mutate(lifespan_years = round(newlifespan/365))

tarsus <- tarsus %>% filter(!is.na(RightTarsus)) %>% 
  mutate(lifespan_years = round(newlifespan/365))



# function to create empty rows per bird

# traitdf<-telo2

fillrows <- function(traitdf, age){
  traitfilled<-data.frame()
  for(i in unique(traitdf$BirdID)){
    onebird<-filter(traitdf, traitdf$BirdID==i)
    lifespan_years=unique(onebird$lifespan_years)
    missingages<-setdiff(c(age:lifespan_years),onebird$age_year)
    birthyear<-unique(onebird$birthyear)
    newlifespan<-unique(onebird$newlifespan)
    SexEstimate<-unique(onebird$SexEstimate)
    onebird<-onebird%>%add_row(BirdID=i, age_year=missingages, birthyear=birthyear, newlifespan=newlifespan, 
                               lifespan_years=lifespan_years, occasionyear=missingages+birthyear, SexEstimate=SexEstimate)
    traitfilled<-rbind(traitfilled, onebird)
  }
  return(traitfilled)
}



telofilled <- fillrows(telo1, 0)
buffyfilled <- fillrows(buffy, 0)
provifilled <- fillrows(provi1, 0)
fatscorefilled <- fillrows(fatscore, 1)
tarsusfilled <- fillrows(tarsus, 1)



telofilled<-telofilled%>%
  arrange(BirdID,age_year)
buffyfilled<-buffyfilled%>%
  arrange(BirdID,age_year)
provifilled<-provifilled%>%
  arrange(BirdID,age_year)
fatscorefilled<-fatscorefilled%>%
  arrange(BirdID,age_year)
tarsusfilled<-tarsusfilled%>%
  arrange(BirdID,age_year)


telofilled<-filter(telofilled, telofilled$age_year<=17)
buffyfilled<-filter(buffyfilled, buffyfilled$age_year<=16)
provifilled<-filter(provifilled, provifilled$age_year<=18)
fatscorefilled<-filter(fatscorefilled, fatscorefilled$age_year<=17 & fatscorefilled$age_year>0)
tarsusfilled<-filter(tarsusfilled, tarsusfilled$age_year<=17 & tarsusfilled$age_year>0)




# fill the covariate gaps

#read the covariate dfs 


#get main field period 
fps<-readxl::read_excel('fps.xlsx')
fps$PeriodEnd<-as.Date(fps$PeriodEnd, "%Y-%m-%d")
fps$PeriodStart<-as.Date(fps$PeriodStart, "%Y-%m-%d")
fps$seasonlength<-fps$PeriodEnd-fps$PeriodStart
fps<-fps%>%
  group_by(PeriodYear)%>%
  mutate(season=case_when(seasonlength == max(seasonlength)~"main",
                          seasonlength != max(seasonlength)~"winter"))
fps<-as.data.frame(fps)

fpsmain <- fps %>% arrange(season) %>% distinct(PeriodYear,.keep_all = T) %>% 
  dplyr::select(FieldPeriodID,PeriodYear) %>% rename(newFPID = FieldPeriodID)

#statuses
lastseen<-read.csv('lastseen.csv', sep=';', stringsAsFactors = F)
lastseen<-filter(lastseen, lastseen$Island=='CN')
colnames(lastseen)[colnames(lastseen)=='PeriodYear']<-'occasionyear'
lastseen<-lastseen%>%
  select(BirdID, Status, FieldPeriodID,occasionyear)
colnames(lastseen)[colnames(lastseen)=='Status']<-'filledstatus'
colnames(lastseen)[colnames(lastseen)=='FieldPeriodID']<-'FPID'


#average invert per year
avg_bug<-read.csv('mean_insect_2024.csv')  #mean by season 
avg_bug<-avg_bug[,-c(1)]
yearly_bug<-avg_bug%>%
  group_by(occasionyear)%>%
  summarize(avg_invert=mean(avg_invert, na.rm=T))

colnames(avg_bug)<-c('occasionyear', 'FPID', 'bug_amount')

#birds with wrong ages
weirdborbs<-c(2729,2732,2728,2858,2855,2859,2751,2740,2741,2859,2747,427)
what<-test%>%
  filter(BirdID %in% weirdborbs)




############################
telo_clean <- merge(telofilled, fpsmain, by.x="occasionyear",by.y="PeriodYear",all.x=T ) %>%
  mutate(FPID = case_when(!is.na(FieldPeriodID) ~ FieldPeriodID, 
                          is.na(FieldPeriodID) ~ newFPID)) %>% 
  merge(.,lastseen,by=c('BirdID', "FPID"),all.x=T ) %>%
  mutate(newstatus = case_when(!is.na(Status) ~Status, 
                               is.na(Status) ~ filledstatus)) %>%
  dplyr::rename(occasionyear=occasionyear.x) %>%  
  merge(.,avg_bug,by=c('occasionyear','FPID'), all.x=T ) %>%
  mutate(new_bug = case_when(!is.na(avg_invert) ~avg_invert, 
                             is.na(avg_invert) ~ bug_amount ))%>%
  filter(BirdID %!in% weirdborbs)%>%
  mutate(newstat = case_when(filledstatus %in% c("BrF", "BrM","BrU") ~ "Dom", 
                             filledstatus %in% "H" ~ "Help", 
                             TRUE ~ "Sub"))%>%
  arrange(BirdID,age_year)

teloimpu <- telo_clean %>%
  select(BirdID, birthyear,occasionyear, age_year, newlifespan,
         FPID, newstat, RTL, Whodunnit, SexEstimate, new_bug) %>%
  mutate(newstat = as.factor(newstat), SexEstimate = as.factor(SexEstimate), BirdID=as.integer(BirdID))



# fill buffy coat
buffyfilled2 <- merge(buffyfilled,fpsmain,by="occasionyear",by.y="PeriodYear",all.x=T ) %>%
  mutate(FPID = case_when(!is.na(FieldPeriodID) ~FieldPeriodID, is.na(FieldPeriodID) ~ newFPID ))%>% 
  merge(.,lastseen,by=c('BirdID', "FPID"),all.x=T ) %>%
  mutate(newstatus = case_when(!is.na(Status) ~Status, is.na(Status) ~ filledstatus ))%>%
  dplyr::rename(occasionyear=occasionyear.x) %>%  
  merge(.,avg_bug,by=c('occasionyear','FPID'),all.x=T ) %>%
  mutate(new_bug = case_when(!is.na(avg_invert) ~avg_invert, 
                             is.na(avg_invert) ~ bug_amount ))%>%
  filter(BirdID %!in% weirdborbs)%>%
  mutate(newstat = case_when(filledstatus %in% c("BrF", "BrM","BrU") ~ "Dom", 
                             filledstatus %in% "H" ~ "Help", 
                             TRUE ~ "Sub"))%>%
  arrange(BirdID,age_year)

buffyimpu <- buffyfilled2 %>%
  select(BirdID, birthyear,occasionyear, age_year, newlifespan, 
         FPID, newstat, BuffyCoat, Observer, SexEstimate, new_bug, CatchTime) %>%
  mutate(newstat = as.factor(newstat), SexEstimate = as.factor(SexEstimate), BirdID=as.integer(BirdID))

library(chron)
buffyimpu$CatchTime<-chron(times=buffyimpu$CatchTime)
buffyimpu$CatchTime_mins<-60 * 24 * as.numeric(times(buffyimpu$CatchTime))



#fill provi
prov_clean <- merge(provifilled,fpsmain,by.x="occasionyear",by.y="PeriodYear",all.x=T ) %>%
  mutate(FPID = case_when(!is.na(FieldPeriodID) ~FieldPeriodID, is.na(FieldPeriodID) ~ newFPID )) %>% 
  merge(.,lastseen,by=c('BirdID', "FPID"),all.x=T ) %>%
  mutate(newstatus = case_when(!is.na(Status) ~Status, 
                               is.na(Status) ~ filledstatus )) %>%
  dplyr::rename(occasionyear=occasionyear.x) %>%  
  merge(.,avg_bug,by=c('occasionyear','FPID'),all.x=T ) %>%
  mutate(new_bug = case_when(!is.na(avg_invert) ~ avg_invert, 
                             is.na(avg_invert) ~ bug_amount ))%>%
  filter(BirdID %!in% weirdborbs)%>%
  mutate(newstat = case_when(filledstatus %in% c("BrF", "BrM","BrU") ~ "Dom", 
                             filledstatus %in% "H" ~ "Help", 
                             TRUE ~ "Sub"))%>%
  arrange(BirdID,age_year)

provimpu <- prov_clean %>%
  select(BirdID, birthyear,occasionyear, age_year, newlifespan, 
         FPID, newstat, prate, Observer, SexEstimate, new_bug, 
         BroodSize, nr_helpers, WatchType) %>%
  mutate(newstat = as.factor(newstat), SexEstimate = as.factor(SexEstimate), BirdID=as.integer(BirdID))


# fill fat score
fatscore_clean <- merge(fatscorefilled,fpsmain,by.x="occasionyear",by.y="PeriodYear",all.x=T ) %>%
  mutate(FPID = case_when(!is.na(FieldPeriodID) ~FieldPeriodID, 
                          is.na(FieldPeriodID) ~ newFPID )) %>% 
  merge(.,lastseen,by=c('BirdID', "FPID"),all.x=T ) %>%
  mutate(newstatus = case_when(!is.na(Status) ~Status, 
                               is.na(Status) ~ filledstatus )) %>%
  dplyr::rename(occasionyear=occasionyear.x) %>%  
  merge(.,avg_bug,by=c('occasionyear','FPID'),all.x=T ) %>%
  mutate(new_bug = case_when(!is.na(avg_invert) ~avg_invert, 
                             is.na(avg_invert) ~ bug_amount ))%>%
  filter(BirdID %!in% weirdborbs)%>%
  mutate(newstat = case_when(filledstatus %in% c("BrF", "BrM","BrU") ~ "Dom", 
                             filledstatus %in% "H" ~ "Help", 
                             TRUE ~ "Sub"))%>%
  arrange(BirdID,age_year)

fatscoreimpu <- fatscore_clean %>%
  select(BirdID, birthyear,occasionyear, age_year, newlifespan, FPID, 
         newstat, FatScore, Observer, SexEstimate, new_bug, CatchTime) %>%
  mutate(newstat = as.factor(newstat), SexEstimate = as.factor(SexEstimate), BirdID=as.integer(BirdID))


#fill tarsus 
tarsus_clean <- merge(tarsusfilled,fpsmain,by.x="occasionyear",by.y="PeriodYear",all.x=T ) %>%
  mutate(FPID = case_when(!is.na(FieldPeriodID) ~FieldPeriodID, 
                          is.na(FieldPeriodID) ~ newFPID )) %>% 
  merge(.,lastseen,by=c('BirdID', "FPID"),all.x=T ) %>%
  mutate(newstatus = case_when(!is.na(Status) ~Status, 
                               is.na(Status) ~ filledstatus )) %>%
  dplyr::rename(occasionyear=occasionyear.x) %>%  
  merge(.,avg_bug,by=c('occasionyear','FPID'),all.x=T ) %>%
  mutate(new_bug = case_when(!is.na(avg_invert) ~avg_invert, 
                             is.na(avg_invert) ~ bug_amount ))%>%
  filter(BirdID %!in% weirdborbs)%>%
  mutate(newstat = case_when(filledstatus %in% c("BrF", "BrM","BrU") ~ "Dom", 
                             filledstatus %in% "H" ~ "Help", 
                             TRUE ~ "Sub"))%>%
  arrange(BirdID,age_year)

tarsusimpu <- tarsus_clean %>%
  select(BirdID, birthyear,occasionyear, age_year, newlifespan, FPID, newstat,
         RightTarsus, Observer, SexEstimate, new_bug, BodyMass) %>%
  mutate(newstat = as.factor(newstat), SexEstimate = as.factor(SexEstimate), BirdID=as.integer(BirdID))

##################################


# imputation steps for all traits ----
## Telo ----
# Setup predictor matrix and method

telo_pred<-make.predictorMatrix(teloimpu)

telo_pred[c('RTL'), "BirdID"] <- -2  # Random intercept group indicator
telo_pred["BirdID",c('RTL')] <- -2 

#values in predictor: 0 means not used in imputation and 1 means used.
telo_exclude <- c('birthyear', 'Whodunnit', 'occasionyear')

telo_pred[, telo_exclude] <- 0

diag(telo_pred) <- 0

#method for imputation 
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
  Whodunnit = "",            # character — exclude or factor if needed
  SexEstimate = "logreg",   # categorical - 2 levels factor
  new_bug = "pmm"           # numeric  this also fine 
)

# colnames(teloimpu)
teloimpu$newstat<-as.factor(teloimpu$newstat)
teloimpu$BirdID<-as.integer(teloimpu$BirdID)
teloimpu$SexEstimate<-as.factor(teloimpu$SexEstimate)

#imputation 
telo_imp_done <- mice(teloimpu, method = telo_method, predictorMatrix = telo_pred, m = 10)
# str(bm_impu)


#this pulls the iterations of the imputation 
telo_imputed_data <- complete(telo_imp_done, c(1:10))

#check missingness pattern of data 
md.pattern(telo_imputed_data, rotate.names = T)

na_telo<-filter(telo_imputed_data, is.na(telo_imputed_data$RTL))


telo_completed_data<-complete(telo_imp_done, action='long', include=T, all=T)
#this pulls all of the dataframes, 0 is original data 
telo_completed_data$.imp<-as.factor(telo_completed_data$.imp)

teloimpu_densityplot <- ggplot(telo_completed_data, aes(x=RTL, colour=.imp)) + 
  geom_density() 




## haem/ buffy coat ----
# Setup predictor matrix and method
buffyimpu<-buffyimpu%>%
  select(-CatchTime)

buffypred<-make.predictorMatrix(buffyimpu)

buffypred[c('BuffyCoat'), "BirdID"] <- -2  # Random intercept group indicator
buffypred["BirdID",c('BuffyCoat')] <- -2 

#values in predictor: 0 means not used in imputation and 1 means used.
buffy_exclude <- c('birthyear', 'Observer', 'occasionyear')

buffypred[, buffy_exclude] <- 0

#method for imputation 
buffymethod<-make.method(buffypred)

buffymethod <- c(
  BirdID = "",              # ID — exclude
  birthyear = "",           # exclude
  occasionyear = "",        # exclude
  age_year = "",            # derived
  newlifespan = "",         # fixed per bird
  FPID = "",          # continuous
  newstat = "polyreg",      # categorical (factor with >2 levels)
  BuffyCoat = "2l.lmer",      # continuous
  Observer = "",            # character — exclude or factor if needed
  new_bug = "pmm",           # numeric  this also fine , 
  SexEstimate='',
  CatchTime='pmm'
)

colnames(buffyimpu)

#imputation 
buffydone <- mice(buffyimpu, method = buffymethod, predictorMatrix = buffypred, m = 10)
# str(bm_impu)


#this pulls the iterations of the imputation 
buffy_complete <- complete(buffydone, c(1:10))

#check missingness pattern of data 
# md.pattern(haem_imputed_data, rotate.names = T)
# 
na_haem<-filter(haem_imputed_data, is.na(haem_imputed_data$BuffyCoat))


haem_completed_data<-complete(haem_imp_done, action='long', include=T, all=T)
#this pulls all of the dataframes, 0 is original data 
haem_completed_data$.imp<-as.factor(haem_completed_data$.imp)

haemimpu_densityplot <-ggplot(haem_completed_data, aes(x=BuffyCoat, colour=.imp)) + geom_density()



## Provi ---- 
# Setup predictor matrix and method

provimpu2<-provimpu%>%
  select(-c(nr_helpers, WatchType,BroodSize))

provi_pred<-make.predictorMatrix(provimpu)
provi_pred[c('prate'), "BirdID"] <- -2  # Random intercept group indicator
provi_pred["BirdID",c('prate')] <- -2 

#values in predictor: 0 means not used in imputation and 1 means used.
provi_exclude <- c('birthyear', 'Observer', 'occasionyear', 'nr_helpers', 'WatchType','BroodSize')

#originally tried to include nr helpers and brood size as predictors, but seems to not impute
#perhaps too many NAs and not enough variation. 


provi_pred[, provi_exclude] <- 0

#method for imputation 
provi_method<-make.method(provi_pred)

provi_method <- c(
  BirdID = "",              # ID — exclude
  birthyear = "",           # exclude
  occasionyear = "",        # exclude
  age_year = "pmm",            # derived
  newlifespan = "pmm",         # fixed per bird
  FPID = "",          # continuous
  newstat = "polyreg",      # categorical (factor with >2 levels)
  prate = "pmm",      # continuous
  Observer = "",            # character — exclude or factor if needed
  SexEstimate = "logreg",   # categorical - 2 levels factor
  new_bug = "pmm",           # numeric  this also fine,  
  nr_helpers='',   
  WatchType='',  
  BroodSize=''
)

# colnames(provimpu)

#imputation 
provi_imp_done <- mice(provimpu, method = provi_method, predictorMatrix = provi_pred, m = 5)
# str(bm_impu)


#this pulls the iterations of the imputation 
provi_imputed_data <- complete(provi_imp_done, c(1:5))

#check missingness pattern of data 
md.pattern(provi_imputed_data, rotate.names = T)

na_provi<-filter(provi_imputed_data, is.na(provi_imputed_data$prate))


provi_completed_data<-complete(provi_imp_done, action='long', include=T, all=T)
#this pulls all of the dataframes, 0 is original data 
provi_completed_data$.imp<-as.factor(provi_completed_data$.imp)

proviimpu_densityplot <-ggplot(provi_completed_data, aes(x=prate, colour=.imp)) + geom_density()



## fat score ----
# Setup predictor matrix and method
fatscoreimpu$CatchTime<-chron(times=fatscoreimpu$CatchTime)
fatscoreimpu$CatchTime_mins<-60 * 24 * as.numeric(times(fatscoreimpu$CatchTime))


fatscoreimpu<-fatscoreimpu %>%
  select(-CatchTime)
  

library(micemd)
fatscore_pred<-make.predictorMatrix(fatscoreimpu)

fatscore_pred[c('FatScore'), "BirdID"] <- -2  # Random intercept group indicator
fatscore_pred["BirdID",c('FatScore')] <- -2 
#values in predictor: 0 means not used in imputation and 1 means used.
fatscore_exclude <- c('birthyear', 'Observer', 'occasionyear')

fatscore_pred[, fatscore_exclude] <- 0

#method for imputation 
fatscore_method<-make.method(fatscore_pred)

# chuen removed catch time, should it be added? 
fatscore_method <- c(
  BirdID = "",              # ID — exclude
  birthyear = "",           # exclude
  occasionyear = "",        # exclude
  age_year = "",            # derived
  newlifespan = "",         # fixed per bird
  FPID = "",          # continuous
  newstat = "polyreg",      # categorical (factor with >2 levels)
  FatScore = "2l.lmer",      # continuous
  Observer = "",            # character — exclude or factor if needed
  SexEstimate = "logreg",   # categorical - 2 levels factor
  new_bug = "pmm",           # numeric  this also fine 
  CatchTime_mins='pmm'
)

# colnames(fatscoreimpu)

#imputation 
fatscore_imp_done <- mice(fatscoreimpu, method = fatscore_method, predictorMatrix = fatscore_pred, m = 5)
# str(bm_impu)


test<-mice.impute.2l.glm.pois(fatscoreimpu, x=fatscore_pred, m=5)

#this pulls the iterations of the imputation 
fatscore_imputed_data <- complete(fatscore_imp_done, c(1:5))

#check missingness pattern of data 
md.pattern(fatscore_imputed_data, rotate.names = T)

na_fatscore<-filter(fatscore_imputed_data, is.na(fatscore_imputed_data$FatScore))


fatscore_completed_data<-complete(fatscore_imp_done, action='long', include=T, all=T)
#this pulls all of the dataframes, 0 is original data 
fatscore_completed_data$.imp<-as.factor(fatscore_completed_data$.imp)

fatscoreimpu_densityplot <-ggplot(fatscore_completed_data, aes(x=FatScore, colour=.imp)) + geom_density()



## Tarsus ----
#done while doing body mass 

# Setup predictor matrix and method
# 
# tarsus_pred<-make.predictorMatrix(tarsusimpu)
# 
# #values in predictor: 0 means not used in imputation and 1 means used.
# tarsus_exclude <- c('birthyear', 'Observer', 'occasionyear')
# 
# tarsus_pred[, tarsus_exclude] <- 0
# 
# #method for imputation 
# tarsus_method<-make.method(tarsus_pred)
# 
# # chuen removed catch time, should not matter for tarsus
# tarsus_method <- c(
#   BirdID = "",              # ID — exclude
#   birthyear = "",           # exclude
#   occasionyear = "",        # exclude
#   age_year = "",            # derived
#   newlifespan = "",         # fixed per bird
#   FPID = "",          # continuous
#   newstat = "polyreg",      # categorical (factor with >2 levels)
#   RightTarsus = "2l.lmer",      # continuous
#   Observer = "",            # character — exclude or factor if needed
#   SexEstimate = "logreg",   # categorical - 2 levels factor
#   new_bug = "pmm"           # numeric  this also fine 
# )
# 
# colnames(tarsusimpu)
# 
# #imputation 
# tarsus_imp_done <- mice(tarsusimpu, method = tarsus_method, predictorMatrix = tarsus_pred, m = 5)
# # str(bm_impu)
# 
# 
# #this pulls the iterations of the imputation 
# tarsus_imputed_data <- complete(tarsus_imp_done, c(1:5))
# 
# #check missingness pattern of data 
# md.pattern(tarsus_imputed_data, rotate.names = T)
# 
# na_tarsus<-filter(tarsus_imputed_data, is.na(tarsus_imputed_data$RightTarsus))
# 
# 
# tarsus_completed_data<-complete(tarsus_imp_done, action='long', include=T, all=T)
# #this pulls all of the dataframes, 0 is original data 
# tarsus_completed_data$.imp<-as.factor(tarsus_completed_data$.imp)
# 
# tarsusimpu_densityplot <-ggplot(tarsus_completed_data, aes(x=RightTarsus, colour=.imp)) + geom_density()
# 
# library(ggpubr)
# ggarrange(teloimpu_densityplot,
#           haemimpu_densityplot,
#           proviimpu_densityplot,
#           fatscoreimpu_densityplot,
#           tarsusimpu_densityplot, 
#           common.legend=T,labels="AUTO", legend = "bottom")
