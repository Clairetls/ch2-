pedigree<-read.csv('updated pedigree.csv', sep=';')
lifespan<-read.csv('lifespan_28_5_24.csv')
lastseen<-read.csv('lastseen.csv',sep=';')
lifespan<-lifespan[,-c(1)]
"%!in%" <-Negate("%in%")

library(RODBC)
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)



#wait whats this for??? 
# eh<-read.csv('updated pedigree.csv', sep=';')
# eh<-filter(eh, is.na(eh$GenMumConfidence))


############################################
library(tidyverse)
library(data.table)
pedigree<-left_join(pedigree,lifespan, by="BirdID")
pedigree<-filter(pedigree, pedigree$GenDadConfidence>=80 & pedigree$GenMumConfidence>=80)

sex<-readxl::read_excel('sys_SexEstimates.xlsx')
discard<-c(199,530,984,1230,1397,1508,1580,1619,1620,1675,1705,1806,3456,
           3531,3550,3616,3622,3629,3654,3662,3673,3674,3679,3681,3687,5256)
sex<-filter(sex, sex$BirdID %!in% discard)
sex<-sex[,c(1,2)]


#subset pedigree to exclude individuals older than 90 days 
# test<-filter(pedigree, pedigree$newlifespan>90|is.na(pedigree$newlifespan))
#filter for 90 

test2<-filter(pedigree, pedigree$newlifespan>364|is.na(pedigree$newlifespan))
#filter for a year survival as per otto and day

#NAs<-filter(test, is.na(test$newlifespan))  check NAs 
# filteredped<-test
filteredped2<-test2  #a year

# filteredped$chickyear<-as.numeric(str_sub(filteredped$BirthDate, 7))

filteredped2$chickyear<-as.numeric(str_sub(filteredped2$BirthDate, 7)) #a year


#i should not have to run ARS code for males/ genetic fathers but will keep for now. 


#use this 
#for a year
dams2<-list()
for(i in unique(filteredped2$BirdID)){
  dam2<-data.frame()
  dam2<-filter(filteredped2, filteredped2$GeneticMother==i)
  dams2[[i]]<-dam2
}
dams2<-do.call(rbind.data.frame, dams2)
dams2<-left_join(dams2,sex,by='BirdID')

#dont know why this one removes some bird IDs 
#i think its because i is gen mom not in BirdID 


dams2new<-filteredped2%>%
  group_by(GeneticMother, chickyear)%>%
  summarise(ars=length(BirdID))

colnames(dams2new)[colnames(dams2new)=='GeneticMother']<-'BirdID'
dams2new<-left_join(dams2new, sex, by='BirdID')


#fathers 
#no need 
# sires<-list()
# for(i in filteredped$BirdID){
#   sire<-data.frame()
#   sire<-filter(filteredped, filteredped$GeneticFather==i)
#   sires[[i]]<-sire
# }
# 
# 
# sires<-do.call(rbind.data.frame, sires)
# sires$chickyear<-str_sub(sires$BirthDate, 7,10)
# sires<-left_join(sires,sex,by='BirdID')

#one year ped
sires2<-list()
for(i in filteredped2$BirdID){
  sire2<-data.frame()
  sire2<-filter(filteredped2, filteredped2$GeneticFather==i)
  sires2[[i]]<-sire2
}

sires2<-do.call(rbind.data.frame, sires2)
sires2$chickyear<-str_sub(sires2$BirthDate, 7,10)
sires2<-left_join(sires2, sex, by='BirdID')

#######################################################
sires2new<-filteredped2%>%
  group_by(GeneticFather, chickyear)%>%
  summarise(ars=length(BirdID))

colnames(sires2new)[colnames(sires2new)=='GeneticFather']<-'BirdID'
sires2new<-left_join(sires2new, sex, by='BirdID')





###########



#one year dams################
dars2<-data.frame()
dars2<-dams2%>%
  group_by(GeneticMother, chickyear)%>%
  mutate(ars=length(chickyear))  #originally this row is summarize to make new df



dars2<-as.data.frame(dars2)
names(dars2)[names(dars2) == 'BirdID'] <- 'ChickBirdID'
names(dars2)[names(dars2) == 'GeneticMother'] <- 'BirdID'
dars2<-dars2[,c("NestID","BirdID","chickyear","ars")]
dars2<-unique(dars2)

#####################


# 
# #one year sire --------------
# sars2<-data.frame()
# sars2<-sires2%>%
#   group_by(GeneticFather, chickyear)%>%
#   mutate(ars=length(chickyear))  #originally this row is summarize to make new df
# 
# sars2<-as.data.frame(sars2)
# names(sars2)[names(sars2) == 'BirdID'] <- 'ChickBirdID'
# names(sars2)[names(sars2) == 'GeneticFather'] <- 'BirdID'
# sars2<-sars2[,c("BirdID","chickyear","ars","NestID")]
# sars2<-unique(sars2)

#fill in the gaps where they werent reproducing

#mom
# d<-subset(lastseen, lastseen$BirdID %in% dars$BirdID) #gets genetic mom
# d<-d[,c("BirdID","PeriodYear")]
# d$chickyear<-d$PeriodYear
# d<-unique(d)
# 
# idd<-data.frame()
# #i<-444
# for(i in unique(d$BirdID)){
#   bird<-filter(d, d$BirdID==i)
#   birds<-filter(dars, dars$BirdID==i)
#   no<-subset(bird, bird$chickyear %!in% birds$chickyear)
#   no<-unique(no)
#   idd<-rbind(no,idd)
# }
# idd$ars<-0
# 
# # names(idd)[names(idd) == 'PeriodYear'] <- 'chickyear'
# # names(idd)[names(idd) == 'FieldPeriodID'] <- 'BirthFieldPeriodID'
# 
# idd<-idd%>%
#   mutate(NestID=NA)
# idd<-idd[,-c(2)]


#one year --------------------------------
d2<-subset(lastseen, lastseen$BirdID %in% dams2new$BirdID) #gets genetic mom
d2<-d2[,c("BirdID","PeriodYear")]
d2$chickyear<-d2$PeriodYear
d2<-unique(d2)

unique(dams2new$BirdID)
idd2<-data.frame()
#i<-444
for(i in unique(d2$BirdID)){
  bird2<-filter(d2, d2$BirdID==i)
  birds2<-dams2new%>% filter(BirdID==i)
  no2<-subset(bird2, bird2$chickyear %!in% birds2$chickyear)
  no2<-unique(no2)
  idd2<-rbind(no2,idd2)
}
idd2$ars<-0

# names(idd)[names(idd) == 'PeriodYear'] <- 'chickyear'
# names(idd)[names(idd) == 'FieldPeriodID'] <- 'BirthFieldPeriodID'

idd2<-idd2%>%
  mutate(SexEstimate=0)
idd2<-idd2[,-c(2)]

# 4 borbs have chicks the only years they were alive so idd is 4 less than d2 and dams2new

#dad turn---------------------------------
# s<-subset(lastseen, lastseen$BirdID %in% sars$BirdID) #gets genetic dad
# s<-s[,c("BirdID","PeriodYear")]
# s$chickyear<-s$PeriodYear
# s<-unique(s)
# 
# sars<-as.data.frame(sars)
# 
# ids<-data.frame()
# for(i in unique(s$BirdID)){
#   bird<-filter(s, s$BirdID %in% i)
#   birds<-filter(sars, sars$BirdID %in% i)
#   no<-subset(bird, bird$chickyear %!in% birds$chickyear)
#   no<-unique(no)
#   ids<-rbind(no,ids)
# }
# ids$ars<-0
# # names(ids)[names(ids) == 'PeriodYear'] <- 'chickyear'
# # ids<-ids[,-c(2)]
# 
# ids<-ids%>%
#   mutate(NestID=NA,)
# ids<-ids[,-c(2)]
# 
# ######### one year -------------------------
s2<-subset(lastseen, lastseen$BirdID %in% sires2new$BirdID) #gets genetic dad
s2<-s2[,c("BirdID","PeriodYear")]
s2$chickyear<-s2$PeriodYear
s2<-unique(s2)

sires2new<-as.data.frame(sires2new)

ids2<-data.frame()
for(i in unique(s2$BirdID)){
  bird2<-filter(s2, s2$BirdID %in% i)
  birds2<-filter(sires2new, sires2new$BirdID %in% i)
  no2<-subset(bird2, bird2$chickyear %!in% birds2$chickyear)
  no2<-unique(no2)
  ids2<-rbind(no2,ids2)
}
ids2$ars<-0
# names(ids)[names(ids) == 'PeriodYear'] <- 'chickyear'
# ids<-ids[,-c(2)]

ids2<-ids2%>%
  mutate(SexEstimate=1)
ids2<-ids2[,-c(2)]


setdiff(sires2new$BirdID, ids2$BirdID)

#same situation as idd2

###########
#rbind yes offspring and no offspring 
# Female_rs<-rbind(idd,dars)
# Malears<-rbind(ids,sars)
# 
# Female_rs$sex<-0
# Malears$sex<-1
# annualReprosuccess<-rbind(Female_rs, Malears)
# 
# write.csv(annualReprosuccess, "ars_90day.csv")
# write.csv(Female_rs, 'femalers90day.csv')

# one year ----------------------------
Female_rs2<-rbind(idd2,dams2new)
Malears2<-rbind(ids2,sires2new)

# Female_rs2$sex<-0

Female_rs2<-arrange(Female_rs2, Female_rs2$BirdID, Female_rs2$chickyear)
# Malears2$sex<-1
annualReprosuccess2<-rbind(Female_rs2, Malears2)

write.csv(annualReprosuccess2, "ars_365.csv")
write.csv(Female_rs2, 'femalers_365.csv')


 
#i can just add 0 for the individuals with no lifetime reproductive success later




#survival 

lastseen_new<-sqlQuery(swdb, 'SELECT sys_StatusByFieldPeriod.BirdID, sys_StatusByFieldPeriod.FieldPeriodID, sys_StatusByFieldPeriod.Status, tblBirdID.BirthDate, tblFieldPeriodIDs.Island, tblFieldPeriodIDs.PeriodYear, tblBirdID.BirthFieldPeriodID, tblFieldPeriodIDs.PeriodEnd
FROM (sys_StatusByFieldPeriod INNER JOIN tblBirdID ON sys_StatusByFieldPeriod.BirdID = tblBirdID.BirdID) INNER JOIN tblFieldPeriodIDs ON sys_StatusByFieldPeriod.FieldPeriodID = tblFieldPeriodIDs.FieldPeriodID;
', stringsAsFactors=F)

lastseen_new<-filter(lastseen_new, lastseen_new$Island=="CN")

#save data new file to folder 
write.csv(lastseen_new, 'breedstatuses_20112024.csv')

#survival again 
lastseen_new$birthyear<-as.numeric(substr(lastseen_new$BirthDate, 1,4))
lastseen_new$age<-lastseen_new$PeriodYear-lastseen_new$birthyear

#assign survival column
lastseen_new$survival<-1
lastseen_new$survival[lastseen_new$Status=="NS"]<-0


#retain 2023 latest
survival23<-filter(lastseen_new, lastseen_new$PeriodYear<=2023)
alivebirds<-lastseen_new[(lastseen_new$Status!="NS" & lastseen_new$FieldPeriodID==186),]

#removed birds that are still alive from analysis
survivalanalysis<-filter(survival23, survival23$BirdID %!in% alivebirds$BirdID)
survivalanalysis$PeriodEndYear<-as.numeric(str_sub(survivalanalysis$PeriodEnd, 1,4))



#create ns for birds that have no NS 
for(i in unique(survivalanalysis$BirdID)){
  onebird<-filter(survivalanalysis, survivalanalysis$BirdID==i)
  if(all(onebird$survival!=0)){
    lastyear<-max(onebird$PeriodEndYear)#find the last year vector
    PeriodYear<-max(onebird$PeriodYear)
    age<-max(onebird$age)
    birthyear<-unique(onebird$birthyear)
    survivalanalysis<-survivalanalysis%>%
      add_row(BirdID=i, PeriodEndYear=lastyear+1, PeriodYear=PeriodYear+1, Status="NS", age=age+1, survival=0, birthyear=birthyear)
    #and then add a new row into the dataframe and assign the survival as 0
  }
}
 


survivalanalysis<-left_join(survivalanalysis,sex, by="BirdID")

#remove translocated bidrs 
translocated<-sqlFetch(swdb, "tblTranslocations", stringsAsFactor=F)
# do you have any reason to assume the survival proportion in the translocated individuals differs from the main population 
#what age did you translocate the birds 

translocated<- translocated[,c("BirdID","ReleaseDate")]
translocated$releaseyear<-as.numeric(str_sub(translocated$ReleaseDate, 1,4))
translocated<-left_join(translocated, tblbirdid, by='BirdID')
translocated$age<-translocated$releaseyear-translocated$birthyear
hist(translocated$age)
hist(translocated$birthyear,20)

survivalanalysis2<-filter(survivalanalysis, survivalanalysis$BirdID %!in% translocated$BirdID)


survivalanalysis2$PeriodEnd<-as.Date(survivalanalysis2$PeriodEnd, '%Y-%m-%d')


#remove extra status 
fps<-readxl::read_excel('fps.xlsx')
fps$PeriodEnd<-as.Date(fps$PeriodEnd, "%Y-%m-%d")
fps$PeriodStart<-as.Date(fps$PeriodStart, "%Y-%m-%d")
fps$seasonlength<-fps$PeriodEnd-fps$PeriodStart
fps<-fps%>%
  group_by(PeriodYear)%>%
  mutate(season=case_when(seasonlength == max(seasonlength)~"main",
                          seasonlength != max(seasonlength)~"winter"))
fps<-as.data.frame(fps)


#join fp 
test<-left_join(survivalanalysis2, fps, by=c('FieldPeriodID', 'PeriodYear',"Island"))


test<-test%>%group_by(BirdID, PeriodYear)%>%mutate(no_st=length(Status))

#if bird has more than one status that year, 
#take status of the fieldperiod that is summer 
morethan1<-test|>filter(no_st>1)

mainst<-morethan1|>
  group_by(BirdID, PeriodYear)|>
  arrange(desc(Status == "NS"), desc(season == "main")) %>% # Prioritize NS, then main season
  slice(1)
#if there is main, take main, if no main take one randomly or ns

onest<-test|>filter(no_st==1)

survivaldf<-rbind(mainst, onest)

#should be okay now. duplicated statuses in a year removed 


#for each bird, summarize check if theres more than 1 NS 

n_distinct(survivaldf$BirdID)  #3611 birds
sum(survivaldf$Status=='NS')   #3699 NS statuses

survivaldf<-survivaldf%>%
  group_by(BirdID)%>%
  mutate(nrns=sum(Status=="NS"))

#remove the double NS 
onens <- survivaldf%>%filter(nrns==1)

multiplens<-survivaldf%>%filter(nrns>1)
multiplens<-as.data.frame(multiplens)

getrid<-data.frame()
for(i in unique(multiplens$BirdID)){
  onebirdns<-filter(multiplens, multiplens$BirdID==i& multiplens$Status=='NS')
  onebirdns<-arrange(onebirdns, onebirdns$PeriodEnd.x)
  onetormv<-onebirdns[-c(1),]
  getrid<-rbind(getrid,onetormv)
}


test<-anti_join(survivaldf, getrid)

survivaldata<-test%>%
  arrange(BirdID, PeriodYear)

#save data
write.csv(survivaldata, 'survival_updated.csv')

####################################
#getting survival probability  


latestlifespan<-read.csv('C:/PhD/Data/lifespan_171224.csv',stringsAsFactors = F)
latestlifespan<-latestlifespan[,-c(1)]
survivaldata<-read.csv('survival_updated.csv')
survivaldata<-survivaldata[,-c(1)]
test2<-left_join(survivaldata, latestlifespan, by='BirdID')
# test2<-test2[,-c(20)]  #what the fuck was this?? 
check<-test2%>%
  filter(is.na(lifespan))

survivaldata2<-test2%>%filter(!is.na(lifespan))

#remove fledglings 
survivaldatafinal<-survivaldata2%>%filter(lifespan>90)

survivaldata_all<-filter(survivaldatafinal, survivaldatafinal$birthyear>=1992)
survivaldata_all<-filter(survivaldata_all, survivaldata_all$age>=0)   #there was age -1

# test<-survivaldata_all%>%
#   group_by(BirdID, age)%>%
#   summarize(count=length(survival))
#bird 823 and bird 1268 at ages 9 and 5 rsp have more than 1 status. 
#no need now- fixed the filtering before this 


#looking at age when bird first ringed 
test<-survivaldata_all%>%
  group_by(BirdID)%>%
  summarise(minage=min(age))

not0<-filter(test,test$minage>0)
not0<-unique(not0$BirdID)

#bird ID 4847 caught age 3, 3022 and 3495 caught age 2 
#4847 was migrated from CE, 3022 is normal, 3495 is ur 
#remove 4847 and 3495

survivaldata_all<-filter(survivaldata_all, survivaldata_all$BirdID %!in% c(4847,3495))


addto<-subset(survivaldata_all, survivaldata_all$BirdID %in% not0)

#add rows for individuals not entering df at age 0


# test2<-test2%>%
#   group_by(BirdID)%>%
#   complete(PeriodYear=full_seq(unique(birthyear):max(PeriodYear),1),
#            fill=list(survival=1, age=0, birthyear=unique(birthyear)))


# complete df 
#for each unique bird id, 
#if birth year is before min(periodyear)
#add a row for each year the bird was not observed between birth year and min(periodyear)


#remove age and survival
addto_zero <- addto %>% distinct(BirdID,.keep_all = T) %>% 
  dplyr::select(-age, -survival) %>% 
  mutate(age = 0, survival = 1, PeriodYear=birthyear)

#cuz its all 0 now max(age) is ok
addto_zero %>% summarise(minage = max(age), meansurv = mean(survival))

addto_one <- addto %>% 
  arrange(age) %>% 
  distinct(BirdID,.keep_all = T) %>%   #remove dups i think 
  filter(age>1) %>% 
  dplyr::select(-age, -survival) %>% 
  mutate(age = 1, survival = 1, PeriodYear=birthyear+1)

survivaldata_all_zero <- rbind(survivaldata_all,addto_zero) %>% rbind(.,addto_one )
survivaldata_all_zero<-survivaldata_all_zero%>%arrange(BirdID,age)


n_distinct(survivaldata_all_zero$BirdID)
survivaldata_all_zero %>% filter(age==0) %>% {n_distinct(.$BirdID)}

#check again
test2<-survivaldata_all_zero%>%
  group_by(BirdID)%>%
  summarise(minage=min(age))

#chuen checking if the birds have the right number of rows by checking if n() = lifespan per bird
survdatarows <- survivaldata_all_zero %>% 
  group_by(BirdID) %>%
  mutate(numberofrows = n()) %>%
  arrange(desc(age)) %>%
  distinct(BirdID,.keep_all = T) %>%
  mutate(conflict = case_when((numberofrows-1) == age ~ "goodgood", TRUE ~ "notgood")) %>%
  filter(conflict %in% "notgood") # four birds with missing values,let's fix them
# manually check all four bird IDs: 470 missing age 7, 561 missing age 6, 599 missing 6, 640 also missing 6

#470 also has period year 1993 twice, age 0 and 1 



finalsurvival<-survivaldata_all_zero

write.csv(finalsurvival,'cleansurvivaldata.csv')

###############################
#leslie matrix stuff 

finalsurvival<-read.csv('cleansurvivaldata.csv')
finalsurvival<-finalsurvival[,-c(1)]


#.SD means to subset , selfsame or self reference the data 

#separate survival into cohorts 
cohorts<- split(finalsurvival, finalsurvival$birthyear)
# x<-cohorts[['2005']]
# x<-as.data.frame(cohorts[['2012']])
# t<-1
# & (nextage$BirdID %in% oneage$BirdID)

#issues caused by :
#either first appeared was already 0 
#caught at age 2 not seen at age 0 and 1 
#everything past age 2 is fine 

# troubleshooting 
# dead1<-filter(nextage, nextage$survival==0 & (nextage$BirdID %in% oneage$BirdID)) #49
# dead2<-filter(nextage, nextage$survival==0)#57
# #in 2014 there are 8 birds in age t+1 died not in age t 
# 
# length(unique(dead1$BirdID))
# setdiff(dead2$BirdID, dead1$BirdID)
# 
# dead3<-filter(nextage, nextage$survival==0 & (nextage$BirdID %in% oneage$BirdID)) #5
# dead4<-filter(nextage, nextage$survival==0) #5 

#maybe need to add any potential birds that are dead 

pxfunc<-function(x){
  x<-as.data.frame(x)
  onecohort<-data.frame()
  for(t in unique(x$age)){    # i should not need a counter for this
    oneage<-filter(x, x$age==t & x$survival==1)  #birds alive at age t #should work? 
    nextage<-filter(x, x$age==(t+1))   
    og<-length(unique(oneage$BirdID))
  # appeared<-length(setdiff(nextage$BirdID, oneage$BirdID)) #dont need this after fixing 
    dead<-nrow(filter(nextage, nextage$survival==0)) #length of those at age 1 that survival 0. #is it enough to just tally the number of 0?  
    px<-(og-dead)/(og)
    #add appeared 2
    onerow<-data.frame(age=t,px=px)
    onecohort<-rbind(onecohort,onerow) #but need to remove last age 
  }
  onecohort<-onecohort[-c(nrow(onecohort)),]
  return(onecohort)
}

survprob<-lapply(cohorts, pxfunc)
#checking cohort 12 calcs from age 6-11 is correct. 

# check 2003
finalsurvival %>% filter(birthyear %in% "2003") %>%
  group_by(age,survival) %>% summarise(n=n())



#for survival need to remove cohorts before 1992 and after 2020 due to lack of corresponding reproductive success data 
#also need to remove last row for last age from all dataframes before making leslie matrix. 


#translocations? 


#############################################################
#age specific fertility 


# #get er 
# ars_90<-read.csv('ars_90day.csv')
# ars_90<-arrange(ars_90, ars_90$BirdID, ars_90$chickyear)
# ars_90<-ars_90[,-c(1)]

tblbirdid<-sqlFetch(swdb, 'tblBirdID', stringsAsFactors=F)
tblbirdid$birthyear<-as.numeric(str_sub(tblbirdid$BirthDate, 1,4))
tblbirdid<-tblbirdid[,c('BirdID', 'birthyear')]
# ars_90<-left_join(ars_90, tblbirdid, by='BirdID')
# ars_90$age<-ars_90$chickyear-ars_90$birthyear
# ars_90<-unique(ars_90)
#there is still one individual with age -1 


#female 90 days
# 
# female90rs<-read.csv('femalers90day.csv')
# female90rs<-female90rs[,-c(1)]
# 
# female90rs<-arrange(female90rs, female90rs$BirdID, female90rs$chickyear)
# 
# female90rs<-left_join(female90rs, tblbirdid, by='BirdID')
# female90rs$age<-female90rs$chickyear-female90rs$birthyear
# female90rs<-unique(female90rs)

################# one year --------
ars_365<-read.csv('ars_365.csv')
ars_365<-arrange(ars_365, ars_365$BirdID, ars_365$chickyear)
ars_365<-ars_365[,-c(1)]

ars_365<-left_join(ars_365, tblbirdid, by='BirdID')
ars_365$age<-ars_365$chickyear-ars_365$birthyear
ars_365<-unique(ars_365)

############################################################

#use this!!!!!!!!!!!!!!!


#female 365 days


female365rs<-read.csv('femalers_365.csv')
female365rs<-female365rs[,-c(1)]

# female365rs<-arrange(female365rs, female365rs$BirdID, female365rs$chickyear)

female365rs<-left_join(female365rs, tblbirdid, by='BirdID')
female365rs$age<-female365rs$chickyear-female365rs$birthyear
female365rs<-unique(female365rs)

#separate into cohorts 
# fertilitycohorts<-split(ars_90, ars_90$birthyear)
# 
# fertcohort2<-split(ars_365, ars_365$birthyear)



##female offspring only 
# fxco<-split(female90rs, female90rs$birthyear)
fxco2<-split(female365rs, female365rs$birthyear)



# x<-fertcohort2[['2011']]
# t<-2

#function to get age specific fertility 

mxfunc<-function(x){
  x<-as.data.frame(x)
  onecohort<-data.frame()
  for(t in unique(x$age)){    #for each age in a cohort,   
    oneage<-filter(x, x$age==t)  #filter: one dataset for each age of each cohort 
    mx<-mean(oneage$ars)*0.5     #find the expected ars, 0.5 because female offspring of females only, but also fitness is halved
    #unknowns: mx should be half female female offspring? or half all offspring? or complete female offspring
    onerow<-data.frame(age=t,mx=mx)
    onecohort<-rbind(onecohort,onerow) #but need to remove last age 
  }
  return(onecohort)
}


# fx<-lapply(fertilitycohorts, mxfunc)  #90 day ars 
# fx2<-lapply(fertcohort2, mxfunc)  # one year ars 

# realfx<-lapply(fxco,mxfunc)
realfx2<-lapply(fxco2,mxfunc)

###########################################

#code for finding px and mx for the whole population over all time 



#px (survival probabilty)
px_all<-data.frame()
for(t in unique(finalsurvival$age)){   
    oneage<-filter(finalsurvival, finalsurvival$age==t & finalsurvival$survival==1)  #birds alive at age t  
    nextage<-filter(finalsurvival, finalsurvival$age==t+1)   
    og<-length(unique(oneage$BirdID))
    # appeared<-length(setdiff(nextage$BirdID, oneage$BirdID))
    dead<-nrow(filter(nextage, nextage$survival==0)) #length of those at age 1 that survival 0. #is it enough to just tally the number of 0?  
    px<-(og-dead)/(og)
    onerow<-data.frame(age=t,px=px)
    px_all<-rbind(px_all,onerow) #but need to remove last age 
  }
px_all<-px_all[-c(21),]


#age0dead<-filter(survivaldata_all, survivaldata_all$age==0, survival==0)
#there are 0 dead at age 0. 


#mx (fecundity) --------------------------
# use female365rs

# fxall_90<-data.frame()
# 
# for(t in unique(female90rs$age)){    #for each age in a cohort,   
#     oneage<-filter(female90rs, female90rs$age==t)  #filter: one dataset for each age of each cohort 
#     mx<-mean(oneage$ars)*0.5     #find the expected ars, 0.5 because female offspring of females only, but also fitness is halved
#     #unknowns: mx should be half female female offspring? or half all offspring? or complete female offspring
#     onerow<-data.frame(age=t,mx=mx)
#     fxall_90<-rbind(fxall_90,onerow) #but need to remove last age 
#   }


fxall_365<-data.frame()

for(t in unique(female365rs$age)){    #for each age in a cohort,   
    oneage<-filter(female365rs, female365rs$age==t)  #filter: one dataset for each age of each cohort 
    mx<-mean(oneage$ars)*0.5     #find the expected ars, 0.5 * all offspring of females only, but also fitness is halved
    #mx should be half all offspring? or complete female offspring
    onerow<-data.frame(age=t,mx=mx)
    fxall_365<-rbind(fxall_365,onerow) #but need to remove last age 
  }

###############################################

#create leslie matrix

#dataframe containing fx and mx 


#for population of all time
# pxfx_90<-merge(px_all, fxall_90, by='age')
pxfx_365<-merge(px_all, fxall_365, by='age')


ggplot(pxfx_365, aes(x=age,y=px))+geom_point() #sub mx for px for other graph 


#for different cohorts 
#remove cohorts 1972-1991
survprob<-survprob[c(1:29)]
realfx2

# function(x,y){merge(x[[i]],y[[j]], by='age')}

pxfx_cohorts<-lapply(names(survprob), 
                     function(x){merge(survprob[[x]], realfx2[[x]], by='age')} )

names(pxfx_cohorts)<-names(survprob)

# merge(x,y, by='age')


#px is NOT CUMULATIVE 



#get n for populations -------------------


n_all<-data.frame()
for(t in unique(finalsurvival$age)){   
  oneage<-filter(finalsurvival, finalsurvival$age==t & finalsurvival$survival==1)  #birds alive at age t  
  og<-length(oneage$BirdID)
  onerow<-data.frame(age=t,nx=og)
  n_all<-rbind(n_all,onerow) #but need to remove last age 
}

# check if the two methods (n_all and px_all) matches up
n_all_div <- n_all %>% 
  mutate(diff = lead(nx)/nx) %>% 
  merge(.,px_all,by="age",all=T) %>%
  mutate(conflict = case_when(diff == px ~ "goodgood", TRUE ~ "notgood"))
#yay i fixed it 
#qqboo

#population for the cohorts now 

nfunc<-function(x){
  x<-as.data.frame(x)
  n_cohort<-data.frame()
  for(t in unique(x$age)){   
    oneage<-filter(x, x$age==t & x$survival==1)  #birds alive at age t  
    og<-length(unique(oneage$BirdID))
    onerow<-data.frame(age=t,nx=og)
    n_cohort<-rbind(n_cohort,onerow) #but need to remove last age 
  }
  return(n_cohort)
}

#-dead

nbycohort<-lapply(cohorts, nfunc)


nbycohort<-nbycohort[c(1:29)]

#list of all cohorts with px, mx and nx 
all_bycohort<-lapply(names(pxfx_cohorts), 
                     function(x){merge(pxfx_cohorts[[x]],nbycohort[[x]], by='age')})

names(all_bycohort)<-names(pxfx_cohorts)



#population size and px and fx for all time 
pxfxnx_alltime<-merge(pxfx_365, n_all, by='age')

#save objects 
saveRDS(all_bycohort, 'pxmxnx_bycohort.rds')

write.csv(pxfxnx_alltime, 'pxfxnx_all.csv')

#fx should be mx because fx=sxmx

# estimate asr -------
# males<-filter(survivaldatafinal, survivaldatafinal$age==0 &survivaldatafinal$SexEstimate==1)
# females<-filter(survivaldatafinal, survivaldatafinal$age==0 &survivaldatafinal$SexEstimate==0)
# m to f at age 1 is 0.962 (female biased?)
# at age 0 is 0.952  
# 


#####################################################
#make leslie matrix 
library(demogR)
library(mpmtools)

all_bycohort<-read_rds('pxmxnx_bycohort.rds')

pxfxnx_alltime<-read.csv('pxfxnx_all.csv')
pxfxnx_alltime<-pxfxnx_alltime[,-c(1)]

# colnames(pxfxnx_alltime)<-c('x','sx','mx','nx')  
#sx for survival, mx for fecundity, x for age, nx for popn size

# bla<-make_Leslie_matrix(pxfxnx_alltime, model='post')
#this gives me weird values 


leslie_all<-matrix(0,20,20)

leslie_all[1,]<-pxfxnx_alltime$mx
for (i in 2:20) {
  leslie_all[i, i-1] <- pxfxnx_alltime$px[i-1]
}



lambda_all<-lambda1(leslie_all)  
# 0.9279268
0.9223668


#so technically you dont need the population size to calculate the dominant eigenvalue


#so no i dont want n(t+1)/n(t) (should give the same value though)

#################################################################################
#doing it per cohort 



lesliefunc<-function(data){
  n<-nrow(data)
  leslie<-matrix(0,n,n)
  leslie[1,]<-data$mx
  for (i in 2:nrow(data)) {
    leslie[i, i-1] <- data$px[i-1]
  }
  return(leslie)
}

leslie_percohort<-lapply(all_bycohort, lesliefunc)


lambdapercohort<-sapply(leslie_percohort, lambda1)

lambda_df<-data.frame(year=names(lambdapercohort),lambda=lambdapercohort)


ggplot(lambda_df, aes(x=year, y=lambda))+geom_point()+geom_line()


#looking at this there seems to be too much stochasticity. 

#testing for stable age distribution 


eigen<-data.frame(coeff=c(pxfxnx_alltime$px), n=c(0:19))
eigen<-eigen[-nrow(eigen),]

# eigen$n<-paste('n', eigen$n)
eigen$coeff_L<-eigen$coeff/lambda_all

eigen$coeff_new<-paste(eigen$coeff_L,'n', eigen$n)
eigen$n<-as.numeric(c(1:19))
eigen$lead<-cumprod(eigen$coeff_L)


eigen$mx<-pxfxnx_alltime$mx[2:20]

eigen$equa<-eigen$lead*eigen$mx
# eigen$n<-as.numeric(c(1:19))
eigen<-rbind(eigen,data.frame(coeff=-0.9250696433, n=0, coeff_L=NA,coeff_new='n0',lead=1,mx=NA,equa=-0.9250696433))
eigen<-eigen%>%
  arrange(n)

eigen$n<-paste('n',eigen$n)

eigen$sad<-eigen$lead*2099

#depends on if i did it correctly, I don't think the population meets stable age distribution.. 


######################################################################################

#survival shadow 
# selection<-(csurv*e^(-lambda_all*age))/px

e<-exp(1)
r<-log(lambda_all)

#y is 1

pxfxnx_alltime<-as.data.frame(pxfxnx_alltime)

survshadow<-pxfxnx_alltime%>%
  mutate(cumsurv=cumprod(px), 
         ly=lead(cumsurv),
         cumsurvery=ly*e^(-r*(x+1))) %>%
  arrange(desc(age)) %>%
  filter(!(age %in%"19")) %>%
  mutate(selection=cumsum(cumsurvery)/px)%>%
  arrange(age)

survplot<-ggplot(survshadow, aes(age, selection))+geom_point()+
  geom_line()+ylab('Hamiltonian Strength of Selection')+xlab("Age")+theme_classic()


survplot






survshadow<-data.frame()
for(i in 1:nrow(pxfxnx_alltime)){
  # x<-seq(1:i+1)
  age<-(i-1)
  lx<-cumprod(pxfxnx_alltime$sx[1:i])
  lx<-lx[-c(1)]
  lx<-lx*e^(-lambda_all*age)
  upper<-cumsum(that)
  beta<-upper/pxfxnx_alltime$sx[i]
  onerow<-c(age,beta)
  survshadow<-rbind(survshadow, onerow)
}


#######################

#fertility survival 


view(pxfxnx_alltime)


fert<-pxfxnx_alltime
fert$lx<-cumprod(pxfxnx_alltime$px)

#ravindran 
fert$selection<-fert$lx*e^(-r*fert$age)

#from baudisch
fert$selection2<-(fert$lx*fert$mx)*e^(-r*fert$age)

#hamilton vs baudisch method 

fert$upper<-fert$lx*e^(-r*fert$age)
fert$prelower<-fert$age*fert$upper*fert$mx 

fert$lower<-


#plotting 
fert<-pivot_longer(fert,cols = 6:7, names_to = 'selection')

ggplot(fert, aes(x=age, y=value, colour=selection))+geom_point()+geom_line()





############################
#just for fun

library(ggplot2)
blah<-do.call(rbind, pxfx_cohorts)
blah$cohort<-as.numeric(str_sub(rownames(blah),1,4))
blah<-filter(blah, blah$age<5)
blah$age<-as.factor(blah$age)

ggplot(blah, aes(x=blah$cohort, y=blah$mx,colour = blah$age))+geom_point()+geom_smooth(method='lm')
  ylim(0,1)


####################


hist(survivalanalysis$age)
hist(physio$age_year)

survprop<-survivalanalysis%>%
  group_by(age)%>%
  summarize(prop=length(age)/nrow(survivalanalysis))

sum(survprop$prop)
physprop<-physio%>%
  group_by(age_year)%>%
  summarize(prop=length(age_year)/nrow(physio))

#check whether catches are proportional across ages and size 
#from survival, 

hist(physio$BodyMass)

#is r the same? 
#no its nr births minus nr deaths 


#get age specific survival 
#separate by cohort (birth year)
#get no of deaths per age 


#################################
#leslie matrix from 1997 to 2013 
surv97_13<-filter(finalsurvival, finalsurvival$birthyear>1996 & finalsurvival$birthyear<2014)
px_9713<-data.frame()
for(t in unique(surv97_13$age)){   
  oneage<-filter(surv97_13, surv97_13$age==t & surv97_13$survival==1)  #birds alive at age t  
  nextage<-filter(surv97_13, surv97_13$age==t+1)   
  og<-length(unique(oneage$BirdID))
  # appeared<-length(setdiff(nextage$BirdID, oneage$BirdID))
  dead<-nrow(filter(nextage, nextage$survival==0)) #length of those at age 1 that survival 0. #is it enough to just tally the number of 0?  
  px<-(og-dead)/(og)
  onerow<-data.frame(age=t,px=px)
  px_9713<-rbind(px_9713,onerow) #but need to remove last age 
}
px_9713<-px_9713[-c(20),]


rs_9713<-filter(female365rs, female365rs$birthyear>1996, female365rs$birthyear<2014)

fx_9713<-data.frame()

for(t in unique(rs_9713$age)){    #for each age in a cohort,   
  oneage<-filter(rs_9713, rs_9713$age==t)  #filter: one dataset for each age of each cohort 
  mx<-mean(oneage$ars)*0.5     #find the expected ars, 0.5 * all offspring of females only, but also fitness is halved
  #mx should be half all offspring? or complete female offspring
  onerow<-data.frame(age=t,mx=mx)
  fx_9713<-rbind(fx_9713,onerow) #but need to remove last age 
}


leslie_9713<-matrix(0,18,18)

leslie_9713[1,]<-fx_9713$mx[1:18]
for (i in 2:18) {
  leslie_9713[i, i-1] <- px_9713$px[i-1]
}

lambda1(leslie_9713)

# 0.9497799 
0.9469736

######################################################################################


# sex specific leslie matrix analysis 


#split ars 
ars_fem<-ars%>%filter(SexEstimate==0)
ars_male<-ars%>%filter(SexEstimate==1)


#split survival 

surv_fem<-filter(finalsurvival, finalsurvival$SexEstimate==0)
  
surv_male<-filter(finalsurvival, finalsurvival$SexEstimate==1)


#female 

fem_df<-pxfunc(surv_fem)  #px


#mx

fx_fem<-data.frame()

for(t in unique(ars_fem$age)){    #for each age in a cohort,   
  oneage<-filter(ars_fem, ars_fem$age==t)  #filter: one dataset for each age of each cohort 
  mx<-mean(oneage$ars)*0.5     #find the expected ars, 0.5 * all offspring of females only, but also fitness is halved
  #mx should be half all offspring? or complete female offspring
  onerow<-data.frame(age=t,mx=mx)
  fx_fem<-rbind(fx_fem,onerow) #but need to remove last age 
}

fem_pxmx<-merge(fem_df, fx_fem, by='age')

#leslie 19x19

fem_leslie<-matrix(0,20,20)

fem_leslie[1,]<-fem_pxmx$mx
for (i in 2:20) {
  fem_leslie[i, i-1] <- fem_pxmx$px[i-1]
}

lambda_fem<-lambda1(fem_leslie)
0.9458425

#
e<-exp(1)
rf<-log(lambda_fem)

fem_pxmx<-as.data.frame(fem_pxmx)

survshadow_fem<-fem_pxmx%>%
  mutate(cumsurv=cumprod(px), 
         ly=lead(cumsurv),
         cumsurvery=ly*e^(-rf*(age+1))) %>%
  arrange(desc(age)) %>%
  filter(!(age %in%"19")) %>%
  mutate(selection=cumsum(cumsurvery)/px)%>%
  arrange(age)



#####################################################




#male 

male_df<-pxfunc(surv_male)


fx_male<-data.frame()

for(t in unique(ars_male$age)){    #for each age in a cohort,   
  oneage<-filter(ars_male, ars_male$age==t)  #filter: one dataset for each age of each cohort 
  mx<-mean(oneage$ars)*0.5     #find the expected ars, 0.5 * all offspring of females only, but also fitness is halved
  #mx should be half all offspring? or complete female offspring
  onerow<-data.frame(age=t,mx=mx)
  fx_male<-rbind(fx_male,onerow) #but need to remove last age 
}

male_pxmx<-merge(male_df, fx_male, by='age')

#leslie 19x19

male_leslie<-matrix(0,19,19)

male_leslie[1,]<-male_pxmx$mx
for (i in 2:19) {
  male_leslie[i, i-1] <- male_pxmx$px[i-1]
}


lambda1(male_leslie)
rm<-0.9439132

#population growth is similar between the two 
#but makes sense 



survshadow_male<-male_pxmx%>%
  mutate(cumsurv=cumprod(px), 
         ly=lead(cumsurv),
         cumsurvery=ly*e^(-rm*(age+1))) %>%
  arrange(desc(age)) %>%
  filter(!(age %in%"18")) %>%
  mutate(selection=cumsum(cumsurvery)/px)%>%
  arrange(age)



#combine survival 
survshadow_male$sex<-1
survshadow_fem$sex<-0

surv_femmale<-rbind(survshadow_fem, survshadow_male)

surv_femmale$sex<-as.factor(surv_femmale$sex)

femmaleplot<-ggplot(surv_femmale, aes(age, selection, col=sex))+geom_point()+
  geom_line()+ylab('Hamiltonian Strength of Selection')+xlab("Age")+theme_classic()


femmaleplot


#################################################################

#fertility 



#female 

fert_fem<-fem_pxmx

fert_fem$lx<-cumprod(fem_pxmx$px)


#from baudisch
fert_fem$selection<-(fert_fem$lx*fert_fem$mx)*e^(-rf*fert_fem$age)


#male 
fert_male<-male_pxmx

fert_male$lx<-cumprod(fert_male$px)


#from baudisch
fert_male$selection<-(fert_male$lx*fert_male$mx)*e^(-rm*fert_male$age)



# combine selection on fertility 
fert_fem$sex<-'Female'
fert_male$sex<-'Male'

mx_femmale<-rbind(fert_fem,fert_male)

ggplot(mx_femmale, aes(x=age, y=selection, colour=sex))+geom_point()+geom_line()


px_separate<-ggplot(mx_femmale, aes(x=age, y=px, colour=sex))+geom_point()+geom_line()
mx_separate<-ggplot(mx_femmale, aes(x=age, y=mx, colour=sex))+geom_point()+geom_line()

