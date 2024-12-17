pedigree<-read.csv('updated pedigree.csv', sep=';')
lifespan<-read.csv('lifespan_28_5_24.csv')
lastseen<-read.csv('lastseen.csv',sep=';')
lifespan<-lifespan[,-c(1)]
"%!in%" <-Negate("%in%")

library(RODBC)
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"

#change to path of your database 
MDBPATH <- "C:/Users/P309444/Downloads/database day 191124/SeychellesWarbler1.11.1.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)


#wait whats this for??? 
# eh<-read.csv('updated pedigree.csv', sep=';')
# eh<-filter(eh, is.na(eh$GenMumConfidence))


############################################
library(tidyverse)
pedigree<-left_join(pedigree,lifespan, by="BirdID")

pedigree<-filter(pedigree, pedigree$GenDadConfidence>=80 & pedigree$GenMumConfidence>=80)

#subset pedigree to exclude individuals older than 90 days 
test<-filter(pedigree, pedigree$newlifespan>90|is.na(pedigree$newlifespan))

#NAs<-filter(test, is.na(test$newlifespan))  check NAs 
filteredped<-test

filteredped$chickyear<-as.numeric(str_sub(filteredped$BirthDate, 7))

dams<-list()
for(i in filteredped$BirdID){
  dam<-data.frame()
  dam<-filter(filteredped, filteredped$GeneticMother==i)
  dams[[i]]<-dam
}

sires<-list()
for(i in filteredped$BirdID){
  sire<-data.frame()
  sire<-filter(filteredped, filteredped$GeneticFather==i)
  sires[[i]]<-sire
}

dams<-do.call(rbind.data.frame, dams)
sires<-do.call(rbind.data.frame, sires)


sires$chickyear<-str_sub(sires$BirthDate, 7,10)


dars<-data.frame()
dars<-dams%>%
  group_by(GeneticMother, chickyear)%>%
  mutate(ars=length(chickyear))  #originally this row is summarize to make new df



dars<-as.data.frame(dars)
names(dars)[names(dars) == 'BirdID'] <- 'ChickBirdID'
names(dars)[names(dars) == 'GeneticMother'] <- 'BirdID'
dars<-dars[,c("NestID","BirdID","chickyear","ars")]


sars<-data.frame()
sars<-sires%>%
  group_by(GeneticFather, chickyear)%>%
  mutate(ars=length(chickyear))  #originally this row is summarize to make new df



sars<-as.data.frame(sars)
names(sars)[names(sars) == 'BirdID'] <- 'ChickBirdID'
names(sars)[names(sars) == 'GeneticFather'] <- 'BirdID'
sars<-sars[,c("BirdID","chickyear","ars","NestID")]


#fill in the gaps where they werent reproducing

#mom
d<-subset(lastseen, lastseen$BirdID %in% dars$BirdID) #gets genetic mom
d<-d[,c("BirdID","PeriodYear")]
d$chickyear<-d$PeriodYear
d<-unique(d)

idd<-data.frame()
#i<-444
for(i in unique(d$BirdID)){
  bird<-filter(d, d$BirdID==i)
  birds<-filter(dars, dars$BirdID==i)
  no<-subset(bird, bird$chickyear %!in% birds$chickyear)
  no<-unique(no)
  idd<-rbind(no,idd)
}
idd$ars<-0

# names(idd)[names(idd) == 'PeriodYear'] <- 'chickyear'
# names(idd)[names(idd) == 'FieldPeriodID'] <- 'BirthFieldPeriodID'

idd<-idd%>%
  mutate(NestID=NA)
idd<-idd[,-c(2)]


#dad turn
s<-subset(lastseen, lastseen$BirdID %in% sars$BirdID) #gets genetic dad
s<-s[,c("BirdID","PeriodYear")]
s$chickyear<-s$PeriodYear
s<-unique(s)

sars<-as.data.frame(sars)

ids<-data.frame()
for(i in unique(s$BirdID)){
  bird<-filter(s, s$BirdID %in% i)
  birds<-filter(sars, sars$BirdID %in% i)
  no<-subset(bird, bird$chickyear %!in% birds$chickyear)
  no<-unique(no)
  ids<-rbind(no,ids)
}
ids$ars<-0
# names(ids)[names(ids) == 'PeriodYear'] <- 'chickyear'
# ids<-ids[,-c(2)]

ids<-ids%>%
  mutate(NestID=NA,)
ids<-ids[,-c(2)]


Female_rs<-rbind(idd,dars)
Malears<-rbind(ids,sars)

Female_rs$sex<-0
Malears$sex<-1
annualReprosuccess<-rbind(Female_rs, Malears)

write.csv(annualReprosuccess, "ars_90day.csv")
 
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
 

sex<-read.csv('sys_SexEstimates.csv', sep=';')
sex<-sex[,c(1,2)]
survivalanalysis<-left_join(survivalanalysis,sex, by="BirdID")

#remove translocated bidrs 
translocated<-sqlFetch(swdb, "tblTranslocations", stringsAsFactor=F)

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
mainst<-morethan1|>filter(season=="main"|Status=='NS')

onest<-test|>filter(no_st==1)

survivaldf<-rbind(mainst, onest)

#should be okay now. duplicated statuses in a year removed 


#for each bird, summarize check if theres more than 1 NS 

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

test2<-left_join(survivaldata, latestlifespan, by='BirdID')
test2<-test2[,-c(20)]
check<-test2%>%
  filter(is.na(lifespan))

survivaldata2<-test2%>%filter(!is.na(lifespan))

#remove fledglings 
survivaldatafinal<-survivaldata2%>%filter(lifespan>90)



#separate survival into cohorts 
cohorts<- split(survivaldatafinal, survivaldatafinal$birthyear)

pxfunc<-function(x){
  x<-as.data.frame(x)
  onecohort<-data.frame()
  for(t in unique(x$age)){    # i should not need a counter for this
    oneage<-filter(x, x$age==t)
    nextage<-filter(x, x$age==t+1)   #is including age t+1 enough? #there are only 39 birds ringed at age 2 so is negligible. 
    og<-length(oneage$BirdID)
    appeared<-length(setdiff(nextage$BirdID, oneage$BirdID)) #keep this 
    dead<-nrow(filter(nextage, nextage$survival==0 & (nextage$BirdID %in% oneage$BirdID))) #length of those at age 1 that survival 0. #is it enough to just tally the number of 0?  
    px<-(og+appeared-dead)/(og+appeared)
    onerow<-data.frame(age=t,px=px)
    onecohort<-rbind(onecohort,onerow) #but need to remove last age 
  }
  return(onecohort)
}

survprob<-lapply(cohorts, pxfunc)

#Remember 

#dead<-length(setdiff(oneage$BirdID, nextage$BirdID))

#what will happen in the last age??? 
#next age will be empty, there will be none not dead, none appeared 

#pseudocode 
# for each cohort, find BirdIDs at age t
#find same BirdIDs at age t+1 
























#get er 
ars_90<-read.csv('ars_90day.csv')
ars_90<-arrange(ars_90, ars_90$BirdID, ars_90$chickyear)
ars_90<-ars_90[,-c(1)]

tblbirdid<-sqlFetch(swdb, 'tblBirdID', stringsAsFactors=F)
tblbirdid$birthyear<-as.numeric(str_sub(tblbirdid$BirthDate, 1,4))
tblbirdid<-tblbirdid[,c('BirdID', 'birthyear')]
ars_90<-left_join(ars_90, tblbirdid, by='BirdID')
ars_90$age<-ars_90$chickyear-ars_90$birthyear
#there is still one individual with age -1 


#test may work. should have gotten rid of individuals with double ns 


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










#how many times has one bird come up in the Genetic mothers and Genetic fathers 
# dams<-filteredped%>%
#   group_by(GeneticMother)%>%
#   summarise(lifetimeRS=length(GeneticMother))%>%
#   mutate(sex=0)
# 
# sires<-filteredped%>%
#   group_by(GeneticFather)%>%
#   summarise(lifetimeRS=length(GeneticFather))%>%
#   mutate(sex=1)
# 
# mom<-dams$GeneticMother
# dad<-sires$GeneticFather
# 
# breeders<-c(mom,dad)


#i dont think i can do the above because of formula 


