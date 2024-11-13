pedigree<-read.csv('updated pedigree.csv', sep=';')
lifespan<-read.csv('lifespan.csv')
lastseen<-read.csv('lastseen.csv',sep=';')
lifespan<-lifespan[,-c(1)]
"%!in%" <-Negate("%in%")

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


#get r from leslie matrix 








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




