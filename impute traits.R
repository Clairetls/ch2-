#impute data for traits 

physio<-read.csv('physio_28_5.csv')
missingdf<-physio[,c('newlifespan','age_year','BodyMass',
                     'RightTarsus','avg_invert')]

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

#maybe i should add the rows that are bird alive but not caught 
missingness<-ifelse(is.na(missingdf)==TRUE, 0,1)
missdata<-data.frame(missingdf,missingness)
pairs.panels(missdata, ellipses=F, method = 'spearman')


#find out which birds are missing what years 
#create rows for them (major field season)
#fill in major field season 


