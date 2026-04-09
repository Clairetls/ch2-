#tars and bm? 

library(tidyverse)
library(mice)

#read imputed dataset 
tars_bm_complete<-read.csv('imputed_bm.csv') 
fitnessdf<-read.csv('lifetimefitness.csv')
fitnessdf<-fitnessdf[,-c(1)]
tars_bm_complete<-tars_bm_complete[,-c(1)]


#there is no age 0 because fledgling measurements removed. 

# tars_bm<-filter(tars_bm_complete, !is.na(tars_bm_complete$RightTarsus))
View(fitnessdf)

tars_bm_complete<-left_join(tars_bm_complete, fitnessdf, by='BirdID')


tars_bm_complete$age_year[tars_bm_complete$age_year>15]<-15
tars_bm_complete$w1[is.na(tars_bm_complete$w1)]<-0 #assign 0 to birds with NA fitness 


tarsbm_byiter<-split(tars_bm_complete, tars_bm_complete$.imp)


tarsbm_byiter[[1]]%>%
  group_by(age_year)%>%
  summarize(count=n())


iterbyage<-lapply(tarsbm_byiter,function(x){split(x,x$age_year)})

noimpu<-iterbyage[[1]]
tarsiter1<-iterbyage[[2]]
tarsiter2<-iterbyage[[3]]
tarsiter3<-iterbyage[[4]]
tarsiter4<-iterbyage[[5]]
tarsiter5<-iterbyage[[6]]


#scale right tarsus per df 
#could do in function but do i really want to? 
#probably fken not
tarsiter1<-lapply(tarsiter1, function(x){x<-x%>%mutate(RightTarsus_z = scale(RightTarsus))})
tarsiter2<-lapply(tarsiter2, function(x){x<-x%>%mutate(RightTarsus_z = scale(RightTarsus))})
tarsiter3<-lapply(tarsiter3, function(x){x<-x%>%mutate(RightTarsus_z = scale(RightTarsus))})
tarsiter4<-lapply(tarsiter4, function(x){x<-x%>%mutate(RightTarsus_z = scale(RightTarsus))})
tarsiter5<-lapply(tarsiter5, function(x){x<-x%>%mutate(RightTarsus_z = scale(RightTarsus))})


#scale relative fit 
tarsiter1<-lapply(tarsiter1, function(x){x<-x%>%mutate(w1_z = scale(w1))})
tarsiter2<-lapply(tarsiter2, function(x){x<-x%>%mutate(w1_z = scale(w1))})
tarsiter3<-lapply(tarsiter3, function(x){x<-x%>%mutate(w1_z = scale(w1))})
tarsiter4<-lapply(tarsiter4, function(x){x<-x%>%mutate(w1_z = scale(w1))})
tarsiter5<-lapply(tarsiter5, function(x){x<-x%>%mutate(w1_z = scale(w1))})



# summary(tarsiter1[[1]]$w1)

#get estimate
tarslm_getest<-function(x){
  model<-glmmTMB(w1 ~ RightTarsus, data=x, family=ziGamma(), ziformula = ~. , control = glmmTMBControl(optimizer = optim , optArgs = list(method='BFGS')))
  return(summary(model)$coefficient$cond[2,c(1,4)])
}

#get model object to run Dharma test 
tarslm_getmod<-function(x){
  model<-glmmTMB(w1~RightTarsus, data=x, family=ziGamma(), ziformula = ~. , control = glmmTMBControl(optimizer = optim , optArgs = list(method='BFGS')))
  return(model)
}


 # m1<-glmmTMB(w1~RightTarsus, data=tarsiter1[[1]], family=ziGamma(), ziformula=~1, control = glmmTMBControl(optimizer = optim , optArgs = list(method='BFGS')))
 # summary(m1)$coefficient$cond[2,c(1,4)]
library(DHARMa)
# simulateResiduals(m1, plot = T)

#iter 1 #iter 1 gaussian()
tars1_est<-lapply(tarsiter1, tarslm_getest)  
tars1_mods<-lapply(tarsiter1, tarslm_getmod)

#iter 2
tars2_est<-lapply(tarsiter2, tarslm_getest)  
tars2_mods<-lapply(tarsiter2, tarslm_getmod)

#iter 3
tars3_est<-lapply(tarsiter3, tarslm_getest)  
tars3_mods<-lapply(tarsiter3, tarslm_getmod)

View(tars3_est)
#iter 4 
tars4_est<-lapply(tarsiter4, tarslm_getest)  
tars4_mods<-lapply(tarsiter4, tarslm_getmod)

#iter 5 
tars5_est<-lapply(tarsiter5, tarslm_getest)  
tars5_mods<-lapply(tarsiter5, tarslm_getmod)

# View(tars2_est)





#make list of same age per iteration 
#goddamn this is so annoying 
#fucking hell 

# tarsyr0<-list(tars1_mods[[1]],tars2_mods[[1]],tars3_mods[[1]],tars4_mods[[1]], tars5_mods[[1]])


tarsmodelperage<-lapply(c(1:15), function(x){
  tars<-list(
    tars1_mods[[x]], 
    tars2_mods[[x]], 
    tars3_mods[[x]], 
    tars4_mods[[x]], 
    tars5_mods[[x]]
  )
  return(tars)
}
)

View(tarsmodelperage)

tarsestperage<-lapply(c(1:15), function(x){
  tarsest<-list(
    tars1_est[[x]], 
    tars2_est[[x]], 
    tars3_est[[x]], 
    tars4_est[[x]], 
    tars5_est[[x]]
  )
  tarsest<-do.call(rbind.data.frame, tarsest)
  colnames(tarsest)<-c('Estimate','p')
  return(tarsest)
}
)

# View(tarsestperage[[1]])

# fit1<-with(imp, lm(BodyMass~RightTarsus))
# summary(fit1)

source('modelchecker code.R')

dfage1<-modelchecker(tarsmodelperage[[1]])



modelcheckperage<-lapply(tarsmodelperage ,function(x){modelchecker(x)})

View(modelcheckperage[[2]])


# View(tarsmodelperage[[1]])
#use model averaging in mumin to combine estimates 

modelavgperage<-lapply(tarsmodelperage, function(x){model.avg(x)})


tarscoeffperage<-lapply(modelavgperage, 
                        function(x){modsum<-summary(x)
                        one<-as.data.frame(as_tibble_row(modsum$coefmat.full["cond(RightTarsus)",]))
                        return(one)})


tarscoeffperagedf<-do.call(rbind.data.frame, tarscoeffperage)
tarscoeffperagedf$age<-as.numeric(rownames(tarscoeffperagedf))

write.csv(tarscoeffperagedf, 'tarscoeffperagedf.csv')
tarscoeffperagedf$ymin<-tarscoeffperagedf$Estimate-1.96*tarscoeffperagedf$`Std. Error`
tarscoeffperagedf$ymax<-tarscoeffperagedf$Estimate+1.96*tarscoeffperagedf$`Std. Error`

tarsselection<-ggplot(tarscoeffperagedf, aes(x=age, y=Estimate))+
  stat_smooth(method='lm')+geom_point()+
  geom_errorbar(aes(ymin = (Estimate-`Std. Error`), ymax=(Estimate+`Std. Error`)), alpha=0.3)

tarsselection

thing<-summary(model.avg(tarsmodelperage[[1]]))
b<-as.data.frame(as_tibble_row(thing$coefmat.full["cond(RightTarsus)",]))


# tarsmodelperage<-lapply(tarsmodelperage, function(x){as.mira(x)})

mice::pool(tarsmodelperage[[1]])

#can do as.mira(), make list of model fits 
#as.mira(list of model fits)
#pool(mira object)

#######################################################

#bodymass

#imputed df is same as tarsus dataframe 

noimpu<-iterbyage[[1]]
bmiter1<-iterbyage[[2]]
bmiter2<-iterbyage[[3]]
bmiter3<-iterbyage[[4]]
bmiter4<-iterbyage[[5]]
bmiter5<-iterbyage[[6]]

#scale bm 
bmiter1<-lapply(bmiter1, function(x){x<-x%>%mutate(BodyMass_z = scale(BodyMass))})
bmiter2<-lapply(bmiter2, function(x){x<-x%>%mutate(BodyMass_z = scale(BodyMass))})
bmiter3<-lapply(bmiter3, function(x){x<-x%>%mutate(BodyMass_z = scale(BodyMass))})
bmiter4<-lapply(bmiter4, function(x){x<-x%>%mutate(BodyMass_z = scale(BodyMass))})
bmiter5<-lapply(bmiter5, function(x){x<-x%>%mutate(BodyMass_z = scale(BodyMass))})


#scale relative fit 
bmiter1<-lapply(bmiter1, function(x){x<-x%>%mutate(w1_z = scale(w1))})
bmiter2<-lapply(bmiter2, function(x){x<-x%>%mutate(w1_z = scale(w1))})
bmiter3<-lapply(bmiter3, function(x){x<-x%>%mutate(w1_z = scale(w1))})
bmiter4<-lapply(bmiter4, function(x){x<-x%>%mutate(w1_z = scale(w1))})
bmiter5<-lapply(bmiter5, function(x){x<-x%>%mutate(w1_z = scale(w1))})



#get gau estimate
bmest_gaus<-function(x){
  model<-glmmTMB(w1_z ~ BodyMass_z, data=x, family=gaussian(), ziformula = ~1)
  return(summary(model)$coefficient$cond[2,c(1,4)])
}

#get model object to run Dharma test 
bmmod_gaus<-function(x){
  model<-glmmTMB(w1_z ~BodyMass_z, data=x, family=gaussian(), ziformula = ~1)
  return(model)
}




#get zigamma estimate
bmest_zi<-function(x){
  model<-glmmTMB(w1 ~ BodyMass, data=x, family=ziGamma(), ziformula = ~. , control = glmmTMBControl(optimizer = optim , optArgs = list(method='BFGS')))
  return(summary(model)$coefficient$cond[2,c(1,4)])
}


#get zigamma m0d 
bmmod_zi<-function(x){
  model<-glmmTMB(w1 ~ BodyMass, data=x, family=ziGamma(), ziformula = ~. , control = glmmTMBControl(optimizer = optim , optArgs = list(method='BFGS')))
  return(model)
}


#iter 1 #iter 1 gaussian()
bm1est_gaus<-lapply(bmiter1, bmest_gaus)  
bm1mod_gaus<-lapply(bmiter1, bmmod_gaus)

#iter bm
bm2est_gaus<-lapply(bmiter2, bmest_gaus)  
bm2mod_gaus<-lapply(bmiter2, bmmod_gaus)

#iter 3
bm3est_gaus<-lapply(bmiter3, bmest_gaus)  
bm3mod_gaus<-lapply(bmiter3, bmmod_gaus)

# View(tars3_est)
#iter 4 
bm4est_gaus<-lapply(bmiter4, bmest_gaus)  
bm4mod_gaus<-lapply(bmiter4, bmmod_gaus)

#iter 5 
bm5est_gaus<-lapply(bmiter5, bmest_gaus)  
bm5mod_gaus<-lapply(bmiter5, bmmod_gaus)


bmestperage_gau<-lapply(c(1:15), function(x){
  bmest_gau<-list(
    bm1est_gaus[[x]], 
    bm2est_gaus[[x]], 
    bm3est_gaus[[x]], 
    bm4est_gaus[[x]], 
    bm5est_gaus[[x]]
  )
  bmest_gau<-do.call(rbind.data.frame, bmest_gau)
  colnames(bmest_gau)<-c('Estimate','p')
  return(bmest_gau)
}
)

bmmodperage_gau<-lapply(c(1:15), function(x){
  bm<-list(
    bm1mod_gaus[[x]], 
    bm2mod_gaus[[x]], 
    bm3mod_gaus[[x]], 
    bm4mod_gaus[[x]], 
    bm5mod_gaus[[x]]
  )
  return(bm)
}
)

View(tarsmodelperage)




####### zigamma ############

bm1est_zi<-lapply(bmiter1, bmest_zi)  
bm1mod_zi<-lapply(bmiter1, bmmod_zi)

#iter bm
bm2est_zi<-lapply(bmiter2, bmest_zi)  
bm2mod_zi<-lapply(bmiter2, bmmod_zi)

#iter 3
bm3est_zi<-lapply(bmiter3, bmest_zi)  
bm3mod_zi<-lapply(bmiter3, bmmod_zi)


#iter 4 
bm4est_zi<-lapply(bmiter4, bmest_zi)  
bm4mod_zi<-lapply(bmiter4, bmmod_zi)

#iter 5 
bm5est_zi<-lapply(bmiter5, bmest_zi)  
bm5mod_zi<-lapply(bmiter5, bmmod_zi)

#############################################

#model checker



