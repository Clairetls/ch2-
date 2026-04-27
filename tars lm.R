#tars and bm? 

library(tidyverse)
library(mice)

#read imputed dataset 
tars_bm_complete<-read.csv('imputed_bm.csv') 
tars_bm_10iter<-read.csv('imputed_bm_10.csv')
fitnessdf<-read.csv('lifetimefitness.csv')
fitnessdf<-fitnessdf[,-c(1)]
tars_bm_complete<-tars_bm_complete[,-c(1)]
tars_bm_10iter<-tars_bm_10iter[,-c(1)]


#there is no age 0 because fledgling measurements removed. 

# tars_bm<-filter(tars_bm_complete, !is.na(tars_bm_complete$RightTarsus))
View(fitnessdf)

tars_bm_complete<-left_join(tars_bm_complete, fitnessdf, by='BirdID')
tars_bm_10iter<-left_join(tars_bm_10iter, fitnessdf, by='BirdID')



tars_bm_complete$age_year[tars_bm_complete$age_year>15]<-15
tars_bm_complete$w1[is.na(tars_bm_complete$w1)]<-0 #assign 0 to birds with NA fitness 


tars_bm_10iter$age_year[tars_bm_10iter$age_year>15]<-15
tars_bm_10iter$w1[is.na(tars_bm_10iter$w1)]<-0 #assign 0 to birds with NA fitness 

tarsbm_byiter<-split(tars_bm_complete, tars_bm_complete$.imp)
tarsbm10_byiter<-split(tars_bm_10iter, tars_bm_10iter$.imp)

# tarsbm_byiter[[1]]%>%
#   group_by(age_year)%>%
#   summarize(count=n())


iterbyage<-lapply(tarsbm_byiter,function(x){split(x,x$age_year)})

iter10byage<-lapply(tarsbm10_byiter,function(x){split(x,x$age_year)})

iter10byage<-iter10byage[-1]

# View(iterbyage)

noimpu<-iterbyage[[1]]
tarsiter1<-iterbyage[[2]]
tarsiter2<-iterbyage[[3]]
tarsiter3<-iterbyage[[4]]
tarsiter4<-iterbyage[[5]]
tarsiter5<-iterbyage[[6]]




tars10iter1<-iter10byage[[2]]
tars10iter2<-iter10byage[[3]]
tars10iter3<-iter10byage[[4]]
tars10iter4<-iter10byage[[5]]
tars10iter5<-iter10byage[[6]]
tars10iter6<-iter10byage[[7]]
tars10iter7<-iter10byage[[8]]
tars10iter8<-iter10byage[[9]]
tars10iter9<-iter10byage[[10]]
tars10iter10<-iter10byage[[11]]




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











#maybe try both gaussian and gamma and compare? 

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


#list per iterations of models per age 
tarsrep10mods<-lapply(iter10byage, function(x){lapply(x, tarslm_getmod)})

tarsrep10est<-lapply(iter10byage, function(x){lapply(x, tarslm_getest)})


View(tarsrep10mods)
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

#iter 4 
tars4_est<-lapply(tarsiter4, tarslm_getest)  
tars4_mods<-lapply(tarsiter4, tarslm_getmod)

#iter 5 
tars5_est<-lapply(tarsiter5, tarslm_getest)  
tars5_mods<-lapply(tarsiter5, tarslm_getmod)

# View(tars2_est)



#make list of same age per iteration 


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



# for(i in 1:15){
#   oneagemods<-list()
#   for(n in 1:10){
#     onemod<-tarsrep10mods[[n]][[i]]
#     oneagemods[n]<-onemod
#   }
#   tarsperagemods10[i]<-oneagemods
# }



tarsmod10_byage<-purrr::transpose(tarsrep10mods)

#for i in list1-10
#take the first item of all 

# View(tarsmodelperage)

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

# dfage1<-modelchecker(tarsmodelperage[[1]])



modelcheckperage<-lapply(tarsmodelperage ,function(x){modelchecker(x)})

View(modelcheckperage[[2]])

modelcheck_tars10<-lapply(tarsmod10_byage, function(x){modelchecker(x)})

View(modelcheck_tars10[[1]])
# modelchecker_10iter<-lapply()
  


# View(tarsmodelperage[[1]])
#use model averaging in mumin to combine estimates 

modelavgperage<-lapply(tarsmodelperage, function(x){model.avg(x)})

tarsmodavgperage_10<-lapply(tarsmod10_byage, function(x){model.avg(x)})


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

#################
#10 iters 
tars10_coeffperage<-lapply(tarsmodavgperage_10, 
                        function(x){modsum<-summary(x)
                        one<-as.data.frame(as_tibble_row(modsum$coefmat.full["cond(RightTarsus)",]))
                        return(one)})


tars10coeffperagedf<-do.call(rbind.data.frame, tars10_coeffperage)
tars10coeffperagedf$age<-as.numeric(rownames(tars10coeffperagedf))
tars10coeffperagedf<-tars10coeffperagedf%>%
  mutate(pval_lab= case_when(`Pr(>|z|)`<0.05 ~ '**', 
                             `Pr(>|z|)`>0.05 ~ ''))

write.csv(tars10coeffperagedf, 'tars10coeffperagedf.csv')
tars10coeffperagedf$ymin<-tars10coeffperagedf$Estimate-1.96*tars10coeffperagedf$`Std. Error`
tars10coeffperagedf$ymax<-tars10coeffperagedf$Estimate+1.96*tars10coeffperagedf$`Std. Error`

tarsselection_10iter<-ggplot(tars10coeffperagedf, aes(x=age, y=Estimate))+
  stat_smooth(method='lm')+geom_point()+  geom_text(aes(label=pval_lab), vjust= -10)+
  geom_errorbar(aes(ymin = (Estimate-`Std. Error`), ymax=(Estimate+`Std. Error`)), alpha=0.3)+
  theme_cowplot()+xlab('Age (years)')

tarsselection_10iter

# thing<-summary(model.avg(tarsmodelperage[[1]]))
# b<-as.data.frame(as_tibble_row(thing$coefmat.full["cond(RightTarsus)",]))


# tarsmodelperage<-lapply(tarsmodelperage, function(x){as.mira(x)})

# mice::pool(tarsmodelperage[[1]])

#can do as.mira(), make list of model fits 
#as.mira(list of model fits)
#pool(mira object)

write_rds(tarsmodelperage, 'tarsmodperage.rds')
write_rds(tarsmodavgperage_10, 'tarsmodavgperage_10.rds')

#######################################################

#bodymass

#imputed df is same as tarsus dataframe 

noimpu<-iterbyage[[1]]
bmiter1<-iterbyage[[2]]
bmiter2<-iterbyage[[3]]
bmiter3<-iterbyage[[4]]
bmiter4<-iterbyage[[5]]
bmiter5<-iterbyage[[6]]
#####################




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





# tarsrep10est<-lapply(iter10byage, function(x){lapply(x, tarslm_getest)})




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



#10iterations
bm_rep10mods<-lapply(iter10byage, function(x){lapply(x, bmmod_zi)})



#compile estimates
bmestperage_zi<-lapply(c(1:15), function(x){
  bmest_zi<-list(
    bm1est_zi[[x]], 
    bm2est_zi[[x]], 
    bm3est_zi[[x]], 
    bm4est_zi[[x]], 
    bm5est_zi[[x]]
  )
  bmest_zi<-do.call(rbind.data.frame, bmest_zi)
  colnames(bmest_zi)<-c('Estimate','p')
  return(bmest_zi)
}
)

#compile models 
bmmodperage_zi<-lapply(c(1:15), function(x){
  bmmod_zi<-list(
    bm1mod_zi[[x]], 
    bm2mod_zi[[x]], 
    bm3mod_zi[[x]], 
    bm4mod_zi[[x]], 
    bm5mod_zi[[x]]
  )
  return(bmmod_zi)
}
)

bm10_modbyage<-purrr::transpose(bm_rep10mods)
View(bm10_modbyage)


#############################################

#model checker

bmcheckperage_gau<-lapply(bmmodperage_gau ,function(x){modelchecker(x)})


bmcheckperage_zi<-lapply(bmmodperage_zi, function(x){modelchecker(x)})


bmcheckperage_10<-lapply(bm10_modbyage, function(x){modelchecker(x)})

View(bmcheckperage_10[[1]])

bmcheckgau_df<-do.call(rbind.data.frame, bmcheckperage_gau)
bmcheckzi_df<-do.call(rbind.data.frame, bmcheckperage_zi)

####
#modelavg

bmavgperage_gau<-lapply(bmmodperage_gau, function(x){model.avg(x)})
bmavgperage_zi<-lapply(bmmodperage_zi, function(x){model.avg(x)})   #i think the zigamma performs better here. 


# View(bmcheckgau_df)


# View(bmcheckzi_df)

bmestavgperage_gau<-lapply(bmavgperage_gau, 
                        function(x){modsum<-summary(x)
                        one<-as.data.frame(as_tibble_row(modsum$coefmat.full["cond(BodyMass_z)",]))
                        return(one)})


bmestavgperage_gau_df<-do.call(rbind.data.frame, bmestavgperage_gau)
bmestavgperage_gau_df$age<-as.numeric(rownames(bmestavgperage_gau_df))

View(bmestavgperage_gau_df)

# tarscoeffperagedf$ymin<-tarscoeffperagedf$Estimate-1.96*tarscoeffperagedf$`Std. Error`
# tarscoeffperagedf$ymax<-tarscoeffperagedf$Estimate+1.96*tarscoeffperagedf$`Std. Error`

bmselection_gau<-ggplot(bmestavgperage_gau_df, aes(x=age, y=Estimate))+
  stat_smooth(method='lm')+geom_point()+
  geom_errorbar(aes(ymin = (Estimate-`Std. Error`), ymax=(Estimate+`Std. Error`)), alpha=0.3)+xlab('Age (years)')+theme_cowplot()

bmselection_gau

#############################
#zi



bmestavgperage_zi<-lapply(bmavgperage_zi, 
                           function(x){modsum<-summary(x)
                           one<-as.data.frame(as_tibble_row(modsum$coefmat.full["cond(BodyMass)",]))
                           return(one)})


bmestavgperage_zi_df<-do.call(rbind.data.frame, bmestavgperage_zi)
bmestavgperage_zi_df$age<-as.numeric(rownames(bmestavgperage_zi_df))

View(bmestavgperage_zi_df)

# tarscoeffperagedf$ymin<-tarscoeffperagedf$Estimate-1.96*tarscoeffperagedf$`Std. Error`
# tarscoeffperagedf$ymax<-tarscoeffperagedf$Estimate+1.96*tarscoeffperagedf$`Std. Error`

bmselection_zi<-ggplot(bmestavgperage_zi_df, aes(x=age, y=Estimate))+
  stat_smooth(method='lm')+geom_point()+
  geom_errorbar(aes(ymin = (Estimate-`Std. Error`), ymax=(Estimate+`Std. Error`)), alpha=0.3)+xlab('Age (years)') +theme_cowplot()

bmselection_zi

###############################
bm10_avgperage<-lapply(bm10_modbyage, function(x){model.avg(x)})

bm10avgperage<-lapply(bm10_avgperage, 
                          function(x){modsum<-summary(x)
                          one<-as.data.frame(as_tibble_row(modsum$coefmat.full["cond(BodyMass)",]))
                          return(one)})


bm10avgperagedf<-do.call(rbind.data.frame, bm10avgperage)
bm10avgperagedf$age<-as.numeric(rownames(bm10avgperagedf))

View(bmestavgperage_zi_df)

# tarscoeffperagedf$ymin<-tarscoeffperagedf$Estimate-1.96*tarscoeffperagedf$`Std. Error`
# tarscoeffperagedf$ymax<-tarscoeffperagedf$Estimate+1.96*tarscoeffperagedf$`Std. Error`

bmselection_zi<-ggplot(bmestavgperage_zi_df, aes(x=age, y=Estimate))+
  stat_smooth(method='lm')+geom_point()+
  geom_errorbar(aes(ymin = (Estimate-`Std. Error`), ymax=(Estimate+`Std. Error`)), alpha=0.3)+xlab('Age (years)') +theme_cowplot()

bmselection_zi


bm10avgperagedf<-bm10avgperagedf%>%
  mutate(pval_lab=case_when(`Pr(>|z|)`>0.05~'',
                            `Pr(>|z|)`<0.05~'**'))

bm10selection<-ggplot(bm10avgperagedf, aes(x=age, y=Estimate))+
  stat_smooth(method='lm')+geom_point()+geom_text(aes(label=pval_lab), vjust=-10)+
  geom_errorbar(aes(ymin = (Estimate-`Std. Error`), ymax=(Estimate+`Std. Error`)), alpha=0.3)+xlab('Age (years)') +theme_cowplot()

bm10selection

#############################################




