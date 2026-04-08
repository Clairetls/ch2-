

# check<-tarsmodelperage[[1]]
# 
# i<-1

modelchecker <- function(check) {
  require(DHARMa)
  require(tidyverse)
  require(hablar)
  require(MuMIn)
  require(magrittr)
  test <- matrix(ncol = 14, nrow = length(check))
  pb <- txtProgressBar(0, length(check), style = 3)
  
  for (i in 1:length(check)){
    
    setTxtProgressBar(pb, i)
    
    s1b <- try(simulateResiduals(check[[i]]), silent = T)
    if ('try-error' %in% class(s1b)) next
    
    zi <- testZeroInflation(s1b)
    zitest <- zi$statistic
    zip <- zi$p.value
    
    di <- testDispersion(s1b)
    ditest <- di$statistic
    dip <- di$p.value
    
    uni <- testUniformity(s1b)
    unitest <- uni$statistic
    unip <- uni$p.value
    
    com<-AIC(check[[i]])
    
    we<-model.sel(check[[i]])
    
    test[i, 1:14] <- c(i, paste(check[[i]]$modelInfo$family$family), paste(check[[i]]$modelInfo$allForm$formula[3]), paste(check[[i]]$modelInfo$allForm$ziformula[2]), zitest, zip, ditest, dip, unitest, unip, com, paste(check[[i]]$fit$convergence), paste(summary(check[[i]]$sdr)[1,2]), paste(we$df))
    Sys.sleep(time = 1)
  }
  
  we<-model.sel(check[[i]])
  we$df
  test <- as.data.frame(test)
  names(test) <- c("ModelNo", "Family", "Formula", "ziFormula", "zi", "zip", "di", "dip", "uni", "unip", "AIC", "Converge?-1", "Converge?-2", "df")
  
  test %<>%
    convert(num(zi, zip, di, dip, uni, unip, AIC))
  
  final <- test %>%
    filter(zi <= 1.10) %>%
    filter(di <= 1.10) %>%
    filter(zi >= 0.80) %>%
    filter(di >= 0.80) %>%
    filter(AIC != "NA") %>%
    filter(`Converge?-1` == "relative convergence (4)") %>%
    filter(`Converge?-2` != "NaN") %>%
    arrange(AIC)
  
  # View(test)
  # View(final)
  write.csv(final, "final.models.csv", row.names = F)
  write.csv(test, "all.models.csv", row.names = F)
  return(test)
  return(final)
  
}
