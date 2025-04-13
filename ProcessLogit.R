library(tidyverse)
#library(plyr)

#Read in the data
#windows
#baseframe <- read.csv("c:\\work\\R\\BinGenerator\\Data\\pr_usvi1623_fish18sppLH_depregdat.csv", header = TRUE)
#mac
logit_frame <- read.csv("../test_logit_data.csv", header = TRUE)

result <- data.frame()
i<-1
current_sum <- 0
rows_to_add <- list()
vSppCd <- vector()
vObs <- vector()
vMinDc <- vector()
vMaxDc <- vector()
vPosObs <- vector()
vNegObs <- vector()

#this loop takes care of everything except potentially the last row
while(i<=nrow(logit_frame)){
  #print(current_sum)
  while(current_sum < 1 && i<=nrow(logit_frame)){
    current_sum <- current_sum + sum(logit_frame[i, c("pos_obs")])
    vSppCd <- append(vSppCd, logit_frame[i,c("species_cd")])
    vObs <- append(vObs, logit_frame[i,c("obs")])
    vMinDc <- append(vMinDc, logit_frame[i, c("min_dc")])
    vMaxDc <- append(vMaxDc, logit_frame[i, c("max_dc")])
    vPosObs <- append(vPosObs, logit_frame[i, c("pos_obs")])
    vNegObs <- append(vNegObs, logit_frame[i, c("neg_obs")])
    
    intermed_result_frame <- data.frame(vSppCd, vObs, vMinDc, vMaxDc, vPosObs, vNegObs)
    i<-i+1    
    print(paste("inner:", current_sum))
  }
  result_frame <- intermed_result_frame %>% group_by(vSppCd) %>% summarise(sum(vObs), min(vMinDc), max(vMaxDc), sum(vPosObs), sum(vNegObs))
  result <- rbind(result, result_frame)
  #clear all the variables for next loop
  intermed_result_frame <- intermed_result_frame[0,]
  result_frame <- result_frame[0,]
  vSppCd <- vector()
  vObs <- vector()
  vMinDc <- vector()
  vMaxDc <- vector()
  vPosObs <- vector()
  vNegObs <- vector()
  current_sum <- 0
}

colnames(result) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")

#handle the last row case
if(result[nrow(result), c("pos_obs")] == 0){
  result[nrow(result)-1, c("obs")] <- result[nrow(result)-1, c("obs")] + result[nrow(result), c("obs")]
  result[nrow(result)-1, c("max_dc")] <- result[nrow(result), c("max_dc")]
  result[nrow(result)-1, c("pos_obs")] <- result[nrow(result)-1, c("pos_obs")]
  result[nrow(result)-1, c("neg_obs")] <- result[nrow(result)-1, c("neg_obs")] + result[nrow(result), c("neg_obs")]
  result <- result %>% filter(!row_number() %in% nrow(result))
}