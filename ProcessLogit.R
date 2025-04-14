library(tidyverse)
#library(plyr)

#Read in the data
#windows
#baseframe <- read.csv("c:\\work\\R\\BinGenerator\\Data\\pr_usvi1623_fish18sppLH_depregdat.csv", header = TRUE)
#mac
logit_frame <- read.csv("../test_logit_data.csv", header = TRUE)
logit_frame <- logit_frame[,-1]
result <- data.frame()
#the two columns on which to operate
#each one must be > 0 at the end of the day
vCols <- c("pos_obs", "neg_obs")

for(j in seq_along(vCols)){
  i<-1
  current_sum <- 0
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
      #current_sum <- current_sum + sum(logit_frame[i, c("pos_obs")])
      current_sum <- current_sum + sum(logit_frame[i, vCols[j]])
      vSppCd <- append(vSppCd, logit_frame[i,c("species_cd")])
      vObs <- append(vObs, logit_frame[i,c("obs")])
      vMinDc <- append(vMinDc, logit_frame[i, c("min_dc")])
      vMaxDc <- append(vMaxDc, logit_frame[i, c("max_dc")])
      vPosObs <- append(vPosObs, logit_frame[i, c("pos_obs")])
      vNegObs <- append(vNegObs, logit_frame[i, c("neg_obs")])
      
      intermed_result_frame <- data.frame(vSppCd, vObs, vMinDc, vMaxDc, vPosObs, vNegObs)
      i<-i+1    
      #print(paste("inner:", current_sum))
    }
    print(intermed_result_frame)
    result_frame <- intermed_result_frame %>% group_by(species_cd) %>% summarise(sum(obs), min(min_dc), max(max_dc), sum(pos_obs), sum(neg_obs))
    #colnames(result_frame) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    
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
  #print(j)
  #print(vCols[j])
  #handle the last row case
  #print(result)
  #print(result[nrow(result), vCols[j]])
  if(result[nrow(result), j] == 0){
    result[nrow(result)-1, c("obs")] <- result[nrow(result)-1, c("obs")] + result[nrow(result), c("obs")]
    result[nrow(result)-1, c("max_dc")] <- result[nrow(result), c("max_dc")]
    #result[nrow(result)-1, c("pos_obs")] <- result[nrow(result)-1, c("pos_obs")]
    result[nrow(result)-1, vCols[j]] <- result[nrow(result)-1, vCols[j]]
    #result[nrow(result)-1, c("neg_obs")] <- result[nrow(result)-1, c("neg_obs")] + result[nrow(result), c("neg_obs")]
    result[nrow(result)-1, vCols[vCols != vCols[j]]] <- result[nrow(result)-1, vCols[vCols != vCols[j]]] + result[nrow(result), vCols[vCols != vCols[j]]]
    result <- result %>% filter(!row_number() %in% nrow(result))
  }
  logit_frame<-result
}