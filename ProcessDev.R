#David Graham 3/26/25
#R version 4.4.3
#Tidyverse 2.0.0 (dplyr 1.1.4)
library(tidyverse)
#library(plyr)

#Read in the data
#windows
#baseframe <- read.csv("c:\\work\\R\\BinGenerator\\Data\\pr_usvi1623_fish18sppLH_depregdat.csv", header = TRUE)
#mac
baseframe <- read.csv("../pr_usvi1623_fish18sppLH_depregdat.csv", header = TRUE)

#baseframe
#summary(baseframe)

#create a new variable for depth category (DC) in a new dataframe
#yeah, I know we don't need a new dataframe but I like to be able to debug easily
ana_frame <-
  baseframe %>%
  mutate(dc = floor(DEPTH))

ana_frame <- ana_frame %>% mutate(species_cd = str_replace_all(species_cd, " ", "__"))

#Create new, currently empty columns for the min, max, and midpoint depth categories
ana_frame <- ana_frame %>%
  add_column("dc_min"=0, "dc_max"=0, "dc_midpoint"=0)

#vector of spp codes
distinct_spp <- ana_frame %>% distinct(species_cd) %>% pull()

result <- data.frame()
final_result <- data.frame()
the_frame <- data.frame()
#get all rows with the species code and create a new summarised df on which to operate
spp_extractor <- function(spp_code){
  filter(ana_frame, species_cd == spp_code) %>%
    group_by(species_cd, dc) %>%
    summarise(n=n(), 
              obs=max(pres), 
              pres_true = sum(ifelse(pres==1, 1, 0)), 
              pres_false = sum(ifelse(pres==0, 1, 0)))
}

process_spp <- function(processing_frame, threshold){
  i <- 1
  while (i <= nrow(processing_frame)){
    current_sum <- 0
    #number of present observations for logistic
    pos_sum <- 0
    #number of not-present observations for logistic
    neg_sum <- 0
    rows_to_add <- list()
    vObs <- vector()
    vPos <- vector()
    vNeg <- vector()
    vSppCd <- vector()
    vDc <- vector()
    
    while (current_sum < threshold && i <= nrow(processing_frame)) {
      #does this have a first row error and always add the first row to the second?
      current_sum <- current_sum + sum(processing_frame[i,c("n")])
      pos_sum <- pos_sum + sum(processing_frame[i, c("pres_true")])
      neg_sum <- neg_sum + sum(processing_frame[i, c("pres_false")])
      
      vObs <- append(vObs, current_sum)
      vPos <- append(vPos, pos_sum)
      vNeg <- append(vNeg, neg_sum)
      vSppCd <- append(vSppCd, processing_frame[i,1])
      vDc <- append(vDc, processing_frame[i,2])
      
      #debug code
      #print(vObs)
      #print(vSppCd)
      
      result_frame <- data.frame(unlist(vSppCd), vObs, min(unlist(vDc)), max(unlist(vDc)), vPos, vNeg)
      
      #debug code
      #print(result_frame)
      #if(result_frame[1,3] == result_frame[1,4]){
      #  print("true")
      
      #set the current row's max_dc to the next row's min_dc
      if(!is.na(processing_frame[i+1, 2]))
      {
        result_frame[1,4] = processing_frame[i+1,2]
      } else 
      {
        result_frame[1,4] = processing_frame[i,2]
      }
      
      #debug code
      #print(result_frame)
      
      rows_to_add <- append(rows_to_add, result_frame)
      #print(rows_to_add)
      i <- i + 1
    }
    
    #debug code  
    #print("result")
    #print(result_frame)
    #print("end result")
    
    colnames(result_frame) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    intermed_result_frame <- result_frame %>% group_by(species_cd) %>% summarise(max(obs), min(min_dc), min(min_dc), max(max_dc), sum(pos_obs), sum(neg_obs))
    colnames(intermed_result_frame) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    #debug code
    #print(intermed_result_frame)
    
    result <- rbind(result, intermed_result_frame)
    #colnames(result) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    #print(result)
  }
  
  #If the last row is < the threshold, add it to the preceding row, and remove the last
  if(nrow(result) > 1 && result[nrow(result), 2] < threshold){
    result[nrow(result)-1, 2] <- result[nrow(result)-1, 2] + result[nrow(result), 2]
    result[nrow(result)-1, 4] <- result[nrow(result), 4]
    result[nrow(result)-1, 5] <- result[nrow(result)-1, 5] + result[nrow(result), 5]
    result[nrow(result)-1, 6] <- result[nrow(result)-1, 6] + result[nrow(result), 6]

    result <- result %>% filter(!row_number() %in% nrow(result))
  }
  
  #print(result)
  #return the result frame
  result
}

process_logit <- function(processing_frame){
    #go through result frame and add subsequent rows until pos_obs and neg_obs both >= 1
    #do pos_obs first
    #print(processing_frame)
  
    if(nrow(processing_frame) > 1){
      i <- 1
      #print(i) 
     #return(processing_frame)
      #print(nrow(processing_frame))
      while(i < nrow(processing_frame)){
         current_sum <- 0
         #i <- 1
         vObs <- vector()
         vPos <- vector()
         vNeg <- vector()
         vSppCd <- vector()
         vMinDc <- vector()
         vMaxDc <- vector()
         #print(i)
    # #     #print(current_sum)
       while(current_sum < 1 && i <= nrow(processing_frame)){
         current_sum <- current_sum + min(processing_frame[i,c("pos_obs")])
    #       #print(current_sum + min(processing_frame[i,c("pos_obs")]))
    #       print(paste("current_sum:", current_sum, " current_sum<1:", current_sum <1, " i:", i, " nrow:", nrow(processing_frame)))
    #       #print(current_sum)
    #       #print(str(current_sum))
    #       #print(i)
    # #       print (result[i,5])
          #pos_sum <- pos_sum + sum(processing_frame[i, c("pres_true")])
          #neg_sum <- neg_sum + sum(processing_frame[i, c("pres_false")])

          vObs <- append(vObs, result[i, c("obs")])
          vPos <- append(vPos, result[i, c("pos_obs")])
          vNeg <- append(vNeg, result[i, c("neg_obs")])
          vSppCd <- append(vSppCd, result[i,c("species_cd")])
          vMinDc <- append(vMinDc, result[i,c("min_dc")])
          vMaxDc <- append(vMaxDc, result[i, c("max_dc")])
          
          #result_frame <- data.frame(unlist(vSppCd), unlist(vObs), min(unlist(vMinDc)), max(unlist(vMaxDc)), sum(vPos), sum(vNeg))
          i<-i+1
          #print(paste("vObs:", vObs, "i:", i))
          #print(result[i, c("obs")])
       }
         
        #print(result_frame)
       #i <- i + 1
      }
      #print(i)
    }
    #do neg_obs
    
}

#for each element in distinct_spp call the function that does all the work and add the result to the final result df
if(length(distinct_spp) > 0){
  for(i in distinct_spp){
    #i == the species_cd
    #call the function
    the_frame <- spp_extractor(i)
    if(which(distinct_spp == i) > 1){
      final_result <- union_all(final_result, process_spp(the_frame, 25))
    }
    else {
      final_result <- process_spp(the_frame, 25)
    }
  }
  
  colnames(final_result) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
  
  logit <- TRUE
  #logistic portion
  #print(distinct_spp)
  if(logit == TRUE){
    if(length(distinct_spp > 0)){
      for(i in distinct_spp){
        #print(i)
        the_frame <-  filter(final_result, species_cd == i)
        as.data.frame(the_frame)
        #print(the_frame)
         if(which(distinct_spp == i) > 1){
           print("here")
           #print(the_frame)
           final_result <- union_all(final_result, process_logit(the_frame))
        #
         } else {
           print("there")
           #print(the_frame)
          final_result <- process_logit(the_frame)
        }
      }
    }
  }

  #print(final_result)
  #Add a final midpoint column that is the mid between min_dc and max_dc
  # final_result <-
  #   final_result %>%
  #   mutate(midpoint = (min_dc + max_dc)/2)
} #what to do if not true?


write.csv(the_frame, "../test_logit_data.csv")
