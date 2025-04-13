#print(the_frame, n=100)


#set bin size as threshold
threshold <- 25
result <- data.frame()
i <- 1
while (i <= nrow(the_frame)){
  current_sum <- 0
  rows_to_add <- list()
  vObs <- vector()
  vSppCd <- vector()
  vDc <- vector()
  vObsThis <- vector()
  vObsPres <- vector()
  
  while (current_sum < threshold && i <= nrow(the_frame)) {
    current_sum <- current_sum + sum(the_frame[i,c("n")])
    
    vObs <- append(vObs, current_sum)
    vSppCd <- append(vSppCd, the_frame[i,1])
    vDc <- append(vDc, the_frame[i,2])
    vObsThis <- append(vObsThis, the_frame[i,3])
    vObsPres <- append(vObsPres, the_frame[i,4])
    
    #debug code
    #print(vObs)
    #print(vSppCd)
    
    result_frame <- data.frame(unlist(vSppCd), vObs, min(unlist(vDc)), max(unlist(vDc)), max(unlist(vObsPres)))
    
    #debug code
    #print(result_frame)
    #if(result_frame[1,3] == result_frame[1,4]){
    #  print("true")
    
    #set the current row's max_dc to the next row's min_dc
    if(!is.na(the_frame[i+1, 2]))
    {
      result_frame[1,4] = the_frame[i+1,2]
    } else 
    {
      result_frame[1,4] = the_frame[i,2]
    }
    
    #debug code
    #print(result_frame)
    
    rows_to_add <- append(rows_to_add, result_frame)
    i <- i + 1
  }
  
  #debug code  
  #print(result_frame)
  
  intermed_result_frame <- data.frame(result_frame[1,1], max(result_frame[5]), max(result_frame[2]), min(result_frame[3]), max(result_frame[4]))
  
  #debug code
  #print(intermed_result_frame)
  
  result <- rbind(result, intermed_result_frame)
}

#If the last row is < the threshold, add it to the preceding row, and remove the last
if(nrow(result) > 1 && result[nrow(result), 3] < threshold){
  result[nrow(result)-1, 3] <- result[nrow(result)-1, 3] + result[nrow(result), 3]
  result[nrow(result)-1, 5] <- result[nrow(result), 5]
  result <- result %>% filter(!row_number() %in% nrow(result))
}

#Make things prettier
colnames(result) <- c("species_cd", "pres", "obs", "min_dc", "max_dc")

#Add a final midpoint column that is the mid between min_dc and max_dc
result <-
  result %>%
  mutate(midpoint = (min_dc + max_dc)/2)

#have to have at least 1 positive and 1 negative observation for logistic
#iterate subsequent bins until true
#function <- AdjustForLogistic(){

#}



result$species_cd = str_replace_all(result$species_cd, "__", " ")

#debug code
result[[2]][4]
print(result[1])
print(nrow(the_frame))
print(the_frame$n)
print(result)

#cleanup stuff
rm(ana_frame)
rm(baseframe)
rm(list_of_df)
rm(list_of_df.balcapr)
rm(something)
ls()
rm(list=ls())
rm(the_frame)



