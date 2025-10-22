library(tidyverse)

baseframe <- read.csv("../rugosity/pr_usvi1623_fish17sppLH_ARdat2v2_rug_tst.csv", header = TRUE)
binresult_25 <- read.csv("../rugosity/rugosity_test_25.csv", header=TRUE)
binresult_40 <- read.csv("../rugosity/rugosity_test_40.csv", header=TRUE)
#ana_frame <- baseframe %>%

#the 25s
by <- join_by(species_cd, between(DEPTH, min_rc, max_rc, bounds="(]"))
check <- left_join(baseframe, binresult_25, by)

#the 40s
final <- left_join(check, binresult_40, by)

write.csv(final, "../rugosity/rugosity_test_joined_results.csv")

#the final dataframe, "final", will have new columns with .x and .y, the .x's are for the linear regression (25)
#and the y's are for the logistic regression (40)

#run generate the 25's and the 40's in ProcessDev_rug.R, save those to separate filenames (in this case it is rugosity_test_25.csv
#and rugosity_test_40.csv) then run this script.

