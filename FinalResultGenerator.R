library(tidyverse)

baseframe <- read.csv("../pr_usvi1623_fish18sppLH_depregdat.csv", header = TRUE)
binresult_25 <- read.csv("../logit_test_data_25.csv", header=TRUE)
binresult_40 <- read.csv("../logit_test_data_40.csv", header=TRUE)
#ana_frame <- baseframe %>%
#  add_column("lr_dc"=0, "log_dc"=0)

#the 25s
by <- join_by(species_cd, between(DEPTH, min_dc, max_dc, bounds="(]"))
check <- left_join(baseframe, binresult_25, by)

#the 40s
final <- left_join(check, binresult_40, by)

write.csv(final, "whateveryouwantthenametobe.csv")

#the final dataframe, "final", will have new columns with .x and .y, the .x's are for the linear regression (25)
#and the y's are for the logistic regression (40)

#run generate the 25's and the 40's in ProcessDev.R, save those to separate filenames (in this case it is logit_test_data_25.csv
#and logit_test_data_40.csv) then run this script.