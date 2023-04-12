## code to prepare `fishToxicity` dataset goes here

library(readxl)
dd <- read_excel("qsar_fish_toxicity.xlsx")
names(dd)<-c("CIC0", "SM1_Dz_Z", "GATS1i","NdsCH", "NdssC", "MLOGP", "LC50")
dd <- dd[1:100,]
fishToxicity <- dd
fishToxicity <- fishToxicity[c("LC50","CIC0", "SM1_Dz_Z", "GATS1i","NdsCH", 
                               "NdssC", "MLOGP")]

fishToxicity <-data.frame(fishToxicity)

usethis::use_data(fishToxicity, overwrite = TRUE)


