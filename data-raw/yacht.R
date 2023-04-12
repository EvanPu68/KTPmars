## code to prepare `yacht` dataset goes here

library(readxl)
dd <- read_excel("Data for stat 360 (yacht).xlsx")
names(dd)<-c("LPOTCOB", "PrismaticCoefficient", "LengthDisplacementRatio", 
             "BeamDraughtRatio", "LengthBeamRatio", "FroudeNumber", 
             "RRPUWOD")
dd <- dd[1:100,]
yacht <- dd
yacht <- yacht[c("RRPUWOD","LPOTCOB", "PrismaticCoefficient", 
                 "LengthDisplacementRatio", "BeamDraughtRatio", 
                 "LengthBeamRatio", "FroudeNumber")]
yacht <-data.frame(yacht)

usethis::use_data(yacht, overwrite = TRUE)
