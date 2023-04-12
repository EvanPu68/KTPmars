## code to prepare `concrete` dataset goes here

library(readxl)
dd <- read_excel("Concrete_Data.xls")
names(dd)<-c("Cement", "BlastFurnaceSlag", "FlyAsh", "Water", 
             "Superplasticizer", "CoarseAggregate", "FineAggregate",
             "Age","ConcreteCompressiveStrength")
dd <- dd[1:100,]
concrete <- dd
concrete <- concrete[c("ConcreteCompressiveStrength","Cement", 
                       "BlastFurnaceSlag", "FlyAsh", "Water", 
                       "Superplasticizer", "CoarseAggregate", "FineAggregate",
                       "Age")]
concrete<-data.frame(concrete)

usethis::use_data(concrete, overwrite = TRUE)


