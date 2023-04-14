# This file consists of 3 examples to illustrate how to call mars() and
# five methods of class 'mars'.
# library(devtools) #uncomment to run
# load_all() #uncomment to run

#################################Example 1######################################
data(concrete)#read data concrete within the package KTPmars, use ?concrete
#                                                      for more information
mc <- mars.control(Mmax = 10)#create a 'mars.control' object, see help file
# ?mars.control for more information. Mmax is 10 which means we want to add
# 10 basis functions in forward selection and use them for backward selection.
# Unspecified d and trace parameters will set to their default values

# Create a mars object using mars(), formula format is similar while
# calling lm() (response~predictors),
# then, specify your data frame and 'mars.control' object
concretemars <-mars(formula=ConcreteCompressiveStrength~.,
                    data=concrete,control = mc)
#After creating the 'mars' object, you can apply five methods for class 'mars'

#First method: summary.mars()

summary(concretemars)# use summary() directly, S3 method dispatch will find
#                                       summary.mars() using the correct class.

#Second method: print.mars()

print(concretemars)# use print() directly, S3 method dispatch will find
#                                         print.mars() using the correct class.

#Third method: predict.mars()

predict(concretemars)# use predict() directly, S3 method dispatch will find
#                                       predict.mars() using the correct class.

#Fourth method: anova.mars()

anova(concretemars)# use anova() directly, S3 method dispatch will find
#                                   anova.mars() using the correct class.

#Fifth method: plot.mars()

plot(concretemars)# use plot() directly, S3 method dispatch will find
#                                         plot.mars() using the correct class.

#Example 2,3 have similar logic. Therefore, not all comments will be
#demonstrated again.

#################################Example 2######################################
data(fishToxicity)#read data fishToxicity within the package KTPmars,
#                                        use ?fishToxicity for more information

mc <- mars.control(Mmax = 8,trace = TRUE)#create a 'mars.control' object,
# see help file ?mars.control for more information. Mmax is 8 which means we
# want to add 8 basis functions in forward selection and use them for backward
# selection. trace = TRUE means user wants to see the candidate subset basis
# functions that might reduce LOF (RSS) with GCV criterion in backward
# selection procedure.

toxicmars <-mars(formula=LC50~., data=fishToxicity,control = mc)

#First method: summary.mars()

summary(toxicmars)

#Second method: print.mars()

print(toxicmars)

#Third method: predict.mars()

predict(toxicmars)

#Fourth method: anova.mars()

anova(toxicmars)

#Fifth method: plot.mars()

plot(toxicmars)

#################################Example 3######################################
data(yacht)#read data yacht within the package KTPmars, use ?yacht for more
#                                                                   information

mc <- mars.control(Mmax = 10,trace = TRUE)#create a 'mars.control' object,
# see help file ?mars.control for more information. Mmax is 10 which means we
# want to add 10 basis functions in forward selection and use them for backward
# selection. trace = TRUE means user wants to see the candidate subset basis
# functions that might reduce LOF (RSS) with GCV criterion in backward
# selection procedure.

yachtmars <-mars(formula=RRPUWOD~., data=yacht,control = mc)

#First method: summary.mars()

summary(yachtmars)

#Second method: print.mars()

print(yachtmars)

#Third method: predict.mars()

predict(yachtmars)

#Fourth method: anova.mars()

anova(yachtmars)

#Fifth method: plot.mars()

plot(yachtmars)

