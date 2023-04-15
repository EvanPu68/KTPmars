#' Summary implementatioN - S3Method for an object of class 'mars'
#'
#' @description
#' To give an overview of the fitted model's properties, for an object of class
#' 'mars', calling generic summary() with a mars object will dispatch the correct
#' method for class 'mars'.
#'
#' @param object an object of class 'mars'.
#'
#' @return summary() information similar to an object of class 'lm', with
#' information of basis functions.
#'
#' @export
#'
#' @examples
#' mm<-mars(ConcreteCompressiveStrength~.,data=concrete,control = mars.control(
#' Mmax=10,d=3,trace=FALSE))
#' tsummary <- summary(mm)
#'
#' @references
#'
#' i)
#'
#' Paper: Multivariate Adaptive Regression Splines
#'
#' Author(s): Jerome H. Friedman
#'
#' Source: The Annals of Statistics , Mar., 1991, Vol. 19, No. 1 (Mar., 1991), pp. 1-67
#'
#' Published by: Institute of Mathematical Statistics Stable
#'
#' URL: https://www.jstor.org/stable/2241837
#'
#' ii)
#'
#' Github repo of STAT 360 by Becky Lin:
#'
#' https://github.com/Becky07/STAT360
#'
summary.mars<-function(object){
  print(summary.lm(object)) # prints summary output of a 'lm' object
  for (i in 1:length(names(object$B))){# loop over Basis functions and print
    # hinge function information for each of them
    cat(names(object$B)[[i]], ":","\n")
    if(i == 1){
      cat("Intercept", "\n")
      next
    }
    Bf = object$Bfuncs[[i]]
    for(j in 1:nrow(Bf))
      cat("Component ",j,":", "\n",
          "","Sign:",Bf[j,1],"\n",
          "","Split Variable:",Bf[j,2],"\n",
          "","Split Point:",Bf[j,3],"\n")
  }
}
