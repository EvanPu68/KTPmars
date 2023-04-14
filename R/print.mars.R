#' Print implementatioN - S3Method for an object of class 'mars'
#'
#' @param object an object of class 'mars'.
#'
#' @return prints matched function call and coefficients, similar to the print()
#' output of an object of class 'lm', and information of basis functions.
#' @export
#'
#' @examples
#' mm<-mars(ConcreteCompressiveStrength~.,data=concrete,control = mars.control(
#' Mmax=10,d=3,trace=FALSE))
#' result <- print(mm)
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

print.mars<-function(object){
  print(object$call) # print the matched call
  cat("Coefficients:","\n")
  print(object$coefficients) # print the coefficients
  for (i in 1:length(names(object$B))){ # loop over Basis functions and print
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
