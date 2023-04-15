#' Anova implementation - S3Method for an object of class 'mars'
#'
#' @description
#' To provide useful information about the basis functions for an object of class
#' 'mar', calling generic anova() with a mars object will dispatch the correct
#' method for class 'mars'.
#'
#' @param object an object of class 'mar'.
#'
#' @return anova information regarding the mars object, similar to anova output
#' of an object of class 'lm', and information of Basis functions.
#' @export
#'
#' @examples
#' mm<-mars(ConcreteCompressiveStrength~.,data=concrete,control = mars.control(
#' Mmax=10,d=3,trace=FALSE))
#' output <- anova(mm)
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
anova.mars <- function(object) {
  fit <- lm(y~.-1,data=data.frame(y=object$y,object$B))
  print(anova(fit)) # print anova output for an object of class 'lm'
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
