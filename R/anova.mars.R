#' anova implementation - S3Method for an object of class 'mars'
#'
#' @param object a mars object.
#'
#' @return anova information regarding the mars object and information of Basis
#' functions.
#' @export
#'
#' @examples
#' mm<-mars(ConcreteCompressiveStrength~.,data=concrete,mar.)
#' anova(mm)
anova.mars <- function(object) {
  fit <- lm(y~.-1,data=data.frame(y=object$y,object$B))
  anova(fit)
  for (i in 1:length(names(object$B))){
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
