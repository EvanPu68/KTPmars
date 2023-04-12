#' Title
#'
#' @param object a mars object
#'
#' @return lm summary plus information of basis function
#' @export
#'
#' @examples summary<-summary(testmars)
summary.mars<-function(object){
  print(summary.lm(object))
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
