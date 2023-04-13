#' Title
#'
#' @param object 
#'
#' @return
#' @export
#'
#' @examples

print.mars<-function(object){
  print(object$call)
  cat("Coefficients:","\n")
  print(object$coefficients)
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