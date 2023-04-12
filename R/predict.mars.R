#' Title
#'
#' @param object mars object
#' @param newdata data frame
#'
#' @return B matrix
#' @export

predict.mars <- function(object,newdata) {
  if(missing(newdata) || is.null(newdata)) { #if new data is missing
    B <- as.matrix(object$B) # coerce B to matrix for matrix multiplication
  }
  else {
    tt <- terms(object$formula,data=newdata) #complete
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

make_B<-function(X,Bfuncs){
  output <- data.frame(matrix(NA,nrow = nrow(X), ncol = length(Bfuncs)))
  output[[1]] <- rep(1,nrow(X))
  for(i in 2:(length(Bfuncs))){
    output[[i]]<-h(s=Bfuncs[[i]][1,1],x=X[,Bfuncs[[i]][1,2]],t=Bfuncs[[i]][1,3])
    if( nrow(Bfuncs[[i]]) > 1){
      for(k in 2:nrow(Bfuncs[[i]]))
        output[[i]]<-output[[i]]*h(s=Bfuncs[[i]][k,1],x=X[,Bfuncs[[i]][k,2]],
                                   t=Bfuncs[[i]][k,3])
    }
  }
  output<-as.matrix(output)
  return(output)
}
