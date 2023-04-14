#' Predict implementatioN - S3Method for an object of class 'mars'
#'
#' @param object an object of class 'mars'.
#' @param newdata a data frame with response and predictor variables.
#'
#' @return a vector of predicted response value.
#' @export
#'
#' @examples
#' mm<-mars(ConcreteCompressiveStrength~.,data=concrete,control = mars.control(
#' Mmax=10,d=3,trace=FALSE))
#' predictedvalues <- predict(mm)
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

predict.mars <- function(object,newdata) {
  if(missing(newdata) || is.null(newdata)) { #if new data is missing
    B <- as.matrix(object$B) # coerce B to matrix for matrix multiplication
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata) # create model matrix data frame
    mt <- attr(mf, "terms") # a linear regression formula without response
    X <- model.matrix(mt, mf)[,-1] # create model matrix
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta) # matrix multiplication
}

#' Function which uses Bfuncs and data frame to reproduce basis function matrix
#'
#' @param X a model matrix (predictors only).
#' @param Bfuncs a list of Bfuncs information, usually extracts from output of
#' 'mars' object.
#'
#' @return a basis function matrix.
#' @export
#'
#' @examples
#'     tmars <- mars(ConcreteCompressiveStrength~.,data=concrete,control =
#'     mars.control(10,3,FASLE))
#'     tr <- terms(ConcreteCompressiveStrength~.,data=concrete)
#'     tr <- delete.response(tr)
#'     mf <- model.frame(tr,concrete)
#'     mt <- attr(mf, "terms")
#'     tX <- model.matrix(mt, mf)[,-1]
#'     tB <- make_B(tX,tmars$Bfuncs)
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
make_B<-function(X,Bfuncs){
  #Set up the output data frame with known number of row and column
  output <- data.frame(matrix(NA,nrow = nrow(X), ncol = length(Bfuncs)))
  output[[1]] <- rep(1,nrow(X))# set the first column to 1, the intercept
  for(i in 2:(length(Bfuncs))){ #loop over all element of Bfuncs except the first
    sumpdt = 1 # set up an initial value of 1 for sum products of hinge function
    for(k in 1:nrow(Bfuncs[[i]])){ #loop over all rows in ith element of Bfuncs
      # calculate the sum product of hinge functions
      sumpdt = sumpdt * h(s=Bfuncs[[i]][k,1],x=X[,Bfuncs[[i]][k,2]],
                          t=Bfuncs[[i]][k,3])
    }
    output[[i]]<-sumpdt # assign the sum product to the ith element of output
    #                                                              data frame
  }
  output<-as.matrix(output)# coerce output to a matrix
  return(output)
}

