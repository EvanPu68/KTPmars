#' Multivariate Adaptive Regression Splines (MARS)
#'
#' Fit Friedman's Multivariate Adaptive Regression Splines (MARS) model.
#'
#' @param formula an object of class 'formula': Specifically, a linear regression
#' formula that has the response variable's name and indicates the predictors.
#' It is similar to the formula used in lm().
#' @param data a data frame containing the variables, including response and
#' predictor variables.
#' @param control an object of class 'mars.control': It is a list
#' containing information of Mmax (The maximum basis function we want to add in
#' forward selection of linear model fitting, it must be an even integer),
#' d (The parameter used in GCV
#' criterion in LOF, Friedman suggests d = 3, which is set to the default value of d),
#' trace (A logical value that allows user to trace the reduced basis
#' functions in backward selection in each iteration). You can set up a
#' 'mars.control' object using the helping function mars.control(), see help
#' file of mars.control() for further information.
#'
#' @return An object of class 'mars', which is similar to an object of class
#' 'lm', with extra information on basis functions.
#'
#' An object class of 'mars' is a list containing the following information:
#' \itemize{
#' \item{call:}{ the matched function call of mars().}
#' \item{formula:}{ formula used in function call of mars().}
#' \item{y:}{ the column vector of response variable.}
#' \item{B:}{ a matrix that contains information of basis functions determined by
#' forward and backward selection, which is the data used in fitting the
#' linear regression model in mars(). Each column of basis functions are
#' products of hinge functions with information of sign, number of predictor and
#' split point given in Bfuncs in order.}
#' \item{Bfuncs:}{ a list containing information on how each basis function
#' is created, each element of Bfuncs correspond to the hinge functions that are
#' used to create each column of B. Inside each element of Bfuncs, there could be
#' null, 1 or multiples rows. Each row represents information of each hinge function.}
#' \item{x_names:}{ a character vector of predictors' names.}
#' \item{coefficients:}{ a vector of the coefficients.}
#' \item{residuals:}{ a vector of difference between observed response and fitted response.}
#' \item{effects:}{ a numeric vector of orthogonal effects from the fitted model.}
#' \item{rank:}{ the numeric rank of the fitted linear model.}
#' \item{fitted.values:}{ the fitted mean values.}
#' \item{assign:}{ a vector from 1 to the value of rank.}
#' \item{qr:}{ a list of qr decomposition information.}
#' \item{df.residual:}{ the residual degree of freedom.}
#' \item{xlevels:}{ a record of the levels of the factors used in fitting.}
#' \item{call:}{ the match lm function call using basis functions.}
#' \item{terms:}{ the 'term' object that is extracted from basis functions and formula.}
#' \item{model:}{ the model frame used.}
#' }
#'
#' @export
#'
#' @examples
#' mm <- mars(ConcreteCompressiveStrength~.,data=concrete)
#' @import stats
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
#' iii)
#'
#' Rdocumentation:
#'
#' https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm
#'
#' https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/effects
#'
#' https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/terms
#'
#'

mars <- function(formula,data,control = mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data) # a data frame with columns y and all
  #                                 predictors according to the formula
  y <- model.response(mf) # select the response variable column
  mt <- attr(mf, "terms") # a detailed linear regression formula based on
  #                         the formula input, e.g. y~x1+...+x10
  x <- model.matrix(mt, mf)[,-1,drop=FALSE] # select the predictor matrix
  #                                        and keep it as a matrix
  x_names <- colnames(x) # record the names of predictors
  control <- validate_mars.control(control) # use validator to double-check
  #                                          mars.control object
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B)) # notice -1 added to delete extra
  #                                           intercept column fit the model
  #                                           based on output of backward
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}


#' Forward stepwise function based on Algorithm 2 by Friedman
#'
#' @param y the vector of response variable used.
#' @param x the model matrix used.
#' @param control an object of class 'mars.control': a list of Mmax, d, and trace,
#' see helper file mars.control() for more information.
#'
#' @return a list containing the following information:
#' \itemize{
#' \item{y:}{ the column vector of response variable.}
#' \item{B:}{ a matrix that contains information of basis functions determined by
#' forward selection. Each column of basis functions are
#' products of hinge functions with information of sign, number of predictor and
#' split point given in Bfuncs in order.}
#' \item{Bfuncs:}{ a list containing information on how each basis function
#' is created, each element of Bfuncs correspond to the hinge functions that are
#' used to create each column of B. Inside each element of Bfuncs, there could be
#' null, 1 or multiples rows. Each row represents information of each hinge function.}
#' }
#' @export
#'
#' @examples
#' fwdtest <- fwd_stepwise(y=concrete$ConcreteCompressiveStrength,x=concrete[,-1],
#' control = mars.control(8,3,FALSE))
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
fwd_stepwise <- function(y,x,control=mars.control()){
  #---------------------------------------------------
  # Error checking for Mmax
  if(control$Mmax < 2) {
    warning("\nMmax was less than 2, therefore adjusted to 2\n")
    control$Mmax <- 2
  }
  #---------------------------------------------------
  # Initialize:
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors = number of X
  # B: a data frame with optimal basis function as columns
  B <- init_B(N,control$Mmax) # Initialize B
  Bfuncs <- vector(mode="list", length = control$Mmax+1) # Initialize Bfuncs
  #
  #---------------------------------------------------
  # Looping for forward selection:
  for(i in 1:(control$Mmax/2)) { # add 1 pair of basis functions in each
    #                              iteration
    M = (2*i)-1 # for ith pair, there are 2*i - 1 basis functions available
    lof_best <- Inf # set an initial threshold for LOF with GCV criterion
    for(m in 1:M) {
      uni <- setdiff(1:n,Bfuncs[[m]][,2]) # avoid splitting the same basis
      #                                   function using same predictor again
      for(v in uni){ # select a variable to split on
        tt <- split_points(x[,v],B[,m]) # select the values of selected values
        #                                that is available to be a split point
        for(t in tt) {
          Bnew <- data.frame(B[,(1:M)],
                             Btem1=B[,m]*h(s=+1,x=x[,v],t=t),
                             Btem2=B[,m]*h(s=-1,x=x[,v],t=t))#add two temporary
          #    basis functions according to the M,m,v,t values in current loops
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(form = y~.-1,data = gdat,control=control) # -1 is added to
          #                                       delete extra intercept column
          if(lof < lof_best) { #If LOF with GCV criterion of new data frame is
            #                                              lower than threshold
            lof_best <- lof # update the threshold
            note <- c(m=m,v=v,t=t) # record optimal (m, v, t) to update basis
            #                        functions later
          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split
    mstar <- note["m"]
    vstar <- note["v"]
    tstar <- note["t"]
    B[,M+1] <- c(B[,mstar])*h(x=x[,vstar], s=-1, t=tstar)# update B for ith pair
    #                                                    of basis functions
    B[,M+2] <- c(B[,mstar])*h(x=x[,vstar], s=+1, t=tstar)# update B for ith pair
    #                                                    of basis functions
    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]],c(s=-1, v=vstar, t=tstar))
    # update Bfuncs for ith pair of basis functions
    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]],c(s=1, v=vstar, t=tstar))
    # update Bfuncs for ith pair of basis functions
  } # end loop over i
  #
  #---------------------------------------------------
  # Name the columns in matrices in each index of Bfuncs, except for the first:
  colnames(B) <- paste0("B",(0:(ncol(B)-1)))
  temp <- Bfuncs[-1]
  temp <- lapply(temp,`dimnames<-`,list(NULL,c("s","v","t")))
  Bfuncs <- c(Bfuncs[1],temp)
  #
  #---------------------------------------------------
  # Return the results in form of a list:
  return(list(y=y,B=B,Bfuncs = Bfuncs))
}

#' Backward stepwise function based on Algorithm 3 by Freifdman
#'
#' @param fwd output of forward stepwise function, it is a list of vector of
#' response, Basis function matrix and Bfuncs which recorded the information of
#' basis functions.
#' @param control an object of class 'mars.control': a list of Mmax, d, and trace,
#' see helper file mars.control() for more information.
#'
#' @return a list containing the following information:
#' \itemize{
#' \item{y:}{ the column vector of response variable.}
#' \item{B:}{ a matrix that contains information of basis functions reduced from
#' the B in forward selection to avoid overfitting. Each column of basis functions are
#' products of hinge functions with information of sign, number of predictor and
#' split point given in Bfuncs in order.}
#' \item{Bfuncs:}{ a list containing information on how each basis function
#' is created, each element of Bfuncs correspond to the hinge functions that are
#' used to create each column of B. Inside each element of Bfuncs, there could be
#' null, 1 or multiples rows. Each row represents information of each hinge function.}
#' }
#'
#' @export
#'
#' @examples
#' fwdtest <- fwd_stepwise(y=concrete$ConcreteCompressiveStrength,x=concrete[,-1],
#' control = mars.control(8,3,FALSE))
#'
#' bwdtest <- bwd_stepwise(fwdtest,control = mars.control(8,3,FALSE))
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
bwd_stepwise<-function(fwd,control){
  #fwd is a list with elements y, B, and Bfuncs
  #
  #---------------------------------------------------
  # Initailize:
  Mmax<-ncol(fwd$B)-1
  Jstar<-2:(Mmax+1) # Jstar will be the index for the final subset of
  # basis functions, right now it should equal to result in fwd
  Kstar<-Jstar # Let Kstar equals to Jstar
  dat <- data.frame(y=fwd$y,fwd$B)
  lofstar<-LOF(form = y~.-1,data = dat,control=control)#-1 removes intercept
  # column in fwd$B, and calculate the final LOF with GCV criterion in fwd
  #
  #---------------------------------------------------
  # Looping for backward selection:
  for(M in (Mmax+1):2){ # Allow testing LOF with GCV criterion of 11 basis
    #                                                       functions to only 2
    b<-Inf # Initialize a threshold for LOF with GCV criterion regardless of GCV
    #                                                                     in fwd
    L<-Kstar # L represents the candidate basis functions that can be eliminated
    if(control$trace) cat("L",L,"\n")
    for(m in L){ # Loop all candidate basis functions
      K <- setdiff(L,m) # delete the mth basis functions
      dat2 <- data.frame(y=fwd$y,fwd$B[,c(1,K)])# add back the first column
      lof <- LOF(form = y~.-1,data = dat2,control=control)#-1 removes intercept
      # column in fwd$B, calculate LOF of current mth iteration
      if(lof<b){
        b<-lof # Update LOF threshold b as long as eliminating mth basis
        # function reduces previous LOF
        Kstar <- K # Update the available basis functions in next Mth loop
      }
      if(lof<lofstar){
        lofstar<-lof # If the current LOF is lower then result in fwd, update
        # the threshold lofstar
        Jstar <- K # Update the basis functions that result in a lower LOF for
        # mars object
      }
    }
  }

  Jstar <- c(1,Jstar) # Select the basis functions that reduce LOF in fwd
  #
  #---------------------------------------------------
  # Return the results in form of a list:
  return(list(y=fwd$y,B=fwd$B[,Jstar],Bfuncs=fwd$Bfuncs[Jstar]))
}

#' Initialization function for basis function matrix in forward selection
#'
#' @param N number of observations.
#' @param Mmax Mmax from an object of class 'mars.control', the maximum basis
#' functions will be added in forward stepwise function.
#'
#' @return a data frame with first column of 1s and rest column as NA.
#' @export
#'
#' @examples
#' Bmatrix <- init_B(100,10)
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
init_B <- function(N,Mmax) {
  dd <- data.frame(matrix(NA,nrow = N, ncol = Mmax + 1))
  dd[[1]] <- rep(1,N) # first column of B is the intercept, which is 1
  names(dd)<-paste0("B",0:Mmax) # set column names for B
  return(dd)
}

#' LOF function with GCV criterion that produces
#'
#' @param form an object of class 'formula': Specifically, a linear regression
#' formula that has the response variable's name and indicates the predictors.
#' It is similar to the formula used in lm(), but must add -1 at the end to
#' avoid an extra intercept column.
#' @param data a data frame with response and predictor variables.
#' @param control an object of class 'mars.control': a list of Mmax, d, and trace,
#' see helper file mars.control() for more information.
#'
#'
#' @return value of LOF with GCV criterion.
#' @export
#'
#' @examples
#' tlof <- LOF(form = ConcreteCompressiveStrength~.-1,data = concrete,
#' control=mars.control())
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
LOF <- function(form,data,control) {
  mod <- lm(form,data) # fit a lm object using given formula and data, notice
  # that we assume data includes y and entire B matrix(including intercept), and
  # formula includes -1.
  rss <- sum((mod$res)^2) # get the residual sum of squares
  N = nrow(data) # number of observations
  M = length(mod$coefficients)-1 # number of basis functions except intercept
  cm = sum(diag(hatvalues(mod))) # sum of the hat-values from the fitted model
  d = control$d # parameter d in mars.control object
  tildacm = cm + (d*M)
  out = rss * (N/((N-tildacm)^2)) # value of LOF with GCV criterion
  return(out)
}

#' Hinge function for predictor value adjustment.
#'
#' @param x a vector of predictor value.
#' @param s a numeric value of either 1 or -1, indicating sign of hinge function.
#' @param t value of split point, the knot of hinge function.
#'
#' @return a vector of numeric values that adjust values of given predictor
#' vector according to the hinge function.
#' @export
#'
#' @examples
#' h(x=concrete$Cement,s=1,t=300)
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
h <- function(x,s,t) { # hinge function
  return(pmax(0,s*(x-t)))
}

#' A function that picks unique, available split point value in a vector of
#' predictor value.
#'
#' @param xvar a vector of predictor value.
#' @param Bm mth column in basis function matrix.
#'
#' @return a vector of sorted, unique and valid split points, excluding the
#' highest value.
#' @export
#'
#' @examples
#' fwdtest <- fwd_stepwise(y=concrete$ConcreteCompressiveStrength,x=concrete[,-1],
#' control = mars.control(6,3,FALSE))
#'
#' sp <- split_points(concrete$Cement,fwdtest$B[,3])
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
split_points <- function(xvar,Bm) {
  temp <- xvar[Bm>0] # flag the indexs that is positive in mth basis function,
  # and use them to filter avalable split points in vth predictor.
  result <- sort(unique(temp)) # avoid repetition and sort in order
  result <- result[-length(result)] # delete the last split point value
  return(result)
}


#------------------------------------------------------------------------
# constructor, validator and helper for class mars.control
#------------------------------------------------------------------------
#

#' Constructor for 'mars.control' objects
#'
#' @param control a list of values Mmax, d and trace.
#'
#' @return an object of class 'mars.control'.
#' @export
#'
#' @examples
#' testnewmc <- new_mars.control(list(Mmax=10,d=3,trace=TRUE))
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
new_mars.control <- function(control) {
  structure(control,class="mars.control") # specify class to 'mars.control'
}

#' Validator for 'mars.control' objects
#'
#' @param control a list of values Mmax, d and trace.
#'
#' @return the original list if Mmax, d and trace are valid for the list to be
#' assigned to an object of class 'mars.control'. Otherwise,
#' either returning errors or coercing the values while sending warnings.
#'
#' @export
#'
#' @examples
#' validmc <- validate_mars.control(list(Mmax=10,d=3,trace=TRUE))
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
validate_mars.control <- function(control) {
  #
  #---------------------------------------------------
  # Stop the function if one of the types of inputs for mars.control object is
  # wrong:
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
  #
  #---------------------------------------------------
  # Coerce Mmax if it has right type, but wrong value
  # (<2 or not an even integer):
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  control# return the coerced input
}

#' Helper for 'mars.control' objects
#'
#' This function constructs a `mars.control` object that specifies
#' parameters used in the model fitting procedure.
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer.
#' Default value is 2.
#' @param d The parameter used in calculation of Generalized cross-validation.
#' Friedman suggests that d = 3 works well, thus default value is 3.
#' @param trace A True or False value that allows user to see the candidate subset
#' basis functions that might reduce LOF with GCV criterion in backward selection
#' procedure similar to linear regression modeling.
#'
#' @return an object of class 'mars.control', that is a list of parameters
#' @export
#'
#' @examples mc<-mars.control(Mmax=10,d=3,trace=FALSE)
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
mars.control <- function(Mmax = 2,d = 3,trace = FALSE) {
  Mmax <- as.integer(Mmax) # coerce Mmax to an integer
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}


