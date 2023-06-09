#' Plot implementatioN - S3Method for an object of class 'mars'
#'
#' @description
#' To produce graphics for an object of class 'mars', calling generic plot() with
#' a mars object will dispatch the correct method for class 'mars'.
#'
#' @param object an object of class 'mars'
#'
#' @return 3 plots for the fitted model using basis functions:
#' Residuals vs. Fitted values plot, histogram of residuals and normal Q-Q plot
#' of residuals.
#' @export
#'
#' @examples
#' mm<-mars(ConcreteCompressiveStrength~.,data=concrete,control = mars.control(
#' Mmax=10,d=3,trace=FALSE))
#' plots <- plot(mm)
#'
#' @import graphics
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
plot.mars <- function(object) {
  fitted <- fitted(object) # obtain fitted value of response
  resid <- residuals(object) # obtain the value of residuals
  par(mfrow=c(1,3))
  plot(fitted, resid, col = "blue", pch = 20, main = "Residuals vs. Fitted Values", xlab="Fitted Values",
       ylab="Residuals")
  abline(h = 0, col = "red")
  hist(resid, col = "blue", main = "Histogram of Residuals", xlab="Residuals")
  qqnorm(resid, col = "blue", main = "Normal Q-Q Plot of Residuals", ylab="Residuals")
  qqline(resid, col = "red")
}
