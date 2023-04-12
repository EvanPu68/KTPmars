#' Title
#'
#' @param object 
#'
#' @return plots
#' @export
#'
#' @examples plot<-plot(testmars)
plot.mars <- function(object) {
  fitted <- fitted(object)
  resid <- residuals(object)
  par(mfrow=c(1,3))
  plot(fitted, resid, col = "blue", pch = 20, main = "Residuals vs. Fitted Values", xlab="Fitted Values",
       ylab="Residuals")
  abline(h = 0, col = "red")
  hist(resid, col = "blue", main = "Histogram of Residuals", xlab="Residuals")
  qqnorm(resid, col = "blue", main = "Normal Q-Q Plot of Residuals", ylab="Residuals")
  qqline(resid, col = "red")
}
