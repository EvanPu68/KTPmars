plot.mars <- function(object) {
  fitted <- fitted(object)
  resid <- residuals(object)
  #predicted <- predict.mars(object)
  #plot(x[,1], y, col = "blue", pch = 20, main = "Observed vs. Fitted")
  plot(x[,1], fitted, col = "red")
  plot(fitted, resid, col = "blue", pch = 20, main = "Residuals vs. Fitted Values", xlab="Fitted Values",
       ylab="Residuals")
  abline(h = 0, col = "red")
  hist(resid, col = "blue", main = "Histogram of Residuals", xlab="Residuals")
  qqnorm(resid, col = "blue", main = "Normal Q-Q Plot of Residuals", ylab="Residuals")
  qqline(resid, col = "red")
}
