#' anova implementation
#'
#' @param object a mars object
#'
#' @return anova information regarding the mars object
#' @export
#'
#' @examples
#' mm<-mars(y~.,data=marstestdata)
#' anova(mm)
anova.mars <- function(object) {
  fit <- lm(y~.-1,data=data.frame(y=object$y,object$B))
  anova(fit)
}
