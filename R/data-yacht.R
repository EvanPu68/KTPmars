#' A yacht dataset for the KTPmars package.
#'
#' A dataset of size N=100 with n=6 explanatory variables,
#' and a response variable that is Residuary resistance per unit weight of
#' displacement (RRPUWOD). Predictor LPOTCOB means Longitudinal position of the
#' center of buoyancy. All other 5 predictors have original names.
#'
#' @format A data frame with 100 rows and 7 variables:
#' \describe{
#'   \item{RRPUWOD}{response variable}
#'   \item{LPOTCOB}{explanatory variable}
#'   \item{PrismaticCoefficient}{explanatory variable}
#'   \item{LengthDisplacementRatio}{explanatory variable}
#'   \item{BeamDraughtRatio}{explanatory variable}
#'   \item{LengthBeamRatio}{explanatory variable}
#'   \item{FroudeNumber}{explanatory variable}
#' }
#' @references
#'
#' This dataset is a shorten version of the dataset acquired from:
#'
#' https://archive.ics.uci.edu/ml/datasets/Yacht+Hydrodynamics
#'
#' with only first 100 observations.
"yacht"
