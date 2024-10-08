#' `RobinCar2` Package
#'
#' `RobinCar2` implements unbiased prediction and robust inference of variance of a fit in R.
#'
#' @aliases RobinCar2-package
"_PACKAGE"

#' @import checkmate
#' @importFrom numDeriv jacobian
#' @importFrom stats predict residuals fitted model.response model.matrix coefficients family
#' gaussian terms glm var family pnorm var
#' @importFrom sandwich vcovHC
#' @importFrom prediction find_data
NULL
