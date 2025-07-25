#' `RobinCar2` Package
#'
#' `RobinCar2` implements unbiased prediction and robust inference of variance of a fit in R.
#'
#' @aliases RobinCar2-package
#' @import checkmate
#' @importFrom numDeriv grad
#' @importFrom stats predict residuals fitted model.response model.matrix coefficients family
#' gaussian terms glm var family pnorm var as.formula qnorm lm confint
#' @importFrom sandwich vcovHC
#' @importFrom MASS negative.binomial
#' @importFrom utils combn tail
"_PACKAGE"
