% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_couterfactual.R
\name{predict_counterfactual}
\alias{predict_counterfactual}
\title{Counterfactual Prediction}
\usage{
predict_counterfactual(fit, treatment, data, vcov, vcov_args, ...)
}
\arguments{
\item{fit}{fitted object.}

\item{treatment}{(\code{formula}) formula of form treatment ~ strata(s).}

\item{data}{(\code{data.frame}) raw dataset.}

\item{vcov}{(\code{function} or \code{character}) variance function or name.}

\item{vcov_args}{(\code{list}) additional arguments for variance function.}

\item{...}{Additional arguments for methods.}
}
\value{
List of class \code{prediction_cf} containing following elements:
\itemize{
\item \code{estimate}: predicted marginal mean.
\item \code{residual}: residual of the bias-corrected prediction.
\item \code{predictions}: all predicted values.
\item \code{predictions_liner}: linear predictions.
\item \code{schema}: randomization schema.
\item \code{response}: response value.
\item \code{fit}: fitted model.
\item \code{model_matrix}: model matrix.
\item \code{treatment_formula}: treatment assignment and randomization formula.
\item \code{treatment}: treatment value.
\item \code{group_idx}: group index based on the stratification.
\item \code{variance}: estimated variance of the marginal mean.
\item \code{variance_name}: name of the variance.
}
}
\description{
Obtain counterfactual prediction of a fit.
}
