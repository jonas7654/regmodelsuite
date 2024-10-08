% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stepwise_selection.R
\name{forward_selection}
\alias{forward_selection}
\title{Finds a subset of predictors using forward selection}
\usage{
forward_selection(X, y, n_predictors, model_fct = lm, verbose = TRUE)
}
\arguments{
\item{X}{Dataset X}

\item{y}{Dataset y}

\item{n_predictors}{An integer which defines the amount of predictors to
select.}

\item{model_fct}{A function which specifies the modelused.
Needs to have the parameters \code{formula} and \code{data} and needs to
return and object with a \code{predict} function implemented.}

\item{verbose}{A logical which specifies if the selection process should be
printed.}
}
\value{
A stepwise_selection object
}
\description{
Finds a subset of predictors using forward selection
}
