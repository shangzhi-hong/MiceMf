% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mice.impute.mf.categorical.R
\name{mice.impute.mf.categorical}
\alias{mice.impute.mf.categorical}
\title{Single imputation for categorical variable based random forest}
\usage{
mice.impute.mf.categorical(
  y,
  ry,
  x,
  wy = NULL,
  num.trees.categorical = 10,
  use.pred.prob.categorical = FALSE,
  pre.bootstrap = FALSE,
  ...
)
}
\arguments{
\item{y}{Vector to be imputed}

\item{ry}{Logical vector of length \code{length(y)} indicating the
the subset \code{y[ry]} of elements in \code{y} to which the imputation
model is fitted. The \code{ry} generally distinguishes the observed
(\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.}

\item{x}{Numeric design matrix with \code{length(y)} rows with predictors for
\code{y}. Matrix \code{x} may have no missing values.}

\item{wy}{Logical vector of length \code{length(y)}. A \code{TRUE} value
indicates locations in \code{y} for which imputations are created.}

\item{num.trees.categorical}{Number of trees to build for imputing
categorical variables, default to \code{10}.}

\item{use.pred.prob.categorical}{Logical indicator, \code{TRUE} for
imputation based on probabilities of votes, \code{FALSE} for imputation
based on majority votes, default to \code{TRUE}}

\item{pre.bootstrap}{Perform bootstrap prior to imputation to get 'proper'
imputation, i.e. accommodating sampling variation in estimating population
regression parameters (see Shah et al. 2014)}

\item{...}{Other arguments to pass down}
}
\value{
Vector with imputed data, same type as \code{y}, and of length
\code{sum(wy)}
}
\description{
Single imputation for categorical variable based random forest
}
\author{
Shangzhi Hong, Henry S. Lynn*
}
