#' Single imputation using random forest algorithm with normality assumption
#' for prediction error
#'
#' @param y Vector to be imputed
#'
#' @param ry Logical vector of length \code{length(y)} indicating the
#' the subset \code{y[ry]} of elements in \code{y} to which the imputation
#' model is fitted. The \code{ry} generally distinguishes the observed
#' (\code{TRUE}) and missing values (\code{FALSE}) in \code{y}.
#'
#' @param x Numeric design matrix with \code{length(y)} rows with predictors for
#' \code{y}. Matrix \code{x} may have no missing values.
#'
#' @param wy Logical vector of length \code{length(y)}. A \code{TRUE} value
#' indicates locations in \code{y} for which imputations are created.
#'
#' @param num.trees Number of trees to build, default to \code{10}, overrides
#' \code{num.trees.continuous} and \code{num.trees.categorical}
#'
#' @param num.trees.continuous Number of trees to build for imputing continuous
#' variables, default to \code{10}
#'
#' @param num.trees.categorical Number of trees to build for imputing
#' categorical variables, default to \code{10}
#'
#' @param use.pred.prob.categorical Logical indicator, \code{TRUE} for
#' imputation based on probabilities of votes, \code{FALSE} for imputation
#' based on majority votes, default to \code{TRUE}
#'
#' @param pre.bootstrap Perform bootstrap prior to imputation to get 'proper'
#' imputation, i.e. accommodating sampling variation in estimating population
#' regression parameters (see Shah et al. 2014)
#'
#' @param ... Other arguments to pass down
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @references
#'
#' Stekhoven, Daniel J., and Peter Bühlmann.
#' "MissForest—non-parametric missing value imputation for mixed-type data."
#' Bioinformatics 28.1 (2012): 112-118.
#'
#' Shah, Anoop D., et al. "Comparison of random forest and parametric
#' imputation models for imputing missing data using MICE: a CALIBER study."
#' American journal of epidemiology 179.6 (2014): 764-774.
#'
#' @examples
#' imp <- mice(nhanes, method = "mf.norm")
#'
#' @export
mice.impute.mf.norm <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees = 10,
    num.trees.continuous = NULL,
    num.trees.categorical = NULL,
    use.pred.prob.categorical = FALSE,
    pre.bootstrap = FALSE,
    ...
) {
    if (is.numeric(y)) {
        if (is.null(num.trees.continuous)) num.trees.continuous <- num.trees
        rfImpVec <- mice.impute.mf.continuous(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.continuous = num.trees.continuous,
            normal.error.continuous = TRUE,
            pre.bootstrap = pre.bootstrap,
            ...
        )
    } else {
        if (is.null(num.trees.categorical)) num.trees.categorical <- num.trees
        rfImpVec <- mice.impute.mf.categorical(
            y = y,
            ry = ry,
            x = x,
            wy = wy,
            num.trees.categorical = num.trees.categorical,
            use.pred.prob.categorical = use.pred.prob.categorical,
            pre.bootstrap = pre.bootstrap,
            ...
        )
    }
    return(rfImpVec)
}
