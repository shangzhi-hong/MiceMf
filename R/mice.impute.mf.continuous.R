#' Single imputation for continuous variable based random forest
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
#' @param num.trees.continuous Number of trees to build for imputing continuous
#' variables, default to \code{10}
#'
#' @param pre.bootstrap Perform bootstrap prior to imputation to get 'proper'
#' imputation, i.e. accommodating sampling variation in estimating population
#' regression parameters (see Shah et al. 2014).
#'
#' @param normal.error.continuous Logical indicator, \code{TRUE} for
#' assuming normal distribution for error,
#' the variance equals to overall out-of-bag prediction error,
#' i.e. mean squared error (see Shah et al. 2014).
#'
#' @param ... Other arguments to pass down
#'
#' @return Vector with imputed data, same type as \code{y}, and of length
#' \code{sum(wy)}
#'
#' @author Shangzhi Hong, Henry S. Lynn*
#'
#' @export
mice.impute.mf.continuous <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.continuous = 10,
    pre.bootstrap = FALSE,
    normal.error.continuous = FALSE,
    ...
    ) {
    if (is.null(wy)) wy <- !ry
    yMisNum <- sum(wy)
    if (isTRUE(pre.bootstrap)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[ry][bootIdx]
        xObs <- x[ry, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[ry]
        xObs <- x[ry, , drop = FALSE]
    }
    xMis <- x[wy, , drop = FALSE]
    rangerObj <- ranger(
        x = xObs,
        y = yObs,
        oob.error = TRUE,
        num.trees = num.trees.continuous)
    misPredVal <- predictions(predict(rangerObj, xMis))
    if (normal.error.continuous) {
        # Use overall out-of-bag prediction error
        impVal <- rnorm(length(misPredVal),
                        mean = misPredVal,
                        sd = sqrt(rangerObj[["prediction.error"]]))
    } else {
        impVal <- misPredVal
    }
    return(impVal)
}
