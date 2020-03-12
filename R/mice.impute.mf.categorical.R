#' Single imputation for categorical variable based random forest
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
#' @param num.trees.categorical Number of trees to build for imputing
#' categorical variables, default to \code{10}.
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
#' @export
mice.impute.mf.categorical <- function(
    y,
    ry,
    x,
    wy = NULL,
    num.trees.categorical = 10,
    use.pred.prob.categorical = FALSE,
    pre.bootstrap = FALSE,
    ...){
    if (is.null(wy)) wy <- !ry
    yMisNum <- sum(wy)
    if (is.logical(y)) {
        yIsLogical <- TRUE
        y <- as.factor(y)
    } else {
        yIsLogical <- FALSE
    }
    if (isTRUE(pre.bootstrap)) {
        bootIdx <- sample(sum(ry), replace = TRUE)
        yObs <- y[ry][bootIdx]
        xObs <- x[ry, , drop = FALSE][bootIdx, , drop = FALSE]
    } else {
        yObs <- y[ry]
        xObs <- x[ry, , drop = FALSE]
    }
    # Short-cut
    yobsLvDrop <- droplevels(yObs)
    yObsLvNum <- nlevels(yobsLvDrop)
    if (yObsLvNum == 1)
        return(rep_len(yObs, length.out = yMisNum))

    xMis <- x[wy, , drop = FALSE]
    if (isTRUE(use.pred.prob.categorical)) {
        # Construct predictions based on vote probs averaged over nodes.
        # Suppress warnings like:
        # "Dropped unused factor level(s) in dependent variable"
        rangerObjProb <- suppressWarnings(ranger(
            x = xObs,
            y = yObs,
            probability = TRUE,
            oob.error = FALSE,
            num.trees = num.trees.categorical))
        misPredMat <- predictions(predict(rangerObjProb, xMis))
        yLevels <- colnames(misPredMat)
        impValChar <- apply(
            X = misPredMat,
            MARGIN = 1,
            FUN = function(voteProb) {
                sample(x = yLevels, size = 1, prob = voteProb)
            })
        impVal <- factor(x = impValChar, levels = levels(y))
    } else {
        # Construct predictions based on major votes, less variant
        rangerObj <- ranger(
            x = xObs,
            y = yObs,
            oob.error = FALSE,
            num.trees = num.trees.categorical)
        impVal <- predictions(predict(rangerObj, xMis))
    }
    if (yIsLogical) impVal <- as.logical(impVal == "TRUE")
    return(impVal)
}
