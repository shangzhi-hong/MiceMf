#' MissForest-flavored missing data imputation under MICE framework
#'
#' @param xmis Data set containing missing values
#'
#' @param maxiter Number of imputation iterations
#'
#' @param ntree Number of trees to build
#'
#' @param mtry Number of variables randomly sampled at each split
#'
#' @param decreasing If \code{FALSE} then the variables are sorted according
#' increasing amount of missing cells.
#'
#' @param ... Other arguments to pass down
#'
#' @return Imputed complete dataset
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
#' imp <- missForestMice(nhanes)
#'
#' @export
missForestMice <- function(xmis,
                           maxiter = 5,
                           ntree = 10,
                           mtry = floor(sqrt(ncol(xmis))),
                           decreasing = FALSE,
                           ...) {
    # Set-up missForest-flavored initial value
    xInit <- xmis
    xAttrib <- lapply(xmis, attributes)
    numVar <- ncol(xmis)
    for (varIdx in 1:numVar) {
        if (is.null(xAttrib[[varIdx]])) {
            xInit[is.na(xmis[, varIdx]), varIdx] <-
                mean(xmis[, varIdx], na.rm = TRUE)
        } else {
            max.level <- max(table(xInit[, varIdx]))
            class.assign <-
                sample(names(which(max.level == summary(xInit[, varIdx]))), 1)
            if (class.assign != "NA's") {
                xInit[is.na(xmis[, varIdx]), varIdx] <- class.assign
            } else {
                while (class.assign == "NA's") {
                    class.assign <- sample(names(which(
                        max.level ==
                            summary(xInit[, varIdx])
                    )), 1)
                }
                xInit[is.na(xmis[, varIdx]), varIdx] <- class.assign
            }
        }
    }

    miceImp <- mice(
        data = xmis,
        m = 1,
        method = "mf",
        maxit = maxiter,
        visitSequence = ifelse(decreasing, "revmonotone", "monotone"),
        printFlag = FALSE,
        data.init = xInit,
        num.trees = ntree,
        maxcor = 1.0,
        ...
    )
    miceCompData <- complete(data = miceImp, action = 1L)
    return(miceCompData)
}
