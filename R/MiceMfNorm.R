#' MiceMfNorm: Single imputation using random forest algorithm
#'
#' @param data Data set containing missing values
#'
#' @param num.trees Number of trees to build, default to \code{10}
#'
#' @param maxit Number of iterations, default to \code{5}
#'
#' @return Imputed data set
#' @export
#'
#' @examples
#' impDf <- MiceMfNorm(nhanes)
MiceMfNorm <- function(data, num.trees = 10, maxit = 5, ...) {
    miceImp <- mice(
        data = data,
        m = 1,
        method = "mf.norm",
        maxit = maxit,
        printFlag = FALSE,
        num.trees = num.trees,
        maxcor = 1.0,
        eps = 0,
        ...
    )
    miceCompData <- complete(data = miceImp, action = 1L)
    return(miceCompData)
}
