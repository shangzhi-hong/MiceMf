context("Imputation using caller")

# Set-up
NUM_OBS <- 50
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(testData,
                      y = 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS))

amputedData <- ampute(
    data = testData,
    prop = 0.5,
    mech = "MCAR",
    bycases = TRUE,
    patterns = matrix(
        data = c(0, 0, 0, 1,
                 1, 0, 1, 1),
        ncol = 4,
        byrow = T
    )
)[["amp"]]

test_that("missForestMice works for continuous variables", {
    for (decreasing in c(TRUE, FALSE)) {
        for (maxiter in c(5, 10)) {
            impDf <- missForestMice(xmis = amputedData,
                                    decreasing = decreasing)

            expect_true(!anyNA(impDf))
        }
    }

})

test_that("MiceMf works for continuous variables", {
    for (decreasing in c(TRUE, FALSE)) {
        for (maxiter in c(5, 10)) {
            impDf <- MiceMf(data = amputedData,
                            decreasing = decreasing)
            expect_true(!anyNA(impDf))
        }
    }
})

test_that("MiceMfNorm works for continuous variables", {
    for (decreasing in c(TRUE, FALSE)) {
        for (maxiter in c(5, 10)) {
            impDf <- MiceMfNorm(data = amputedData,
                                decreasing = decreasing)
            expect_true(!anyNA(impDf))
        }
    }

})
