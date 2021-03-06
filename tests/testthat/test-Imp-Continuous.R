context("Imputation for continuous variables")

# Set-up
NUM_OBS <- 50
testData <- data.frame(
    x1 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x2 = rnorm(NUM_OBS, mean = 2, sd = 1),
    x3 = rnorm(NUM_OBS, mean = 2, sd = 1)
)

testData <- transform(testData,
                      y = 2 + 2 * x1 + 3 * x2 + 4 * x3 + rnorm(NUM_OBS))

y <- testData[["y"]]
x <- subset(testData, select = -y)
ry <- sample(
    x = c(TRUE, FALSE),
    size = NUM_OBS,
    replace = TRUE,
    prob = c(0.5, 0.5)
)
wy <- !ry
num.trees.continuous <- 10

test_that("mf.continuous works for continuous variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
        for (normal.error.continuous in c(TRUE, FALSE)) {
            numImpOut1 <- mice.impute.mf.continuous(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.continuous = num.trees.continuous,
                normal.error.continuous = normal.error.continuous,
                num.threads = 1
            )

            numImpOut2 <- mice.impute.mf.continuous(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.continuous = num.trees.continuous,
                normal.error.continuous = normal.error.continuous,
                num.threads = 1
            )
            expect_true(all(c(
                is.numeric(numImpOut1),
                is.numeric(numImpOut2)
            )))
            expect_true(length(numImpOut1) == sum(wy))
            expect_true(length(numImpOut2) == sum(wy))
            expect_true(!anyNA(numImpOut1) &&
                            !anyNA(numImpOut2))
            if (normal.error.continuous)
                expect_true(!all(numImpOut1 == numImpOut2))


        }
    }
})

test_that("mf caller works for continuous variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
            numImpOut1 <- mice.impute.mf(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.continuous = num.trees.continuous,
                num.threads = 1
            )
            numImpOut2 <- mice.impute.mf(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.continuous = num.trees.continuous,
                num.threads = 1
            )

            expect_true(all(c(
                is.numeric(numImpOut1),
                is.numeric(numImpOut2)
            )))
            expect_true(length(numImpOut1) == sum(wy))
            expect_true(length(numImpOut2) == sum(wy))
            expect_true(!anyNA(numImpOut1) && !anyNA(numImpOut2))
    }
})

test_that("mf.norm caller works for continuous variables", {
    for (pre.bootstrap in c(TRUE, FALSE)) {
            numImpOut1 <- mice.impute.mf.norm(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.continuous = num.trees.continuous,
                num.threads = 1
            )
            numImpOut2 <- mice.impute.mf.norm(
                y = y,
                ry = ry,
                x = x,
                wy = wy,
                num.trees.continuous = num.trees.continuous,
                num.threads = 1
            )
            expect_true(all(c(
                is.numeric(numImpOut1),
                is.numeric(numImpOut2)
            )))
            expect_true(length(numImpOut1) == sum(wy))
            expect_true(length(numImpOut2) == sum(wy))
            expect_true(!anyNA(numImpOut1) &&
                            !anyNA(numImpOut2))
            expect_true(!all(numImpOut1 == numImpOut2))
    }
})
