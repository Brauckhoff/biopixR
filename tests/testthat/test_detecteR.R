library(testthat)
library(beadR)

test_that("detecteR",
          {
            img <- test
            res_detecteR <- detecteR(img)
            expect_equal(detecteR(img)$centers$value[1], 1)
            expect_type(detecteR(img), "list")
            expect_gt(length(detecteR(img)$centers$value), 10)
            expect_error(detecteR(1:2))
})
