library(testthat)
library(beadR)

test_that("detecteR",
          {
            img <- test_img
            res_detecteR <- detecteR(img)

            expect_equal(res_detecteR$centers$value[1], 1)
            expect_type(res_detecteR, "list")
            expect_length(res_detecteR, 3)
            expect_gt(length(res_detecteR$centers$value), 15)
            expect_error(detecteR(1:2))
})

plot(img)
with(res_detecteR$centers, points(mxx, myy, col = "purple"))

