library(testthat)
library(beadR)

test_that("objectDetection",
          {
            img <- test_img
            res_objectDetection <- objectDetection(img)

            expect_equal(res_objectDetection$centers$value[1], 1)
            expect_type(res_objectDetection, "list")
            expect_length(res_objectDetection, 3)
            expect_gt(length(res_objectDetection$centers$value), 15)
            expect_error(objectDetection(1:2))
})

plot(img)
with(res_objectDetection$centers, points(mxx, myy, col = "purple"))

