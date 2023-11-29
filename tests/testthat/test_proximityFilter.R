library(testthat)
library(beadR)

test_that("proximityFilter",
          {
            img <- test_img
            res_objectDetection <- objectDetection(img)
            res_proximityFilter <- proximityFilter(res_objectDetection)

            expect_type(res_proximityFilter, "list")
            expect_length(res_proximityFilter, 4)
            expect_lt(
              length(which(is.na(res_proximityFilter$discard$mx == FALSE))),
              length(res_proximityFilter$centers$value))
            expect_error(proximityFilter(img))
            expect_equal(nrow(res_proximityFilter$centers),
                         nrow(res_proximityFilter$discard))
})

plot(img)
with(res_proximityFilter$discard, points(mx, my, col = "red"))
