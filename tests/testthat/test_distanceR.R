library(testthat)
library(beadR)

test_that("distanceR",
          {
            img <- test_img
            res_detecteR <- detecteR(img)
            res_distanceR <- distanceR(res_detecteR)

            expect_type(res_distanceR, "list")
            expect_length(res_distanceR, 4)
            expect_lt(
              length(which(is.na(res_distanceR$discard$mx == FALSE))),
              length(res_distanceR$centers$value))
            expect_error(distanceR(img))
            expect_equal(nrow(res_distanceR$centers),
                         nrow(res_distanceR$discard))
})

plot(img)
with(res_distanceR$discard, points(mx, my, col = "red"))
