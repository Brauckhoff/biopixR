library(testthat)
library(beadR)

test_that("sizeR",
          {
            img <- test_img
            res_sizeR <- img |>
              detecteR() |>
              distanceR() |>
              sizeR()

            expect_type(res_sizeR, "list")
            expect_length(res_sizeR, 4)
            expect_equal(length(res_sizeR$size), nrow(detecteR(img)$centers))
            expect_lte(length(unlist(res_sizeR$size)), nrow(res_sizeR$cluster))
            expect_error(sizeR(img))
})

