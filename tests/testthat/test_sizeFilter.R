library(testthat)
library(beadR)

test_that("sizeFilter",
          {
            img <- test_img
            res_sizeFilter <- img |>
              objectDetection() |>
              proximityFilter() |>
              sizeFilter()

            expect_type(res_sizeFilter, "list")
            expect_length(res_sizeFilter, 4)
            expect_equal(length(res_sizeFilter$size), nrow(objectDetection(img)$centers))
            expect_lte(length(unlist(res_sizeFilter$size)), nrow(res_sizeFilter$cluster))
            expect_error(sizeFilter(img))
})

