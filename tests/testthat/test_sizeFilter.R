library(testthat)
library(beadR)

test_that("sizeFilter",
          {
            img <- beads
            res_proximityFilter <- img |>
              objectDetection() |>
              proximityFilter()
            res_sizeFilter <- sizeFilter(res_proximityFilter)

            expect_equal(class(res_sizeFilter$cluster),
                         c("data.table", "data.frame"))
            expect_lte(nrow(res_sizeFilter$cluster),
                      nrow(res_proximityFilter$centers))
            expect_equal(length(which(is.na(res_proximityFilter$discard$mx) == TRUE)),
                         nrow(res_sizeFilter$cluster))
            expect_gt(nrow(res_sizeFilter$coordinates),
                      nrow(res_sizeFilter$cluster))

            expect_equal(length(which(res_sizeFilter$coordinates$value == 1)),
                         unlist(res_sizeFilter$size[1]))
            expect_equal(max(res_sizeFilter$cluster$value), length(res_sizeFilter$size))

            expect_type(res_sizeFilter, "list")
            expect_length(res_sizeFilter, 4)
            expect_equal(length(res_sizeFilter$size), nrow(objectDetection(img)$centers))
            expect_lte(length(unlist(res_sizeFilter$size)), nrow(res_sizeFilter$cluster))
            expect_error(sizeFilter(img))
})

