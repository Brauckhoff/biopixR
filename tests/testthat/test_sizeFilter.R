library(testthat)
library(biopixR)

test_that("sizeFilter",
          {
            img <- beads
            res_proximityFilter <- img |>
              objectDetection() |>
              proximityFilter()
            res_sizeFilter <- sizeFilter(res_proximityFilter)

            expect_equal(class(res_sizeFilter$remaining.coordinates.s),
                         c("data.frame"))
            expect_lte(nrow(res_sizeFilter$remaining.coordinates.s),
                      nrow(res_proximityFilter$remaining.coordinates))
            expect_gt(nrow(res_sizeFilter$remaining.coordinates.s),
                      length(res_sizeFilter$size))

            expect_equal(length(which(res_sizeFilter$remaining.coordinates.s$cluster == 1)),
                         unlist(res_sizeFilter$size[1]))
            expect_equal(max(res_sizeFilter$remaining.coordinates.s$cluster),
                         length(res_sizeFilter$size))

            expect_type(res_sizeFilter, "list")
            expect_length(res_sizeFilter, 3)
            expect_equal(length(res_sizeFilter$size), nrow(objectDetection(img)$centers))
            expect_lte(length(unlist(res_sizeFilter$size)),
                       length(unique(res_sizeFilter$remaining.coordinates.s$cluster)))
            expect_error(sizeFilter(img))
})

