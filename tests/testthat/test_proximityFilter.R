library(testthat)
library(biopixR)

test_that("proximityFilter",
          {
            img <- beads
            res_objectDetection <- objectDetection(img)
            res_proximityFilter <- proximityFilter(res_objectDetection)

            expect_type(res_objectDetection, "list")
            expect_equal(seq_along(res_objectDetection$centers$value),
                         1:nrow(res_objectDetection$centers))
            expect_lt(
              length(which(is.na(res_proximityFilter$discard$mx == FALSE))),
              length(res_proximityFilter$centers$value))
            expect_error(proximityFilter(img))
            expect_equal(nrow(res_proximityFilter$centers),
                         nrow(res_proximityFilter$discard))
            expect_type(res_proximityFilter, "list")
            expect_length(res_proximityFilter, 4)


            mat <- matrix(0, 8, 8)
            mat[3, 5] <- 1
            mat[5, 2] <- 1
            mat[6, 7] <- 1
            mat[7, 3] <- 1
            sim_img <- as.cimg(mat)
            centers <- data.frame(
              mxx = c(3, 5, 6, 7),
              myy = c(5, 2, 7, 3),
              value = c(1:4)
            )
            objects <- list(centers = centers)
            res <- proximityFilter(objects, radius = 3)
            discard <- na.omit(res$discard)


            # example for x - radius < d & x + radius > d
            # d = another x
            expect_true(5 - 3 < 7 & 5 + 3 > 7)

            # example for y - radius < v & y + radius > v
            # v = another y
            expect_true(2 - 3 < 3 & 2 + 3 > 3)

            expect_false(3 - 3 < 5 & 3 + 3 > 5
                         &
                           5 - 3 < 2 & 5 + 3 > 2)

            expect_equal(discard[1, 1], 5)
            expect_equal(discard[1, 2], 2)
})
