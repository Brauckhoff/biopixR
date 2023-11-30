library(testthat)
library(biopixR)

test_that("proximityFilter",
          {
            img <- beads
            res_objectDetection <- objectDetection(img)
            res_proximityFilter <- proximityFilter(res_objectDetection)

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



})

plot(img)
with(res_proximityFilter$discard, points(mx, my, col = "red"))
