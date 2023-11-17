library(testthat)
library(beadR)

test_that("visualizeR",
          {
            img <- test_img
            res_visualizeR <- img |>
              detecteR() |>
              distanceR() |>
              sizeR() |>
              visualizeR()

            expect_type(res_visualizeR, "list")
            expect_length(res_visualizeR, 2)
            expect_s3_class(res_visualizeR$Summary, "data.frame")
            expect_equal(res_visualizeR$Summary$Number_of_Beads,
                         length(unlist(
                           sizeR(distanceR(detecteR(img)))$size)))
            expect_equal(res_visualizeR$Summary$Number_of_Beads,
                         nrow(res_visualizeR$detailed))

})

plot(img)
with(res_visualizeR$detailed, text(x, y, labels = Beadnumber))
