library(testthat)
library(beadR)

test_that("ResultAnalytics",
          {
            img <- test_img
            res_ResultAnalytics <- img |>
              objectDetection() |>
              proximityFilter() |>
              sizeFilter() |>
              ResultAnalytics()

            expect_type(res_ResultAnalytics, "list")
            expect_length(res_ResultAnalytics, 2)
            expect_s3_class(res_ResultAnalytics$Summary, "data.frame")
            expect_equal(res_ResultAnalytics$Summary$Number_of_Beads,
                         length(unlist(
                           sizeFilter(proximityFilter(objectDetection(img)))$size)))
            expect_equal(res_ResultAnalytics$Summary$Number_of_Beads,
                         nrow(res_ResultAnalytics$detailed))

})

plot(img)
with(res_ResultAnalytics$detailed, text(x, y, labels = Beadnumber))
