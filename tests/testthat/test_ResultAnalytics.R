library(testthat)
library(biopixR)

test_that("resultAnalytics",
          {
            img <- beads
            res_sizeFilter <- img |>
              objectDetection() |>
              proximityFilter() |>
              sizeFilter()
            res_resultAnalytics <- resultAnalytics(res_sizeFilter)

            expect_equal(sum(res_resultAnalytics$detailed$Size),
                       nrow(res_sizeFilter$remaining.coordinates.s))
            expect_equal(length(res_resultAnalytics$detailed$Beadnumber),
                       length(unique(res_sizeFilter$remaining.coordinates.s$cluster)))

            expect_type(res_resultAnalytics, "list")
            expect_length(res_resultAnalytics, 2)
            expect_s3_class(res_resultAnalytics$Summary, "data.frame")
            expect_s3_class(res_resultAnalytics$detailed, "data.frame")
            expect_equal(res_resultAnalytics$Summary$Number_of_Beads,
                         length(unlist(res_sizeFilter$size)))
            expect_equal(res_resultAnalytics$Summary$Number_of_Beads,
                         nrow(res_resultAnalytics$detailed))
            expect_equal(mean(res_resultAnalytics$detailed$Size),
                         res_resultAnalytics$Summary$Mean_Size)
})
