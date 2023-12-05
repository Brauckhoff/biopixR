library(testthat)
library(biopixR)

test_that("ResultAnalytics",
          {
            img <- beads
            res_sizeFilter <- img |>
              objectDetection() |>
              proximityFilter() |>
              sizeFilter()
            res_ResultAnalytics <- ResultAnalytics(res_sizeFilter)

            expect_equal(sum(res_ResultAnalytics$detailed$Size),
                       nrow(res_sizeFilter$remaining.coordinates.s))
            expect_equal(length(res_ResultAnalytics$detailed$Beadnumber),
                       length(unique(res_sizeFilter$remaining.coordinates.s$cluster)))

            expect_type(res_ResultAnalytics, "list")
            expect_length(res_ResultAnalytics, 2)
            expect_s3_class(res_ResultAnalytics$Summary, "data.frame")
            expect_s3_class(res_ResultAnalytics$detailed, "data.frame")
            expect_equal(res_ResultAnalytics$Summary$Number_of_Beads,
                         length(unlist(res_sizeFilter$size)))
            expect_equal(res_ResultAnalytics$Summary$Number_of_Beads,
                         nrow(res_ResultAnalytics$detailed))
            expect_equal(mean(res_ResultAnalytics$detailed$Size),
                         res_ResultAnalytics$Summary$Mean_Size)
})
