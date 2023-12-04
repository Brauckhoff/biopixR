library(testthat)
library(biopixR)

test_that("fillLineGaps",
          {
            closed_gaps <- fillLineGaps(droplets)

            expect_equal(class(closed_gaps)[1], "cimg")
            expect_equal(dim(droplets), dim(closed_gaps))


            lab_o <- label(droplets)
            df_lab_o <- as.data.frame(lab_o) |>
              subset(value > 0)
            lab <- label(closed_gaps)
            df_lab <- as.data.frame(lab) |>
              subset(value > 0)
            expect_gt(length(unique(df_lab_o$value)), length(unique(df_lab$value)))

          })
