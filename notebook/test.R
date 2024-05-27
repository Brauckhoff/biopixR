r <- load.image("notebook/Images/sub/1_000000_40x_Cy5_Grey=1000_Z=3509370_Well=A1_Ti=149999us_t=000173547ms.bmp")
r4 <- load.image("notebook/Images/sub/1_000013_40x_Cy5_Grey=1000_Z=3537210_Well=E1_Ti=149999us_t=000315500ms.bmp")
r4 <- load.image("notebook/Images/sub/1_000020_40x_Cy5_Grey=1000_Z=3551510_Well=G1_Ti=149999us_t=000376422ms.bmp")


spe_img <- imagerExtra::SPE(r4, lamda = 0.1)
thresh <- threshold(spe_img)
coords <- as.data.frame(thresh)
threshold(r4) |> plot()
#par(mfrow = c(1,2))

whole_t <- threshold(r4)

x <- add(list(whole_t, thresh))
plot(x)
x[which(x > 0)] <- 1

labeled_components <- label(x, high_connectivity = TRUE)

df_lab <- as.data.frame(labeled_components) |> subset(value > 0)

plot(r4)
points(df_lab$x, df_lab$y, col = factor(df_lab$value))

common_rows <- merge(coords[, 1:2], df_lab[,1:3], by = c("x", "y"))

plot(r4)
points(common_rows$x, common_rows$y, col = factor(common_rows$value))

# Convert to data table for efficient data manipulation
DT <- data.table(common_rows)

# Aggregate data to calculate the mean coordinates for each unique
# label (cluster)
grouped_lab_img <-
  DT[, list(mx = mean(x), my = mean(y), size = length(x)), by = value]

resu <- sizeFilter(grouped_lab_img[,1:3], common_rows, lowerlimit = 1000, upperlimit = 3000)
res <- resultAnalytics(common_rows, common_rows, size = grouped_lab_img$size, img = r)
res$summary
res$detailed
plot(r4)
points(res$detailed$x, res$detailed$y, pch = 20, col = "green")
