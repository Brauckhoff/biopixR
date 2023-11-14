library(imager)
library(EBImage)
library(data.table)

# note do.call

# image import
img <- load.image("2.bmp")

source("edgeR.R")

# first section: detect all beads

# edge detection with default: alpha = 0.75, sigma = 0.1
# fill detected edges and label areas
# create data frame without background
# summarize cluster and calculate center -> display & check
edge_img <- cannyEdges(img, alpha = 0.75, sigma = 0.1)
filled_img <- fillHull(edge_img)
labeled_img <- label(filled_img)
df_lab_img <- as.data.frame(labeled_img) |> 
  subset(value > 0)
DT <- data.table(df_lab_img) 
grouped_lab_img <- DT[ , .(mxx = mean(x), myy = mean(y)), by = value]

edgeR(image = img)
 
plot(img)
with(grouped_lab_img, points(mxx, myy, col = "yellow"))


# section two: distance based exclusion

# looking at every center of an labeled object
# first: check the surrounding x pixels from the current center, if there is
# another center (looking only at x coordinates) -> rectangle over hole y axis,
# with width of 2x
# second (if first true): check if the found x coordinates are in a range of x 
# pixels from the current center regarding the y coordinates -> creates second
# rectangle over hole x axis, with width of 2x
# third: only if a coordinate is in both rectangles (the x^2 around the current 
# center) it is viewed as too close and therefore discarded

res <- list()

for(i in seq_along(grouped_lab_img$value)) {
  x <- grouped_lab_img[i, ]$mxx
  y <- grouped_lab_img[i, ]$myy
  for(d in grouped_lab_img$mxx) {
    if(x == d) {
      next
    } else {
      if(x - 10 < d & x + 10 > d) {
        g <- which(grouped_lab_img$mxx == d)
        #print(g)
        h <- grouped_lab_img[g, ]$myy
        #print(h)
        for(v in h) {
          if(y - 10 < v & y + 10 > v) {
            j <- which(grouped_lab_img$myy == v & grouped_lab_img$mxx == d)
            res[j] <- c(j)
          }
        }
      }
    }
  }
}

res1 <- unlist(res)

data <- data.frame(mx = rep(NA, length(res1)), my = rep(NA, length(res1)))
#data[1,2]

for (a in res1) {
  x <- grouped_lab_img[a, ]$mxx
  y <- grouped_lab_img[a, ]$myy
  data[a, 1] <- x
  data[a, 2] <- y
}
#data

plot(img)
with(grouped_lab_img, points(mxx, myy, col="green"))
with(data, points(mx, my, col = "red"))
#with(grouped_lab_img, points(113, 136, col = "yellow"))
#grouped_lab_img[245, ]


g <- which(is.na(data$mx) == TRUE)
k <- grouped_lab_img[g, ]

#plot(t2)
#with(k, points(mxx, myy, col = "blue"))

#which(df_lab_img$value == 1)
#df_lab_img[88, ]

res2 <- list()

for (j in k$value) {
  z <- which(df_lab_img$value == j)
  res2[z] <- c(z)
}
res3 <- unlist(res2)
dfHull_t3 <- df_lab_img[c(res3), ]
#plot(t2)
#with(dfHull_t3, points(x,y, col = "cyan" ))

res4 <- list()
res5 <- list()

for (l in k$value) {
  for (ö in dfHull_t3$value) {
    if (l == ö) {
      q <- which(dfHull_t3$value == l)
      res4 <- q
      gg <- length(unlist(res4))
      if (is.null(gg) != TRUE & gg < 150 & gg > 50) {
        res5[l] <- c(gg)
      }
    }
  }
}
#length(unlist(res5))


res6 <- list()

for (ok in k$value) {
    if (is.null(res5[[ok]]) != TRUE) {
      res6[ok] <- c(ok)
  }
}


data1 <- data.frame( 
                    x = rep(NA, length()), 
                    y = rep(NA, length()),
                    intensity = rep(NA, length()),
                    Cluster = rep(NA, length()))
#sum(unlist(res5))
# dfHull_t3 enthält noch Werte welche durch größe ausgeschlossen wurden daher entstehen NAs es wird empfohlen vor der Speicherung im Datensatz in 
# listen zu speichern und zu unlisten dann in dataframe :D

res7 <- list()

res8 <- list()

res9 <- list()



for (kk in unlist(res6)) {
  pp <- which(dfHull_t3$value == kk)
  res7[pp] <- c(kk)
  res8[pp] <- dfHull_t3$x[pp]
  res9[pp] <- dfHull_t3$y[pp]
}

data2 <- data.frame(x = unlist(res8),
                    y = unlist(res9),
                    intensity = rep(NA, length(unlist(res8))),
                    Cluster = unlist(res7))

for (jj in 1:nrow(data2)) {
  xx <- data2$x[jj]
  yy <- data2$y[jj]
  int <- img[xx, yy]
  data2[jj, 3] <- c(int)
}

intense <- dplyr::group_by(data2[, c(3:4)], Cluster) %>% 
  dplyr::summarise(intensity = mean(intensity))

amazing <- data.frame(Beadnumber = unlist(res6), Size = unlist(res5), Intesity = intense[,2])

data3 <- dplyr::group_by(data2[, c(1, 2, 4)], Cluster) %>%
  dplyr::summarise(xc = mean(x), yc = mean(y))

plot(img)
with(grouped_lab_img, points(mxx, myy, col="red"))
with(data3, points(xc, yc, col = "green"))

Result <- data.frame(Number_of_Beads = length(unlist(res6)), Mean_Size = mean(unlist(res5)), Mean_intensity = mean(data2$intensity))
