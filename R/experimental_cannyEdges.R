t_fillInit <- function(strong)
{
  lab <- label(strong, TRUE) * strong
  first <- as.data.frame(lab) |> subset(value > 0)
  DT <- data.table(first)
  grouped_first <-
    DT[, .(x = x[1], y = y[1]), by = value]

}

#Starts a fill at each successive location, and accumulates the results
t_rescueFill <- function(strong,weak)
{
  v <- strong
  v[weak==1] <- .9
  loc <- t_fillInit(strong)
  df <- data.table(x = loc$x,
                   y = loc$y)
  out <- v
  for (r in 1:nrow(df)) {
    out <- bucketfill(out,df$x[r],df$y[r],color=1,sigma=.1,high_connexity=TRUE)

  }
  as.cimg(out==1)
}


t_nonmax <- function(gr)
{
  mag <- with(gr,sqrt(x^2+y^2))
  grs <- list(x=gr$x/mag,y=gr$y/mag)
  X <- Xc(gr$x)
  Y <- Yc(gr$y)
  val.bwd <- interp(mag,data.frame(x=as.vector(X-grs$x),
                                   y=as.vector(Y -grs$y)))
  val.fwd <- interp(mag,data.frame(x=as.vector(X+grs$x),
                                   y=as.vector(Y+grs$y)))

  throw <- (mag < val.bwd) | (mag < val.fwd)
  mag[throw] <- 0
  mag
}

t_guess.kmeans <- function(x)
{
  out <- kmeans(as.vector(x),centers=c(min(x),mean(x),max(x)))
  list(t1=max(x[out$cluster==1]),t2=max(x[out$cluster==2]))
}



t_cannyEdges <- function(im,t1,t2,alpha=1,sigma=2)
{
  has.col <- spectrum(im) > 1
  if (has.col)
  {
    warning("Running Canny detector on luminance channel")
    im <- grayscale(im)
  }
  if (depth(im) > 1)
  {
    stop("Videos not supported, run the function on single frames")
  }
  mag <- isoblur(im,sigma) |> imgradient("xy") |> t_nonmax()
  if (missing(t1))
  {
    guess <- t_guess.kmeans(mag)
    t2 <- alpha*guess$t2
    t1 <- alpha*guess$t1
  }
  strong <- as.cimg(mag>t2)
  weak <- as.cimg(mag %inr% c(t1,t2))
  out <- t_rescueFill(strong,weak)

  if (has.col) out <- add.colour(out)
  attr(out,"thresholds") <- c(t1,t2)
  as.pixset(out)
}

