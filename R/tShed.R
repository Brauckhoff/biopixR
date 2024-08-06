#'
#'
#'
#' @param
#' @param
#' @param
#' @param
#' @returns list of 2 objects:
#' \itemize{
#'   \item
#'   \item
#' }
#' @details

#' @examples
# @export
tShed <- function(
    ){

}

# it should be possible - small scale example
beads |> plot()
t <- threshold(beads) |> as.data.frame()
l <- threshold(beads) |> label() |> as.data.frame() |> subset(value > 0)

dist_map <- dist(t)

k <- pam(dist_map, 15)

df <- t[,1:2]
df$cluster <- k$cluster

changePixelColor(beads, df, color = factor(df$cluster), visualize = T)

# Challenge the number of k + algorithm that can deal with high number

## Silhoutte

# Function to calculate average silhouette width for a given k
calculate_silhouette <- function(k, dist_map) {
  pam_fit <- pam(dist_map, k)
  silhouette_values <- silhouette(pam_fit)
  mean(silhouette_values[, 3]) # Column 3 is the silhouette width
}

# Test different k values
k_values <- max(unique(l$value)):18
avg_silhouettes <- sapply(k_values, calculate_silhouette, dist_map = dist_map)

# Plot the average silhouette width for each k
plot(k_values, avg_silhouettes, type = "b", xlab = "Number of clusters", ylab = "Average silhouette width")

optimal_k <- k_values[which.max(avg_silhouettes)]
cat("The optimal number of clusters is", optimal_k, "\n")


## Optimization approach

DT1 <- data.table(l)
grouped_lab_img <-
  DT1[, list(mx = mean(x),
             my = mean(y),
             size = length(x)), by = value]

plot(grouped_lab_img$size)



# Define the original objective function
gaussianHayflick <- function(x) {
  k_a <- round(x)  # Ensure k_a is an integer

  property <- tryCatch({
    t <- threshold(beads) |> as.data.frame()
    l <- threshold(beads) |> label() |> as.data.frame() |> subset(value > 0)

    if (k_a < 1 || k_a > nrow(t)) {
      stop("Invalid number of clusters")
    }

    dist_map <- dist(t)
    k <- pam(dist_map, k_a)

    df <- t[, 1:2]
    df$cluster <- k$cluster

    DT1 <- data.table(df)
    grouped_lab_img <- DT1[, .(mx = mean(x),
                               my = mean(y),
                               size = .N), by = cluster]
    grouped_lab_img
  }, error = function(e) {
    message("Error in clustering: ", e$message)
    return(NULL)
  })

  if (is.null(property)) return(Inf)  # Handle the error by returning Inf

  # Calculate a variety of statistics based on image features
  quality <- sd(property$size)
  return(quality)
}

# Define bounds for the parameter
t <- threshold(beads_large1) |> as.data.frame()
l <- threshold(beads_large1) |> label() |> as.data.frame() |> subset(value > 0)
lower_bound <- max(unique(l$value))
upper_bound <- 100


# Optimization using optimize with the wrapper function
result <- optimize(gaussianHayflick,
                   interval = c(lower_bound, upper_bound),
                   maximum = FALSE,
                   tol = 5)


optimal_k <- round(result$minimum)
cat("The optimal number of clusters is", optimal_k, "\n")
debug(gaussianHayflick)
undebug(gaussianHayflick)

dist_map <- dist(t)

k <- pam(dist_map, 15)

df <- t[,1:2]
df$cluster <- k$cluster

changePixelColor(beads, df, color = factor(df$cluster), visualize = T)

