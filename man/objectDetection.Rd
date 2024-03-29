% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objectDetection.R
\name{objectDetection}
\alias{objectDetection}
\title{Object detection}
\usage{
objectDetection(img, alpha = 1, sigma = 2)
}
\arguments{
\item{img}{image (import by \code{\link[imager]{load.image}})}

\item{alpha}{threshold adjustment factor}

\item{sigma}{smoothing}
}
\value{
list of 4 objects:
\enumerate{
\item data frame of labeled region with the central coordinates
\item all coordinates that are in labeled regions
\item size of labeled objects
\item image were object edges (purple) and detected centers (green) are colored
}
}
\description{
This function identifies objects in an image using edge detection and
labeling, gathering the coordinates and centers of the identified objects.
The edges of detected objects are then highlighted for easy recognition.
}
\examples{
res_objectDetection <- objectDetection(beads, alpha = 1, sigma = 2)
res_objectDetection$marked_beads |> plot()
}
