% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_base_subgroup_geometry.R
\name{makeZimage}
\alias{makeZimage}
\alias{makeZimage,RelativeRadiusImage,LensPolyCoef-method}
\alias{makeZimage,numeric,LensPolyCoef-method}
\title{Generate a ZenithImage.}
\usage{
makeZimage(x, y)

\S4method{makeZimage}{RelativeRadiusImage,LensPolyCoef}(x, y)

\S4method{makeZimage}{numeric,LensPolyCoef}(x, y)
}
\arguments{
\item{x}{numeric or \code{\linkS4class{RelativeRadiusImage}}.}

\item{y}{\code{\linkS4class{LensPolyCoef}}.}
}
\value{
\code{\linkS4class{ZenithImage}}.
}
\description{
Helper function to generate a \code{\linkS4class{ZenithImage}}. Compute zenith angle
  for each pixel needed to represent a circular hemispherical image with \code{180}
  degrees field of view. For definition of zenith angle see details of
  \code{\link{lensPolyCoef}}.
}
\details{
If the \code{x} argument of a call to \code{makeZimage} is numeric,
  the function internally calls to \code{\link{makeRimage}}.
}
\section{Methods (by class)}{
\itemize{
\item \code{x = RelativeRadiusImage,y = LensPolyCoef}: In certains processing chain, maybe it could help to decrease
processing time. For example, in processing photos with same resolution but
taken with different lens.

\item \code{x = numeric,y = LensPolyCoef}: It is the most frequent use. You only need to provide
the diameter in pixels of the circle that has data in the circular hemispherical
photographs you want to process.
}}

\examples{
lens <- lensPolyCoef()
z <- makeZimage(480, lens)
plot(z)
z
}
\seealso{
\code{\linkS4class{RelativeRadiusImage}}.
}
