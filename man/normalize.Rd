% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_base_subgroup_per_pixel.R
\name{normalize}
\alias{normalize}
\alias{normalize,Raster-method}
\alias{normalize,RasterLayer-method}
\alias{normalize,CanopyPhoto-method}
\title{Normalize data in the range \code{0} to \code{1}.}
\usage{
normalize(x, mn, mx, ...)

\S4method{normalize}{Raster}(x, mn, mx, ...)

\S4method{normalize}{RasterLayer}(x, mn, mx, ...)

\S4method{normalize}{CanopyPhoto}(x, mn, mx, ...)
}
\arguments{
\item{x}{\linkS4class{CanopyPhoto} or \code{\linkS4class{Raster}}.}

\item{mn}{One-length numeric. Minimum expected value.}

\item{mx}{One-length numeric. Maximum expected value.}

\item{...}{Additional arguments as for \code{\link[raster]{writeRaster}}.}
}
\value{
\linkS4class{CanopyPhoto} or \linkS4class{Raster}
}
\description{
Normalize data lying between mn and mx in the range \code{0} to \code{1}. Data
  greater than \code{mx} get values greater than \code{1}, proportionally. Conversely, data
  less than mn get values less than \code{0}.
}
\section{Methods (by class)}{
\itemize{
\item \code{Raster}: Result is a \linkS4class{Raster} of the same type that
\code{x}.

\item \code{CanopyPhoto}: Result is a \linkS4class{CanopyPhoto}.
}}

\examples{
x <- 0:100
summary(normalize(x, 0, 100))
summary(normalize(x, 25, 75))
summary(normalize(x, 50, 75))

x <- normalize(loadPhoto(), 0, 255)
getMin(x)
getMax(x)
x <- normalize(loadPhoto(), 100, 200)
getMin(x)
getMax(x)
}
