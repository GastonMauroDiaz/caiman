% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_base_subgroup_read_cp.R
\name{cloneSlots}
\alias{cloneSlots}
\alias{cloneSlots,CanopyPhoto,CanopyPhoto-method}
\title{Copy some slots between CanopyPhoto objects.}
\usage{
cloneSlots(from, to)

\S4method{cloneSlots}{CanopyPhoto,CanopyPhoto}(from, to)
}
\arguments{
\item{from}{\code{\linkS4class{CanopyPhoto}}.}

\item{to}{\code{\linkS4class{CanopyPhoto}}.}
}
\value{
\code{\linkS4class{CanopyPhoto}}.
}
\description{
Copy the slots equipment, fisheye, datetime, geoLocation,
  bearing, elevation, fNumber, exposureTime, isoSpeed and names
  from a \code{\linkS4class{CanopyPhoto}} to another.
}
\examples{
x <- loadPhoto()
equipment(x) <- "some string"

y <- loadPhoto()

x <- cloneSlots(x, y)
equipment(x)
}
\seealso{
\code{\link{loadPhoto}}.
}
