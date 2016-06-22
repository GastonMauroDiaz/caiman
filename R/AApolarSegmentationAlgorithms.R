#####################
## ** makeRings ** ##
#####################

#' @title Make rings.
#'
#' @description Make rings by dividing the zenith angle from 0 to 90 in equals intervals.
#'
#' @param x \code{\linkS4class{ZenithImage}}.
#' @param angleWidth \code{\linkS4class{Angle}}. AngleWidth must divide 0-90 into a whole number of segments.
#' @param angleMean logical. If it is FALSE, all the pixels belong to a ring are labeled with an ID number. Otherwise, the angle mean of the ring are assigned to the pixels.
#'
#' @details The intervals are closed on the right and open on the left. The first ring never contain \code{0} because zenith point is always in between two pixels (\code{\link{makeZimage}}).
#'
#' @return \code{\linkS4class{PolarSegmentation}}.
#'
#' @seealso \code{\link{makeZimage}}, \code{\link{makePolarSectors}}, \code{\link{makePolarGrid}}.
#'
#' @example /inst/examples/makeRingsExample.R
#'
setGeneric("makeRings", function(x, angleWidth, angleMean = FALSE)
                                                  standardGeneric("makeRings"))
#' @export makeRings

#' @rdname makeRings
setMethod("makeRings",
  signature(x = "ZenithImage"),
  function (x, angleWidth, angleMean) {

    if (!angleWidth@degrees) angleWidth <- switchUnit(angleWidth)

    stopifnot(length(angleWidth@values) == 1)
    stopifnot(length(angleWidth@values) < 90)

    tmp <- 90/angleWidth@values
    if (round(tmp) != tmp)
      stop("angleWidth must divide 0-90 into a whole number of segments.")

    intervals <- seq(0, 90, angleWidth@values)
    c1 <- intervals[1:(length(intervals) - 1)]
    c2  <- intervals[2:length(intervals)]
    if (angleMean) {
      c3 <- (c1 + c2) / 2
    } else {
      c3 <- 1:(length(intervals) - 1)
    }
    rcl <- matrix(c(c1, c2, c3), ncol = 3)
    x <- reclassify(x, rcl)

    x <- as(x, "PolarSegmentation")
    x@angleWidth <- angleWidth
    rtitle(x) <- "Polar rings"
    x <- as.factor(x)
    return(x)
  }
)

############################
## ** makePolarSectors ** ##
############################

#' @title Slice the polar space in sectors
#'
#' @description Make sectors by slicing the azimuth angle from 0 to 360 in equals intervals.
#'
#' @param x \code{\linkS4class{AzimuthImage}}.
#' @param angleWidth \code{\linkS4class{Angle}}. AngleWidth must divide 0-360 into a whole number of segments.
#' @param angleMean logical. If it is FALSE, all the pixels belong to a sector are labeled with an ID number. Otherwise, the angle mean of the sector are assigned to the pixels.
#'
#' @details The intervals are closed on the right and open on the left. The first sector never contain \code{0} because zenith point is always in between two pixels (\code{\link{makeZimage}}), that is way a pixel never will be in the exact Norte direction.
#'
#' @return \code{\linkS4class{PolarSegmentation}}.
#'
#' @seealso \code{\link{makeAimage}}, \code{\link{makeRings}}, \code{\link{makePolarGrid}}.
#'
#' @example /inst/examples/makePolarSectorsExample.R
#'
setGeneric("makePolarSectors", function(x, angleWidth, angleMean = FALSE)
                                            standardGeneric("makePolarSectors"))
#' @export makePolarSectors

#' @rdname makePolarSectors
setMethod("makePolarSectors",
  signature(x = "AzimuthImage"),
  function (x, angleWidth, angleMean) {

    if (!angleWidth@degrees) angleWidth <- switchUnit(angleWidth)

    stopifnot(length(angleWidth@values) == 1)
    stopifnot(length(angleWidth@values) <= 360)

    tmp <- 360/angleWidth@values
    if (round(tmp) != tmp)
      stop("angleWidth must divide 0-360 into a whole number of segments.")

    intervals <- seq(0, 360, angleWidth@values)
    c1 <- intervals[1:(length(intervals) - 1)]
    c2  <- intervals[2:length(intervals)]
    if (angleMean) {
      c3 <- (c1 + c2) / 2
    } else {
      c3 <- 1:(length(intervals) - 1)
    }

    rcl <- matrix(c(c1, c2, c3), ncol = 3)
    x <- reclassify(x, rcl)

    x <- as(x, "PolarSegmentation")
    x@angleWidth <- angleWidth
    rtitle(x) <- "Polar sectors"
    x <- as.factor(x)
    return(x)

  }
)

#########################
## ** makePolarGrid ** ##
#########################

#' @title Make a grid of segments.
#'
#' @description Partitioning the hemisphere into segments of equal angular resolution for both zenith and azimuth angles.
#'
#' @param z \code{\linkS4class{ZenithImage}}.
#' @param a \code{\linkS4class{AzimuthImage}}.
#' @param angleWidth \code{\linkS4class{Angle}}. It must be 30, 15, 10, 7.5, 6, 5, 3.75, 3, 2.5, 1.875, 1 or 0.5 degrees. This values could divide both 0-360 and 0-90 into a whole number of segments.
#' @param sequential logical. If it is TRUE the segments are labeled with sequential numbers. By default (FALSE), labeling numbers are not sequential (see Details).
#'
#' @details Intersecting rings with sectors makes a grid in which each segment is a portion of the hemisphere. Each pixel of the grid is labeled with an ID that codify both ring and sector ID. For example, a grid with a regular interval of 1 degree has segment from 1001 to 360090. This numbers are calculated with: \code{sector-ID# x 1000 + rings-ID#}, where \code{sector-ID#} is the ID number of the sector and \code{rings-ID#} is the ID number of the ring.
#'
#' @return \code{\linkS4class{PolarSegmentation}}.
#'
#' @seealso \code{\link{makeZimage}}, \code{\link{makeAimage}}, \code{\link{makePolarSectors}}, \code{\link{makeRings}}.
#'
#' @example /inst/examples/makePolarGridExample.R
#'
setGeneric("makePolarGrid", function(z, a = makeAimage(z),
                angleWidth = asAngle(1), sequential = FALSE)
                  standardGeneric("makePolarGrid"))
#' @export makePolarGrid

#' @rdname makePolarGrid
setMethod("makePolarGrid",
  signature(z = "ZenithImage"),
  function (z, a, angleWidth, sequential) {

    stopifnot(class(a)[[1]] == "AzimuthImage")

    if(!angleWidth@degrees) angleWidth <- switchUnit(angleWidth)

    stopifnot(length(angleWidth@values) == 1)

    if(!max(angleWidth@values == c(30,15,10,7.5,6,5,3.75,3,2.5,1.875,1,0.5)))
      stop("angle.wd must be 30, 15, 10, 7.5, 6, 5, 3.75, 3, 2.5, 1.875, 1 or 0.5 degrees.")

    fun <- function(s, r) s * 1000 + r

    g <- overlay(
                  makePolarSectors(a, angleWidth),
                  makeRings(z, angleWidth),
                  fun = fun
                )

    if (sequential) {
      df <- levels(as.factor(g))[[1]]
      df <- cbind(df, 1:nrow(df))
      g <- raster::subs(g, df)
    }

    g <- as(g, "PolarSegmentation")
    g@angleWidth <- angleWidth
    rtitle(g) <- "Polar grid"
    g <- as.factor(g)
    return(g)

  }
)
