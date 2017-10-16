#' @title Quad-tree segmentation in a polar space.
#'
#' @description The quad-tree segmentation algorithm is a top-down process that
#'   makes recursive divisions in four equal parts until a condition is
#'   satisfied and then stops locally. The usual implementation of the quad-tree
#'   algorithm is based on the raster structure and this is why the result are
#'   squares of different sizes. This method implements the quad-tree
#'   segmentation in a polar space.
#'
#' @param x \code{\linkS4class{Raster}}. The raster to be processed. Single or
#'   multi-layer.
#' @param z \code{\linkS4class{ZenithImage}}.  Should match the lens geometry of
#'   the picture linked with \code{x}.
#' @param a \code{\linkS4class{AzimuthImage}}.
#' @param scaleParameter one-length numeric. It is part of the stopping
#'   condition (see Detalis).
#' @param divisions numeric. See details
#' @param mnSize numeric.  This is an additional stopping criterion to avoid
#'   errors in segments closed to zenith.
#'
#' @details
#'   Argument division could be 1, 2, 3 or 4 and it controls segment resolution.
#'
#'   value / resolution (degrees)
#'
#'   1 / 30, 15, 7.5, 3.75 or 1.875.
#'
#'   2 / 15, 7.5, 3.75 or 1.875.
#'
#'   3 / 15, 7.5 or 3.75.
#'
#'   4 / 7.5 or 3.75.
#'
#'   The algorithm starts with segments of a given resolution depending
#'   on \code{divisions}. Next, it selects one segment and splits it into four
#'   segments of equal angular resolution. Then, it uses the standard deviation
#'   of \code{x} as homogeneous criterion. To that end, it calculates the standard
#'   deviation for the entire segment and for each four segments. To stop the
#'   process locally, the algorithm evaluates if the sum of the standard
#'   deviation of the subsegments minus the standard deviation of the segment
#'   (delta) is less or equal than the \code{scaleParameter}. If x is multilayer delta
#'   is calculated separately and delta mean is used to evaluate the
#'   stopping condition.
#'
#' @references Diaz, G.M., Lencinas, J.D., 2015. Enhanced Gap Fraction
#' Extraction From Hemispherical Photography. IEEE Geosci. Remote Sens. Lett.
#' 12, 1784-1789.
#'
#' @return \linkS4class{PolarSegmentation}.
#'
#' @seealso \code{\link{makeZimage}}, \code{\link{makeAimage}}.
#'
#' @example /inst/examples/doPolarQtreeExample.R
#'
setGeneric("doPolarQtree", function(x, z, a = makeAimage(z),
              scaleParameter, divisions = 1, mnSize = 1000) standardGeneric("doPolarQtree"))
#' @export doPolarQtree

#' @rdname doPolarQtree
setMethod("doPolarQtree",
  signature(x = "Raster"),
  function (x, z, a, scaleParameter, divisions, mnSize) {

    stopifnot(length(divisions) == 1)
    stopifnot(any(divisions == 1, divisions == 2,
                  divisions == 3, divisions == 4))

    stopifnot(class(z)[[1]] == "ZenithImage")
    stopifnot(class(a)[[1]] == "AzimuthImage")

    if (x@fisheye@fullframe) x <- expandFullframe(x, z)

    if (!raster::compareRaster(x, z, stopiffalse = FALSE))
      stop("Maybe you not declare your hemispherical photo as a fullframe. To do it, use fisheye(x) <- newFishEye(TRUE, TRUE, TRUE), for example.")

    raster::compareRaster(z, a)


    if( divisions == 1)
      angle.wds <- c(15, 7.5, 3.75, 1.875)
    if(divisions == 2)
      angle.wds <- c(7.5, 3.75, 1.875)
    if(divisions == 3)
      angle.wds <- c(7.5, 3.75)
    if(divisions == 4)
      angle.wds <- c(3.75)

    g <- makePolarGrid(z, a, asAngle(max(angle.wds) * 2))

    segments <- levels(g)[[1]][, 1]

    fun <- function(x, z, a, segment, scaleParameter) {
      cropped <- g[g == segment, drop = FALSE]
      cropped <- !is.na(cropped)

      cropFun <- function(x, cropped) {
        x <- crop(x, cropped) * cropped
        x[cropped == 0] <- NA
        return(x)
      }

      x <- cropFun(x, cropped)
      z <- cropFun(z, cropped)
      a <- cropFun(a, cropped)

      angleResolution <- (getMax(z) - getMin(z) + getMax(a) - getMin(a)) / 2

      if (angleResolution > min(angle.wds)) {

        angle.wd <- angle.wds[which.min(abs(angle.wds - rep(angleResolution / 2,
                                                          length(angle.wds))))]
        angle.wd <- asAngle(angle.wd)

        g2 <- makePolarGrid(as(z, "ZenithImage"),
                                                as(a, "AzimuthImage"), angle.wd)

        segments2 <- levels(g2)[[1]][, 1]

        if (length(segments2) > 4) stop("Please, report error code qt01")
        if (length(segments2) < 4) {
          delta <- scaleParameter - 1
          } else {

            ##

            hfun <- function(x) stats::sd(x, na.rm = TRUE)

            if (any(class(x)[1] == "RasterStack", class(x)[1] == "RasterBrick")) {
              delta <- c()
              for (l in 1:nlayers(x)) {
                sdIfSplit <- sum(extractFeatures(raster::subset(x, l), g2, hfun))
                sdNow <- hfun(values(raster::subset(x, l)))
                delta[l] <- sdIfSplit - sdNow
              }

              delta <- sum(delta) / nlayers(x)

            } else {
              sdIfSplit <- sum(extractFeatures(x, g2, hfun))
              sdNow <- hfun(values(x))
              delta <- sdIfSplit - sdNow
            }

            ##
          }

        if (delta > scaleParameter) {

          if (all(nrow(levels(g2)[[1]]) == 4, raster::ncell(g2) > mnSize)) {

            segments <- 1:4 + getMax(g)
            df <- data.frame(levels(g2), 1:4)
            g2 <- raster::subs(g2, df)
            index <- !is.na(values(g2))
            g[g == segment] <<- values(g2)[index] + getMax(g)

            fun(x, z, a, segments[1], scaleParameter)
            fun(x, z, a, segments[2], scaleParameter)
            fun(x, z, a, segments[3], scaleParameter)
            fun(x, z, a, segments[4], scaleParameter)
          }
        }
      }
    }

     pb <- pbCreate(length(segments), "text")
     for (i in 1:length(segments)) {
     #for (i in 1) {
       pbStep(pb, i)
       fun(x, z, a, segments[i], scaleParameter)
     }
     pbClose(pb)

  #g@angleWidth <- asAngle(0)
  g@scaleParameter <- scaleParameter
  rtitle(g) <- "Polar Quadtree"
  return(g)
  }
)
