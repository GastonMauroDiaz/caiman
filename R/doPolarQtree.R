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
#' @param parallel logical. Go parallel.
#' @param freeThreads numeric. The number of threads that remain free for other
#'   tasks. Defaul \code{2} avoid the computer comes to a standstill until the R
#'   task has finished. Using \code{1} could be a good idea. Use \code{0} at
#'   your own risk. If you run out of memory, increase \code{freeThreads}.
#' @param memoryUseFactor numeric. Increase to use less memory when
#'   \code{parallel} is set to TRUE.
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
              scaleParameter, divisions = 1, mnSize = 1000, parallel = FALSE,
              freeThreads = 2)
                standardGeneric("doPolarQtree"))
#' @export doPolarQtree

#' @rdname doPolarQtree
setMethod("doPolarQtree",
  signature(x = "Raster"),
  function (x, z, a, scaleParameter, divisions, mnSize, parallel,
            freeThreads) {
    print("doPolarQtree: please wait... use parallel = TRUE to increase speed.")
#    Using parallel = TRUE you could increase speed by roughly 2 times

    stopifnot(length(divisions) == 1)
    stopifnot(any(divisions == 1, divisions == 2,
                  divisions == 3, divisions == 4))

    stopifnot(class(z)[[1]] == "ZenithImage")
    stopifnot(class(a)[[1]] == "AzimuthImage")

    if (x@fisheye@fullframe) x <- expandFullframe(x, z)

    if (!raster::compareRaster(x, z, stopiffalse = FALSE))
      stop("Maybe you not declare your hemispherical photo as a fullframe. To do it, use fisheye(x) <- newFishEye(TRUE, TRUE, TRUE), for example.")

    raster::compareRaster(z, a)

    cropFun <- function(x, cropped) {
      x <- raster::crop(x, cropped) * cropped
      x[cropped == 0] <- NA
      return(x)
    }

    recursiveSegmentation <- function(g) {
      segments <- levels(as.factor(g))[[1]][, 1]

      goRecursively <- function(x, z, a, segment, scaleParameter) {
        cropped <- g[g == segment, drop = FALSE]
        cropped <- !is.na(cropped)

        x <- cropFun(x, cropped)
        z <- cropFun(z, cropped)
        a <- cropFun(a, cropped)
        rm(cropped)

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

              segments <- Map(function(x) x, segments)
              baz <- function(segment) goRecursively(x, z, a, segment, scaleParameter)

              lapply(segments, baz)

            }
          }
        }
      }

      segments <- Map(function(x) x, segments)
      baz <- function(segment) goRecursively(x, z, a, segment, scaleParameter)

      lapply(segments, baz)

      g

    }

    angle.wds <- list(
      c(15, 7.5, 3.75, 1.875),
      c(7.5, 3.75, 1.875),
      c(7.5, 3.75),
      c(3.75)
    )
    angle.wds <- angle.wds[[divisions]]

    g <- makePolarGrid(z, a, asAngle(max(angle.wds) * 2))

    if (parallel) {
      # Calculate the number of threads
      no_threads <- parallel::detectCores() - freeThreads

      padlock <- !raster::canProcessInMemory(g, no_threads * 2)

      if (padlock) {

        rings <- makeRings(z, asAngle(30))
        sectors <- makePolarSectors(a, asAngle(60))
        polarGrid <- as.factor(sectors * 1000 + rings)

        segments <- levels(polarGrid)[[1]][, 1]
        segments <- Map(function(x) x, segments)
        # go parallel
        ## Initiate cluster
        cl <- parallel::makeCluster(no_threads)
        ges <- parallel::parLapply(cl, segments,
                                   function(x) g[polarGrid == x, drop = FALSE]
        )
        ## finish
        parallel::stopCluster(cl)
      } else {
        segments <- levels(g)[[1]][, 1]
        groupSize <- round(length(segments) / no_threads)
        starGroup <- seq(1, length(segments), groupSize)
        endGroup <- c(starGroup[-1] - 1, length(segments))

        forSubs <- Map(function(x,y) seq(x, y, 1), starGroup, endGroup)
        forSubs <- Map(function(x) data.frame(segments[x], rep(1, length(x))), forSubs)

        # go parallel
        ## Initiate cluster
        cl <- parallel::makeCluster(no_threads)
        ges <- parallel::parLapply(cl, forSubs,
                                   function(x) {
                                     cropped <- raster::subs(g, x) * g
                                     cropped <- cropped[!is.na(cropped), drop = FALSE]
                                   })
        ## finish
        parallel::stopCluster(cl)
      }


      # go parallel
      ## Initiate cluster
      cl <- parallel::makeCluster(no_threads)
      ges <- parallel::parLapply(cl, ges, function(g) recursiveSegmentation(g))

      ## finish
      parallel::stopCluster(cl)

      # to avoid segments with same id
      maxOfGes <- unlist(Map(getMax, ges))

      for (i in 2:length(maxOfGes)) {
        maxOfGes[i] <- maxOfGes[i] + maxOfGes[i - 1]
      }
      maxOfGes[1] <- 0
      ges <- Map(function(x, y) x + y, ges, maxOfGes )

      # merge
      # ges <- Map(function(x) extend(x, g), ges)
      g[] <- NA

      for (i in 1:(length(ges))) {
        g <- cover(g, extend(ges[[i]], g))
      }

      g <- as(g, "PolarSegmentation")

    } else {
      # I repeat myself because environment
      goRecursively <- function(x, z, a, segment, scaleParameter) {
        cropped <- g[g == segment, drop = FALSE]
        cropped <- !is.na(cropped)

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

              segments <- Map(function(x) x, segments)
              baz <- function(segment) goRecursively(x, z, a, segment, scaleParameter)

              lapply(segments, baz)

            }
          }
        }
      }

      segments <- levels(g)[[1]][, 1]
      pb <- raster::pbCreate(length(segments), "text")
      for (i in 1:length(segments)) {
        raster::pbStep(pb, i)
        goRecursively(x, z, a, segments[i], scaleParameter)
      }
      raster::pbClose(pb)

    }

    #g@angleWidth <- asAngle(0)
    g@scaleParameter <- scaleParameter
    rtitle(g) <- "Polar Quadtree"
    g <- as.factor(g)
    return(g)
  }
)
