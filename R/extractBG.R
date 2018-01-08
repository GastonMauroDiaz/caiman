.getDataFrame <- function(x, ma, brightness, cells, z, a) {
  title <- rtitle(x)

  meta.data <- c(ssDenominator(x), aperture(x), isoSpeed(x))

  ma <- data.frame(title, ma, Brightness = brightness,
                   ssDenominator = meta.data[1], aperture = meta.data[2],
                   isoSpeed = meta.data[3], hour = getHour(datetime(x)))
  if (!is.null(z)) ma <- data.frame(ma, Zenith = z[cells])
  if (!is.null(a)) ma <- data.frame(ma, Azimuth = a[cells])
  ma
}


#' @title Assist the user to manually select pixels that only represent background
#'
#' @description Assist the user to manually select pixels that only represent background
#'
#' @param x \code{\linkS4class{CanopyPhoto}}, data.frame or character.
#' @param z \code{\linkS4class{ZenithImage}}. Optional.
#' @param a \code{\linkS4class{AzimuthImage}}. Optional.
#' @param canopyPhoto \code{\link{CanopyPhoto}}.
#' @param id Two columns data.frame. The first column is the name of the
#'   photographs (extension included) and must be named \emph{photo}, the second
#'   column is a factor and must be named \code{factor}.
#' @param mn numeric. Minimum expected value (see \code{\link{normalize}}).
#' @param mx numeric. Maximum expected value (see \code{\link{normalize}}).
#' @param ... Additional arguments as for \code{\link{loadPhoto}}:
#' \code{upperLeft}, \code{width} and \code{height}.
#'
#' @return data.frame
#' @export extractBG
#'
#' @example /inst/examples/extractBGExample.R
setGeneric("extractBG", function(x, ...) standardGeneric("extractBG"))

#' @describeIn extractBG Easy to use and useful if only one shot was
#'   done per site.
setMethod("extractBG",
  signature(x = "CanopyPhoto"),
  function (x, z = NULL, a = NULL)
  {

    if (!is.null(z)) compareRaster(x, z)
    if (!is.null(a)) compareRaster(x, a)

    stopifnot(max(getMax(x)) <= 1)
    stopifnot(min(getMin(x)) >= 0)

    x11()

    plotRGB(x * 255)

    bigFoo <- c()
    foo <- 1
    print("Click on background pixels. When you finish click on stop>stop locator.")
    while(!is.null(foo)) {
      foo <- click(x, xy = TRUE, n = 1, show = FALSE)
      if (!is.null(foo)) {
        plot(SpatialPoints(foo[1:2]), add = TRUE, col = "red")
        bigFoo <- c(bigFoo, as.numeric(foo[1:2]))
      }
    }

    dev.off()

    if (!is.null(bigFoo)) {
      ma <- matrix(bigFoo, ncol = 2, byrow = TRUE)
      colnames(ma) <- c("x", "y")
      cells <- cellFromXY(x, xy = ma)
      ma <- cbind(ma, x[cells])

      if (nrow(ma) != 0)  {
        if (nrow(ma) == 1) {
          brightness <- mean(ma[, 3:5])
        } else {
          brightness <- apply(ma[, 3:5], 1, mean)
        }

        ma <- .getDataFrame(x, ma, brightness, cells, z, a)

      }
      return(ma)
    } else {
      return(NULL)
    }

})

#' @describeIn extractBG Easy to use and useful if you want to
#'   extract the information of one photograph (\code{canopyPhoto}) with the
#'   point that you manually selected on another (this makes sense if is the
#'   same site and frame orientation). \code{x} is the data.frame produced using
#'   \code{extractBG} and a given photograph.
setMethod("extractBG",
          signature(x = "data.frame"),
          function (x, z = NULL, canopyPhoto, a = NULL)
          {

            ma <- x[, 2:3]

            stopifnot(colnames(ma) == c("x", "y"))

            x <- canopyPhoto
            rm(canopyPhoto)

            if (!is.null(z)) compareRaster(x, z)
            if (!is.null(a)) compareRaster(x, a)

            stopifnot(max(getMax(x)) <= 1)
            stopifnot(min(getMin(x)) >= 0)

            cells <- cellFromXY(x, xy = ma)
            ma <- cbind(ma, x[cells])

            brightness <- apply(ma[, 3:5], 1, mean)

            ma <- .getDataFrame(x, ma, brightness, cells, z, a)

            return(ma)
})

#' @describeIn extractBG s somewhat difficult to use but powerful. If
#'   you taken photos in different sites and several pictures per site, is
#'   probably your best option. \code{x} is the path to the folder that
#'   contains all the photographs of the session.
setMethod("extractBG",
          signature(x = "character"),
          function (x, id, z = NULL, mn, mx, a = NULL, ...)
          {
            old.wd <- getwd()
            on.exit(setwd(old.wd))
            setwd(x)

            stopifnot(class(id$factor) == "factor")

            ma <- c()

            pb <- pbCreate(length(levels(id$factor)), "text")

            for (i in seq_along(levels(id$factor))) {

              pbStep(pb, i)

              index <- match(id$factor, levels(id$factor)[i])
              index <- !is.na(index)
              photos <- as.character(id$photo[index])

              ees <- numeric(length(photos))
              for (u in seq_along(photos)) {
                x <- loadPhoto(photos[u], ...)
                ees[u] <- calcExposure(ssDenominator(x), aperture(x))
              }

              x <- loadPhoto(photos[which.max(ees)], ...)
              x <- normalize(x, mn, mx)

              x <- extractBG(x, z, a)

              if (!is.null(x)) {
                for (u in seq_along(photos)) {
                  canopyPhoto <- loadPhoto( photos[u], ...)
                  canopyPhoto <- normalize(canopyPhoto, mn, mx)
                  ma <- rbind(ma, cbind(extractBG(x, z, canopyPhoto, a),
                                        factor = levels(id$factor)[i]))
                }
              }

            }
            pbClose(pb)
            return(ma)

})
