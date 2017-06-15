#' @title todo
#'
#' @description todo
#'
#' @param x todo
#' @param ... todo
#'
#' @return todo
#' @export extractSkyBrightness
#'
#' @examples todo
setGeneric("extractSkyBrightness", function(x, ...) standardGeneric("extractSkyBrightness"))

setMethod("extractSkyBrightness",
  signature(x = "CanopyPhoto"),
  function (x, z, smooth = TRUE)
  {

    compareRaster(x, z)
    stopifnot(max(getMax(x)) <= 1)
    stopifnot(min(getMin(x)) >= 0)

    title <- rtitle(x)
    meta.data <- c(ssDenominator(x), aperture(x), isoSpeed(x))

    x11()

    plotRGB(x * 255)

    bigFoo <- c()
    foo <- 1
    print("Click on sky pixels. When you finish click on stop>stop locator.")
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
      cells <- cellFromXY(z, xy = ma)
      ma <- cbind(ma, x[cells])

      if (nrow(ma) != 0)  {
        if (nrow(ma) == 1) {
          bright <- mean(ma[, 3:5])
        } else {
          bright <- apply(ma[, 3:5], 1, mean)
        }
        ma <- data.frame(title, ma, Bright = bright, Zenith = z[cells],
                         ssDenominator = meta.data[1], aperture = meta.data[2],
                         isoSpeed = meta.data[3])
      }
      return(ma)
    } else {
      return(NULL)
    }

})


setMethod("extractSkyBrightness",
          signature(x = "data.frame"),
          function (x, z, canopyPhoto)
          {

            ma <- x[, 2:3]

            stopifnot(colnames(ma) == c("x", "y"))

            x <- canopyPhoto
            rm(canopyPhoto)

            compareRaster(x, z)
            stopifnot(max(getMax(x)) <= 1)
            stopifnot(min(getMin(x)) >= 0)

            title <- rtitle(x)
            meta.data <- c(ssDenominator(x), aperture(x), isoSpeed(x))

            cells <- cellFromXY(z, xy = ma)
            ma <- cbind(ma, x[cells])

            bright <- apply(ma[, 3:5], 1, mean)

            ma <- data.frame(title, ma[, 1:5], Bright = bright, Zenith = z[cells],
                             ssDenominator = meta.data[1], aperture = meta.data[2],
                             isoSpeed = meta.data[3])

            return(ma)
})


setMethod("extractSkyBrightness",
          signature(x = "character"),
          function (x, id, z, mn, mx, ...)
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
              x <- extractSkyBrightness(x, z)

              if (!is.null(x)) {
                for (u in seq_along(photos)) {
                  canopyPhoto <- loadPhoto( photos[u], ...)
                  canopyPhoto <- normalize(canopyPhoto, mn, mx)
                  ma <- rbind(ma, cbind(extractSkyBrightness(x, z, canopyPhoto),
                                        factor = levels(id$factor)[i]))
                }
              }

            }
            pbClose(pb)
            return(ma)

})
