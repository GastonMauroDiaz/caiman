#' @title Extract sky brightness in zenith vecinity.
#'
#' @description todo
#'
#' @param x todo
#' @param z todo
#' @param upperLeft todo
#' @param width todo
#' @param samplingWindow todo
#' @param ... todo
#'
#' @details todo
#'
#' @return data.frame
#' @export extractZenithBrightness
#'
#' @examples "todo"
setGeneric("extractZenithBrightness", function(x, ...) standardGeneric("extractZenithBrightness"))

setMethod("extractZenithBrightness",
  signature(x = "character"),
  function (x, z, upperLeft = NULL, width = NULL, samplingWindow = 3)
  {

    stopifnot(samplingWindow / 2 != round(samplingWindow / 2))

    z2 <- z
    z <- doMask(z, zlim = asAngle(c(0, 10)))
    z <- z[z ==1, drop = FALSE]
    z <- ncol(z)

    height <- width

    oldwd <- getwd()
    on.exit(setwd(oldwd))
    setwd(x)

    x11()

    listofpics <- list.files()

    if (!is.null(upperLeft)) {
      upperLeft2 <- upperLeft
      height2 <- width2 <- width
      upperLeft <- c(upperLeft[1] + width / 2 - round(z / 2),
                      upperLeft[2] + height/2 - round(z / 2)
                    )
      height <- width <- round(z)
    } else {
      width <- height <- nrow(x)
      upperLeft <- c(width / 2 - round(z / 2), height/2 - round(z / 2))
    }

    bigFoo <- c()

    print("Click on a pure sky pixel. If you cannot find one of it, click outside the image circle to get an NA.")

    pb <- pbCreate(length(listofpics), "text")
    j <- 0
    for (i in listofpics) {
      j <- j + 1
      pbStep(pb, j)
      x <- loadPhoto(i, upperLeft = upperLeft2, width = width2, height = height2)
      compareRaster(x, z2)

      x <- loadPhoto(i, upperLeft = upperLeft, width = width, height = height)
      x <- normalize(x, min(getMin(x)), max(getMax(x)))

      if (i == listofpics[1]) m <- doMask(makeZimage(nrow(x), lensPolyCoef()))

      visualRef <- matrix(ncol = samplingWindow, nrow = samplingWindow)
      visualRef[] <- 1
      visualRef <- raster(visualRef)
      extent(visualRef) <- extent(0, 0 + samplingWindow, 180 - samplingWindow, 180)

      x[!m] <- NA
      x[visualRef] <- 0

      plotRGB(x * 255)

      foo <- click(x, n = 1, xy = TRUE, show = FALSE)

      ma <- matrix(rep(as.numeric(foo[1:2]), samplingWindow^2), ncol = 2, byrow = TRUE)
      ma <- ma + expand.grid(0:(samplingWindow - 1), 0:(samplingWindow - 1)) - (samplingWindow - 1) / 2

      rgb <- extract(x, SpatialPoints(ma))
      foo[3:5] <-  apply(rgb, 2, mean)

      foo <- cbind(foo, ssDenominator(x), aperture(x), isoSpeed(x))

      bigFoo <- c(bigFoo, unlist(foo))
    }
    pbClose(pb)
    dev.off()

    ma <- matrix(bigFoo, byrow = TRUE, ncol = 8)
    ma[is.na(ma[,3]), ] <- NA

    bright <- apply(ma[, 3:5], 1, mean)

    df <- data.frame(listofpics, ma[, 1:5], bright, ma[, 6:8])
    names(df) <- c("file", "x", "y", "red", "green", "blue", "bright",
                      "ssDenominator", "aperture", "isoSpeed")
    return(df)
})
