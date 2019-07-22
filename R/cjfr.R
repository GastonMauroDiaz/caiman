# .xyzImageToRasterLayer <- function(x) {
#   xy <- expand.grid(x$x, x$y)
#   r <- raster(nrows=length(x$y), ncols=length(x$x),
#               xmn=min(x$x), xmx=max(x$x), ymn=min(x$y), ymx=max(x$y), crs=NA)
#   cell <- cellFromXY(r, xy)
#   r[cell] <- as.numeric(x$z)
#   return(r)
# }
# .smoothRaster <- function(x, theta) {
#   xy <- data.frame(xyFromCell(x, seq(length=ncell(x))))
#   v <- getValues(x)
#   out <- fields::as.image(v, x=xy, nrow=ncol(x), ncol=nrow(x), na.rm=FALSE)
#   out <- fields::image.smooth(out, theta=theta)
#   out <- .xyzImageToRasterLayer(out)
#   projection(out) <- projection(x)
#   extent(out) <- extent(x)
#   return(out)
# }

##### extractBgb ####
#' @title Extract background brightness
#'
#' @description Extract the background brightness of the photographs of the
#'   canopy models. See reference.
#'
#' @param x data.frame. The return of a call to \code{\link{findOTRs}}.
#' @param path character. The path to the photographs of the canopy models. See
#'   \code{\link{getReady4ECM}}.
#'
#' @references todo
#'
#' @return \code{\linkS4class{BinImage}}
#' @export extractBgb
#'
#' @examples #todo
setGeneric("extractBgb",
           function(x, path)
             standardGeneric("extractBgb"))

setMethod("extractBgb",
          signature(x = "data.frame"),
            function(x, path) {
              photo <- dir(path, pattern = "JPG",full.names = FALSE)
              files <- dir(path, pattern = "JPG",full.names = TRUE)

              fileNames <- lapply(strsplit(photo, "_"), function(x) x[length(x)])
              fileNames <- unlist(fileNames)

              index <- match(fileNames, x$fileName)
              thr <- x$thr[index]

              brightness <- list()
              pb <- raster::pbCreate(length(files), "text")
              for (i in seq_along(files)) {
                raster::pbStep(pb, i)
                r <- loadPhoto(files[i], upperLeft = c(534, 242), width = 1490, height = 1490)
                if (i == i) {
                  z <- makeZimage(nrow(r), lensPolyCoef())
                  m <- doMask(z, zlim = asAngle(c(0,60)))
                }
                r <- raster::calc(r, mean)

                bin <- presetThr(r, thr[i])
                bin[!m] <- NA
                brightness[[i]] <- median(r[bin == 1])
              }
              raster::pbClose(pb)

              data.frame(fileName = x$fileName[index], REV = x$REV[index],
                         refData = x$refData[index],
                         Brightness = unlist(brightness), thr = thr)

          }
)


##### getBgbfun ####
#' @title Get a function to estimate the background brightness
#'
#' @description Wrapper function for \code{bbmle::mle2} to make easier the estimation
#'   of a function to estimate the background brightness.
#'
#' @param x data.frame. The return of a call to \code{\link{extractBgb}}.
#' @param method character. See \code{\link[bbmle]{mle2}}
#' @param REVmn numeric.
#' @param REVmx numeric.
#'
#' @references todo
#'
#' @return list
#' @export getBgbfun
#'
#' @examples #TODO
setGeneric("getBgbfun",
           function(x, method = "BFGS", REVmn = NULL, REVmx = NULL)
             standardGeneric("getBgbfun"))

#' @rdname getBgbfun
setMethod("getBgbfun",
          signature(x = "data.frame"),
          function(x, method, REVmn, REVmx) {

            x <- data.frame(REV = sb$REV, Brightness = sb$Brightness)

            index <- 1:length(x$REV)
            if (!is.null(REVmn)) index <- x$REV >= REVmn
            if (!is.null(REVmx)) index <- x$REV <= REVmx
            if (!is.null(REVmn) & !is.null(REVmx))
              index <- (x$REV >= REVmn) & (x$REV <= REVmx)
            x <- x[index, ]

            fitIt <- function(x, method) {
              flog <- function(a, b, c, S) {
                media <- a / (1 + exp((-x$REV + b) / c))
                - sum(dnorm(x$Brightness, mean = media, sd = exp(S),log = TRUE))
              }
              mm <-  bbmle::mle2(flog, list(a = 216, b = 1.422, c = 1.16, S = 10),
                                 data = x, method = method)

              a <-  unname(mm@coef[1])
              b <-  unname(mm@coef[2])
              c <-  unname(mm@coef[3])

              fun <- function(x) a / (1 + exp((-x + b) / c))

              plot(x$REV, x$Brightness, xlab = "REV", ylab = "Brightness")
              curve(fun, add = TRUE, col = 2)

              list(mm, fun)
            }
            fitIt(x, method)
          }
)


##### bgModel ####
#' @title Background modelling
#'
#' @description Backgroung modelling.
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param z \code{\linkS4class{ZenithImage}}.
#' @param a \code{\linkS4class{AzimuthImage}}.
#' @param bin \code{\linkS4class{BinImage}}.
#' @param filling \code{\linkS4class{RasterLayer}}.
#' @param prob numeric. Probability for \code{\link[stats]{quantile}}.
#' @param erode logical.
#' @param ZA logical.
#' @param parallel logical.
#' @param freeThreads logical.
#'
#' @return \code{\linkS4class{BinImage}}
#' @export bgModel
#'
#' @examples #todo

#' @rdname bgModel
setGeneric("bgModel",
           function(x, z, a, bin, filling = NULL, prob = 0.95,
                    erode = TRUE, ZA = TRUE, parallel = TRUE,
                    freeThreads = 2)
              standardGeneric("bgModel"))

setMethod("bgModel",
          signature(x = "CanopyPhoto"),
          function (x, z, a, bin, filling, prob, erode, ZA, parallel, freeThreads) {

            stopifnot(class(bin) == "BinImage")
            if (!is.null(filling)) compareRaster(bin, filling)
            compareRaster(bin, x)
            compareRaster(z, x)
            compareRaster(z, a)

            fun <- function(x, ...) quantile(x, prob, na.rm = TRUE)

            if (erode) bin[] <- raster(EBImage::erode(as.matrix(bin)))[]

            blue <- raster::subset(x, "Blue")
            #blue <- raster::focal(blue, w=matrix(1/9, nc=3, nr=3))
            blue[!bin] <- NA

            g <- makePolarGrid(z, a, asAngle(5))
            if (!is.null(filling)) {
              Blue <- getFeatureImage(blue, g, fun)
              .findBias <- function(x, y) {
                m <- doMask(z, zlim = asAngle(c(30, 60)))
                mean(x[m], na.rm = TRUE) - mean(y[m], na.rm = TRUE)
              }
              bias <- .findBias(filling, Blue)
              Blue <- cover(Blue, filling - bias)
            }

            Blue <- extractFeatures(blue, g, fun)

            Zenith <- as.numeric(substr(names(Blue), 4, 5)) * 5 - 5/2
            Azimuth <- trunc(as.numeric(names(Blue)) / 1000) * 5 - 5/2

            # Filter out saturated
            index <- Blue < 250
            Blue <- Blue[index]
            Zenith <- Zenith[index]
            Azimuth <- Azimuth[index]

            # Filter out NA
            index <- !is.na(Blue)
            Blue <- Blue[index]
            Zenith <- Zenith[index]
            Azimuth <- Azimuth[index]

            if (length(Blue) > 30) {
              if (ZA) {
                model <-  lm(Blue ~ poly(Zenith, 2, raw = TRUE) +
                               sin(Azimuth * pi / 180) + cos(Azimuth * pi / 180))
                skyFun <- function(z, azimuth) {
                  x <- coef(model)
                  x[is.na(x)] <- 0
                  for(i in 1:5) assign(letters[i], x[i])
                  a + b * z + c * z^2 +
                    d * sin(azimuth * pi / 180) + e * cos(azimuth * pi / 180)
                }
              } else {
                model <-  lm(Blue ~ poly(Zenith, 2, raw = TRUE))
                skyFun <- function(z, azimuth) {
                  x <- coef(model)
                  x[is.na(x)] <- 0
                  for(i in 1:5) assign(letters[i], x[i])
                  a + b * z + c * z^2
                }
              }

              if (parallel) {
                # go parallel
                no_threads <- parallel::detectCores() - freeThreads

                bs <- blockSize(z, round(ncell(z) / no_threads))

                Values <- list()
                for (u in 1:bs$n) Values[[u]] <- data.frame(z = getValues(z, row=bs$row[u], nrows=bs$nrows[u]),
                                                            azimuth = getValues(a, row=bs$row[u], nrows=bs$nrows[u]))

                ## Initiate cluster
                cl <- parallel::makeCluster(no_threads)
                parallel::clusterExport(cl, c("skyFun", "model"), environment())
                out <- parallel::parLapply(cl, Values, function(x) skyFun(x$z, x$azimuth))

                ## finish
                parallel::stopCluster(cl)
              } else {

                Values <- list()
                for (u in 1:bs$n) Values[[u]] <- data.frame(z = getValues(z, row=bs$row[u], nrows=bs$nrows[u]),
                                                            azimuth = getValues(a, row=bs$row[u], nrows=bs$nrows[u]))
                out <- lapply(Values, function(x) skyFun(x$z, x$azimuth))

              }

              mapBgb <- z
              mapBgb[] <- unlist(out)
              return(list(mapBgb, model))
            } else {
              return(NA)
            }

          }
)



##### bgInterpol2 ####

.fitTrendSurface <- function (x, sampleProportion, np) {
  for (i in seq(length=nlayers(x))) {
    if (nlayers(x) > 1) {
      tmp <- sampleRandom(raster::subset(x, i), ncell(x) * sampleProportion,
                          sp=TRUE)
    } else {
      tmp <- sampleRandom(x, ncell(x) * sampleProportion, sp=TRUE)
    }

    tmp <- cbind(tmp@coords, tmp@data)

    fit <- spatial::surf.ls(x = tmp[, 1], y = tmp[, 2], z = tmp[, 3], np)
    xl <- xmin(x)
    xu <- xmax(x)
    yl <- ymin(x)
    yu <- ymax(x)

    out <- spatial::trmat(fit, xl, xu, yl, yu, ncol(x))
    out <- raster(out)
    out <- resample(out, x)
    if (nlayers(x) > 1) {
      x[[i]] <- out
    } else {
      x <- out
    }
  }
  return(list(x, fit))
}

#' @title Background interpolation
#'
#' @description todo
#'
#' @param x \code{\linkS4class{CanopyPhoto}}.
#' @param bin \code{\linkS4class{BinImage}}.
#' @param filling \code{\linkS4class{RasterLayer}} or \code{\linkS4class{RasterStack}}.
#' @param erode logical.
#' @inheritParams raster::aggregate
#' @inheritParams stats::quantile
#' @inheritParams spatial::surf.ls
#'
#' @return \code{\linkS4class{BinImage}}
#' @export bgInterpol2
#'
#' @examples #todo
setGeneric("bgInterpol2",
           function(x, bin, mask, filling, prob = 0.95, erode = TRUE, fact = 1, np = 6)
             standardGeneric("bgInterpol2"))

#' @rdname bgInterpol2
setMethod("bgInterpol2",
          signature(x = "CanopyPhoto"),
          function (x, bin, mask, filling, prob, erode, fact, np) {

            stopifnot(class(bin) == "BinImage")
            stopifnot(class(mask) == "BinImage")
            compareRaster(bin, filling)
            compareRaster(bin, x)
            compareRaster(bin, mask)

            bin[!mask] <- NA
            filling[!mask] <- NA

            fun <- function(x, ...) quantile(x, prob, na.rm = TRUE)

            if (erode) bin[] <- raster(EBImage::erode(as.matrix(bin)))[]

            blue <- raster::subset(x, "Blue")
            #blue <- raster::focal(blue, w=matrix(1/9, nc=3, nr=3))

            Blue <- blue
            Blue[!bin] <- NA
            Blue[!mask] <- NA

            if (fact > 1) {
              Blue <- raster::aggregate(Blue, fact,
                                      fun, na.rm = TRUE)
              filling <- raster::aggregate(filling, fact,
                                         mean, na.rm = TRUE)
            }
            Filling <- Map(function(x) raster::subset(filling, x),
                           1:nlayers(filling))



            # correct bias in the data for filling
            .findBias <- function(x, y) {
              m <- doMask(z, zlim = asAngle(c(30, 60)))
              mean(x[m], na.rm = TRUE) - mean(y[m], na.rm = TRUE)
            }
            Filling <- Map(function(x) x - .findBias(x, Blue), Filling)

            # select the better data for filling
            rmse <- function(error) sqrt(mean(error^2, na.rm = TRUE))
            RMSE <- Map(function(x) rmse((x - Blue)[]), Filling)
            index <- which.min(unlist(RMSE))
            Filling <- Filling[[index]]
            RMSE <- RMSE[[index]]

            Filling[Filling > 255] <- 255
            Filling[Filling < 0] <- 0

            # fill

            foo <- raster::sampleRegular(Filling, ncell(Filling) * 0.7,
                                         cells = TRUE)
            Filling[foo[,1]] <- NA

            Blue <- cover(Blue, Filling)

            # Fit trend surface
            r <- .fitTrendSurface(Blue, sampleProportion = 1, np = np)
            GoF <- r[[2]]
            r <- r[[1]]

            if (fact > 1) r <- resample(r, blue)
            compareRaster(r, blue)

            .mask <- mask
            mask[] <- raster(EBImage::erode(as.matrix(mask)))[]
            bg <- mean(r[.mask - mask])
            r[!mask] <- bg
            list(r, RMSE, GoF)

          }
)


# setGeneric("findBg",
#            function(x, thrFun)
#              standardGeneric("findBg"))
#
# setMethod("findBg",
#           signature(x = "CanopyPhoto"),
#           function (x, thrFun) {
#
#             stopifnot(class(thrFun) == "function")
#
#             blue <- raster::subset(x, "Blue")
#
#             # segment
#             z <- makeZimage(ncol(x), lensPolyCoef())
#             a <- makeAimage(z)
#             rings <- makeRings(z, asAngle(30))
#             sectors <- makePolarSectors(a, asAngle(90))
#             segments <- sectors * 1000 + rings
#             blue[is.na(z)] <- NA
#             rm(rings, a, z)
#             segments <- as(segments, "PolarSegmentation")
#
#             # OBIA thresholding
#             baz <- function(x) {
#               if (min(is.na(x)) == 0) {
#                 #bgb <- quantile(x, 0.99, na.rm = TRUE)
#                 bgb <- max(x, na.rm = TRUE)
#                 thr <- thrFun(bgb)
#               } else {
#                 NA
#               }
#             }
#             softblue <- raster::focal(blue, w=matrix(1/9, nc=3, nr=3))
#             thr <- getFeatureImage(softblue, segments, baz)
#             rm(segments)
#             bin <- (blue > thr)
#             as(bin, "BinImage")
#
#           }
# )
#
#

