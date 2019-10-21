##### getReady4ECM ####
#' @title getReady4ECM
#'
#' @description todo.
#'
#' @param files character. The names of the photographs (extension included) of
#'   the canopy models. If your working directory is the one that contains the
#'   photographs, use just the file name, otherwise, use the full path to
#'   the file.
#' @param keyPhotos numeric vector. The numeric ID of the first photograph taken
#'   to a canopy model with a given light.
#' @param openPipePhotos numeric vector.
#' @param openness numeric.
#' @param path character.
#'
#' @references Song, G.-Z.M., Doley, D., Yates, D., Chao, K.-J., Hsieh, C.-F.,
#'   2014. Improving accuracy of canopy hemispherical photography by a constant
#'   threshold value derived from an unobscured overcast sky. Can. J. For. Res.
#'   44, 17–27. doi:10.1139/cjfr-2013-0082
#'
#' @return data.frame
#' @export getReady4ECM
#'
#' @examples #TODO
setGeneric("getReady4ECM",
           function(files, keyPhotos, openPipePhotos, openness, path)
             standardGeneric("getReady4ECM"))

#' @rdname getReady4ECM
setMethod("getReady4ECM",
          signature(files = "character"),
          function(files, keyPhotos, openPipePhotos, openness, path) {

            foo <- strsplit(files, "/")
            fileName <- unlist(lapply(foo, function(x) x[length(foo[[1]])]))

            if (!dir.exists(path)){
              dir.create(path)
            }

            pathSkies <- paste0(path, "_skies")
            if (!dir.exists(pathSkies)){
              dir.create(pathSkies)
            }

            .findIndex4Photos <- function(files, photoIDs) {
              index <- c()
              for (i in seq_along(photoIDs)) {
                index[i] <- grep(photoIDs[i], files)
              }
              index
            }

            index4Skies <- .findIndex4Photos(files, openPipePhotos)
            openSkiesEV <- calcExposure(files[index4Skies])


            mynames <- letters[1:length(index4Skies)]
            mynames <- paste0(mynames, "_", fileName[index4Skies])

            to <- paste0(pathSkies, "/", mynames)
            file.copy(files[index4Skies], to)

            files <- files[-index4Skies]
            fileName <- fileName[-index4Skies]

            index <- .findIndex4Photos(files, keyPhotos)


            skyTypes <- length(openSkiesEV)
            opennessLevels <- length(keyPhotos) / skyTypes

            foo <- cbind(index, c(index[-1], length(files) + 1))
            foo <- cbind(foo, foo[,2] - foo[,1])

            id <- rep(rep(1:opennessLevels, skyTypes), foo[,3])
            sky <- rep(sort(rep(letters[1:skyTypes], opennessLevels)), foo[,3])

            exposureRefValues <- rep(sort(rep(openSkiesEV, opennessLevels)), foo[,3])
            EV <- calcExposure(files)
            REV <- exposureRefValues - EV
            EV <- signif(EV, 3)
            REV <- signif(REV, 3)

            refData <- openness[match(id, 1:opennessLevels)]

            df <- data.frame(sky, id, EV, EVos = exposureRefValues, REV, refData, fileName)

            mynames <- paste0(df$sky, "_", df$id, "_", EV, "_", 1:length(files))
            mynames <- paste0(mynames, "_", fileName)


            to <- paste0(path, "/", mynames)

            file.copy(files, to)

            write.csv(df, paste0(path, ".csv"))

            return(df)
          }
)

##### findOTRs ####
#' @title Find optimal threholding range
#'
#' @description Find optimal threholding range.
#'
#' @param files character. The names of the canopy model photographs (extension
#'   included), which were renamed by \code{\link{getReady4ECM}}. If your
#'   working directory is the one that contains the photographs, use just the
#'   file name, otherwise, use the full path to the file.
#' @param df data.frame.
#' @param mask \code{\linkS4class{BinImage}}. See \code{\link{doMask}}.
#' @param percentE numeric or NULL. Song et al. (2010) used 1 percent.
#' @param absoluteE numeric of NULL.
#' @param fun function. See \code{\link[raster]{calc}}
#' @param ... aditionals arguments for \code{\link{loadPhoto}}.
#'
#' @references Song, G.-Z.M., Doley, D., Yates, D., Chao, K.-J., Hsieh, C.-F.,
#'   2014. Improving accuracy of canopy hemispherical photography by a constant
#'   threshold value derived from an unobscured overcast sky. Can. J. For. Res.
#'   44, 17-27. doi:10.1139/cjfr-2013-0082
#'
#' @return data.frame
#' @export findOTRs
#'
#' @examples #TODO
setGeneric("findOTRs",
           function(files, df, mask, percentE = NULL, absoluteE = 0.001, fun = mean, ...)
             standardGeneric("findOTRs"))

#' @rdname findOTRs
setMethod("findOTRs",
          signature(files = "character"),
          function(files, df, mask, percentE, absoluteE, ...) {

            foo <- strsplit(files, "/")
            fileName <- unlist(lapply(foo, function(x) x[length(foo[[1]])]))
            foo <- strsplit(files, "_")
            fileName <- unlist(lapply(foo, function(x) x[length(foo[[1]])]))

            index <- match(fileName, df$fileName)
            df <- df[index, ]

            refData <- df$refData

            stopifnot(max(refData) <= 1)
            stopifnot(min(refData) >= 0)

            stopifnot(class(mask) == "BinImage")


            .calcDiscrepancy <- function(x, refValue, mask, percentE) {

              length.out <- max(getMax(x) - getMin(x))

              x <- raster::calc(x, fun)
              x[!mask] <- NA

              thrs <- seq(getMin(x) + 1, getMax(x) - 1, length.out = length.out)

              countWhite <- function(x, thr) sum(x > thr, na.rm = TRUE)

              white <- Map(countWhite, list(values(x)), thrs)

              estimated <- unlist(white) / raster::freq(mask, value = 1)
              if (is.null(percentE)) {
                d <- abs(refValue - estimated)
              } else {
                d <- abs(refValue - estimated) / refValue * 100
              }

              cbind(thrs,d)

            }

            .findOTR <- function(x, targetError){
              index <- which.min(x[,2])
              index <- x[,2] <= (x[index,2] + targetError)
              x[index, 1]
            }

            if (is.null(percentE)) {
              targetError <- absoluteE
            } else {
              targetError <- percentE
            }

            if (is.null(percentE) & is.null(absoluteE))
              stop("You need to choose between percentE and absoluteE.")

            pb <- raster::pbCreate(length(files), "text")
            out <- list()
            for (i in seq_along(files)) {
              raster::pbStep(pb, i)
              x <- loadPhoto(files[i], ...)
              raster::compareRaster(x, mask)
              x <- .calcDiscrepancy(x, refData[i], mask, percentE)
              out[[i]] <- .findOTR(x, targetError)
            }
            raster::pbClose(pb)

            foo <- lapply(out, function(x) length(x))
            foo <- cbind(1:nrow(df), foo)
            foo <- unlist(rep(foo[,1], foo[,2]))
            df <- df[foo, ]
            df <- cbind(df, thr = unlist(out))

            return(df)

          }
)

##### getECMfun ####
#' @title Get the function for EC method
#'
#' @description Wrapper function for \code{bbmle::mle2} to make easier the estimation
#'   of the function for the EC method.
#'
#' @param x data.frame. The return of a call to \code{\link{findOTRs}}.
#' @param method character. See \code{\link[bbmle]{mle2}}
#'
#' @references Song, G.-Z.M., Doley, D., Yates, D., Chao, K.-J., Hsieh, C.-F.,
#'   2014. Improving accuracy of canopy hemispherical photography by a constant
#'   threshold value derived from an unobscured overcast sky. Can. J. For. Res.
#'   44, 17–27. doi:10.1139/cjfr-2013-0082
#'
#' @return list
#' @export getECMfun
#'
#' @examples #TODO
setGeneric("getECMfun",
           function(x, method = "BFGS")
             standardGeneric("getECMfun"))

#' @rdname getECMfun
setMethod("getECMfun",
          signature(x = "data.frame"),
          function(x, method) {

            flog <- function(a, b, c, S) {
              media <- a / (1 + exp((-x$REV + b) / c))
              - sum(dnorm(x$thr, mean = media, sd = exp(S),log = TRUE))
            }
            mm <-  bbmle::mle2(flog, list(a= 216, b= 1.422, c = 1.16, S = 10),
                               data = x, method = method)

            a <-  unname(mm@coef[1])
            b <-  unname(mm@coef[2])
            c <-  unname(mm@coef[3])

            fun <- function(x) a / (1 + exp((-x + b) / c))

            plot(x$REV, x$thr, xlab = "REV", ylab = "Threshold")
            curve(fun, add = TRUE, col = 2)

            list(mm, fun)
          }
)

##### preset_thr_fun ####

#' @title todo
#'
#' @description todo
#'
#' @param DN numeric
#' @param w numeric
#' @param type character
#'
#' @export preset_thr_fun
#'
#' @examples #todo
setGeneric("preset_thr_fun",
           function(DN, w = 1, type = "Generic")
             standardGeneric("preset_thr_fun"))

#' @rdname preset_thr_fun
setMethod("preset_thr_fun",
          signature(DN = "numeric"),
          function (DN, w, type)
          {

            if (type == "Generic") {
              .intercept <- -8
              .slope <- 1
            }
            if (type == "Nikon_Coolpix_5700") {
              .intercept <- -7.78759266023174
              .slope <- 0.948507280931792
            }


            if (length(DN[DN > 255]) > 0) {DN[DN > 255] <- 255}

            thr <- .intercept  + .slope * DN * w

            if (length(thr[thr < 0]) > 0) {thr[thr < 0] <- 0}

            thr

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
                    erode = FALSE, ZA = TRUE, parallel = TRUE,
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
           function(x, bin, mask, filling, prob = 0.95, erode = FALSE, fact = 5, np = 6)
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


            # mask[] <- raster(EBImage::erode(as.matrix(mask)))[]
            # bg <- mean(r[.mask - mask])
            # r[!mask] <- bg
            # list(r, RMSE, GoF)

            # 10/9/2019
            # A trick to increase the robustness of the fit.
            # It uses a plane to model the DNs near the horizon.
            aux_mask <- mask
            ## filter the estimation
            r[r<1] <- NA

            ## sample the estimation near the horizon
            aux_mask[] <- raster(EBImage::erode(as.matrix(mask)))[]
            aux_mask <- mask - aux_mask
            aux_r <- r
            aux_r[!aux_mask] <- NA
            ### filter the sample
            thr <- median(aux_r[], na.rm = TRUE) - sd(aux_r[], na.rm = TRUE)
            aux_r[aux_r<thr] <- NA

            ## fit a plane and edit it
            plane <- .fitTrendSurface(aux_r, 1, 1)
            plane <- plane[[1]]
            plane[plane<1] <- 1
            plane[plane>254] <- 254
            aux_mask <- z
            aux_mask[!mask] <- NA
            if(getMax(aux_mask)+10 < 90) {
              aux_mask <- doMask(z, zlim = asAngle(c(0,
                                                  getMax(aux_mask)+10)))

            } else {
              aux_mask <- doMask(z)
            }
            plane[aux_mask] <- NA

            ## filter the estimation (again but with other approach)
            r[!mask] <- NA
            thr <- median(r[], na.rm = TRUE) - sd(r[], na.rm = TRUE)
            r[r<thr] <- NA

            ## support the estimation with the plane
            r[!mask] <- NA
            r <- cover(r, plane)

            ## adjust a surface to the supported data
            ### if the result is bad,
            ### try with a more rigid model
            m <- doMask(z)
            fun <- function(np) {
              surf <- .fitTrendSurface(r, 0.7, np)
              surf[[1]]
            }
            unlock <- TRUE
            np <- 7
            while (unlock) {
              np <- np - 1
              aux_r <- fun(np)
              r_values <- aux_r[m]
              unlock <- any(r_values < 0)
              if (unlock) unlock <- np > 3
            }

            if (np == 3) {
              thr <- median(r[], na.rm = TRUE)
              aux_r <- fun(6)
              aux_r[aux_r<thr] <- thr
            }

            aux_r[!m] <- NA


            list(skyDN = aux_r, RMSE = RMSE, GoF = GoF)

          }
)





##### MBLT1 ####

#' @title Local thresholding through sky DN estimation
#'
#' @description todo
#'
#' @param ncp \code{\linkS4class{CanopyPhoto}}.
#' @param z todo
#' @param a todo
#' @param m todo
#' @param w todo
#' @param filling_source todo
#' @param thr_fun todo
#' @param channel todo
#'
#' @return \code{\linkS4class{BinImage}} or \code{\linkS4class{RasterLayer}}
#' @export MBLT1
#'
#' @examples #todo
setGeneric("MBLT1",
           function(ncp, z, a, m, w = 0.5, filling_source = NULL,
                    sky = FALSE, linear_stretching = FALSE,
                    thr_fun = preset_thr_fun,
                    channel  = "Blue")
             standardGeneric("MBLT1"))

#' @rdname MBLT1
setMethod("MBLT1",
          signature(ncp = "CanopyPhoto"),
          function (ncp, z, a, m, w, filling_source, sky,
                    linear_stretching, thr_fun, channel)
          {

            if (class(filling_source) == "data.frame") {
              fun <- function(name) {
                ext <- paste0(".", tools::file_ext(name))
                sub(ext, "", name)
              }
              name <- rtitle(ncp)
              name <- fun(name)
              index <- filling_source$file_sky_chunks
              index <- unlist(Map(fun, index))
              index <- match(name, index)
              filling_source <- filling_source$sky_name[index]
              filling_source <- as.character(filling_source)
              filling_source <- raster(filling_source)
              filling_source <- normalize(filling_source,
                                          0 , 2^16)
            }

            #check if filling_source is has only one layer


            cp <- ncp

            if (linear_stretching) {
              cp <- normalize(ncp,
                              mean(getMin(ncp)),
                              mean(getMax(ncp)))
              cp[] <- cp[] * 255
            } else {
              cp[] <- ncp[] * 255
            }

            #blue <- raster::subset(cp, channel)
            blue <- cp$Blue

            # rings <- makeRings(z, asAngle(30))
            rings <- makePolarGrid(z, a, angleWidth = asAngle(30))


            # tower cake form
            .tower_cake <- function(prob) {
              if (prob < 0.9) {
                  stop(paste("Mmm, something is wrong",
                             "with your inputs.",
                             "Please, revise them."))
              }

              bin <- presetThr(blue, getMin(blue))
              .binarize_per_ring <- function(ring_id) {
                indices <- rings == ring_id
                bg <- quantile(blue[indices], prob)
                thr <- thr_fun(bg)
                bin[indices] <<- blue[indices] > thr
              }
              Map(.binarize_per_ring, unique(rings))
              bin
            }


            if (is.null(filling_source)) {
              # irregular cone form --without filling_source
              prob <- 1
              mapBg <- NA
              while (is.na(mapBg)) {
                prob <- prob - 0.01
                bin <- .tower_cake(prob)
                mapBg <- bgModel(cp, z, a, bin)
              }

              thr <- calc(mapBg[[1]], function(x) thr_fun(x))
              bin <- presetThr(blue, thr)

              # irregular smooth surface --without filling_source
              mapBg <- bgInterpol2(cp, bin, m, mapBg[[1]])

            } else {
              # irregular cone form
              prob <- 1
              mapBg <- NA
              while (is.na(mapBg)) {
                prob <- prob - 0.01
                bin <- .tower_cake(prob)
                mapBg <- bgModel(cp, z, a, bin, filling = filling_source)
              }

              thr <- calc(mapBg[[1]], function(x) thr_fun(x))
              bin <- presetThr(blue, thr)

              # irregular smooth surface
              mapBg <- bgInterpol2(cp, bin, m, filling_source)
            }

            #Almost done

            fun <- function(w) {
              thr <- calc(mapBg[[1]], function(x) thr_fun(x, w))
              presetThr(blue, thr)
            }

            if (is.null(w)) {
              return(mapBg[[1]])
            } else {
              if (length(w) == 1) {
                bin <- fun(w)
                rtitle(bin) <- paste("w=", w)

                if (sky) {
                  blue <- ncp$Blue
                  blue <- blue * 2^16
                  blue[!bin] <- NA
                  blue[!m] <- NA
                  return(blue)
                } else {
                  return(bin)
                }

              } else {
                bin <- Map(fun, w)
                bin <- Map(`rtitle<-`, bin, paste("w=", w))
                return(bin)
              }

            }

          }
)

##### process_multi_point ####
.interpolate_akima_2 <- function(ds, linear = TRUE) {
  x <- unlist(Map(function(i) ds[[i]]$x, seq_along(ds)))
  y <- unlist(Map(function(i) ds[[i]]$y, seq_along(ds)))
  z <- unlist(Map(function(i) ds[[i]]$z, seq_along(ds)))

  xo <- seq(min(x), max(x), length=25)
  yo <- seq(min(y), max(y), length=25)
  r <- akima::interp(x, y, z, xo=xo, yo=yo, linear)
  raster(r)
}


#' @title process multi point
#'
#' @description process multi point.
#'
#' @param block_info data.frame
#' @param path2data character
#' @param path4output character
#' @param datatype character
#'
#' @export process_multi_point
#'
#' @examples #TODO
setGeneric("process_multi_point",
           function(block_info, path2data,
                    path4output, datatype = "INT2U")
             standardGeneric("process_multi_point"))

#' @rdname process_multi_point
setMethod("process_multi_point",
          signature(block_info = "data.frame"),
          function(block_info, path2data,
                   path4output, datatype = "INT2U") {

            .extract_blueDN <- function(i) {
              name <- as.character(block_info$file[i])
              ext <- paste0(".", tools::file_ext(name))
              name <- sub(ext, "", name)
              xy <- read.csv(paste0(path2data, name, ".csv"))
              cp <- loadPhoto(paste0(path2data, block_info$file[i]))
              x <- xy$X
              y <- nrow(cp) - xy$Y
              p <- SpatialPoints(data.frame(x, y))
              z <- raster::extract(cp$Blue, p)
              z[is.na(z)] <- 255
              data.frame(x, y, z)
            }

            indices <- seq_along(block_info$file)
            ds <- Map(.extract_blueDN, indices)
            for (i in unique(block_info$block)) {
              indices_i <- indices[block_info$block == i]
              ds_i <- Map(function(u) ds[[u]], indices_i)
              r <- .interpolate_akima_2(ds_i)
              cp <- loadPhoto(paste0(path2data, block_info$file[i]))
              r <- resample(r, cp)
              r <- brick(r, r, r)
              for (u in indices_i) {
                name <- as.character(block_info$file[u])
                ext <- paste0(".", tools::file_ext(name))
                name <- sub(ext, "", name)
                writeRaster(r, paste0(path4output, name, ".tif"),
                            datatype = datatype, overwrite = TRUE)
              }
            }

          }
)


##### asynchronic_sky ####
#' @title asynchronic sky
#'
#' @description asynchronic sky.
#'
#' @param block_info data.frame
#' @param path2sky_chunks character
#' @param path4output character
#' @param m BinImage
#' @param z ZenithImage
#' @param a AzimuthImage
#'
#' @export asynchronic_sky
#'
#' @examples #TODO
setGeneric("asynchronic_sky",
           function(block_info, path2sky_chunks,
                    path4output, m, z, a)
                      standardGeneric("asynchronic_sky"))

#' @rdname asynchronic_sky
setMethod("asynchronic_sky",
          signature(block_info = "data.frame"),
          function(block_info, path2sky_chunks,
                   path4output, m, z, a) {

            sky_name <- rep(NA, length(block_info$file_sky_chunks))
            blocks <- unique(block_info$block)

            for (i in seq_along(blocks)) {
              block <- blocks[i]
              indices <- block_info$block == block
              indices[is.na(indices)] <- FALSE
              files <- block_info$file_sky_chunks[indices]
              hour_24clock <- block_info$hour_24clock[indices]

              .get_sky_chunks <- function(file) {
                r <- raster(paste0(path2sky_chunks, "/",  file))
                if(!is.na(r[1])) {
                  r[r == r[1]] <- NA
                }
                r <- focal(r, matrix(1/9, 3, 3))
                if (!is.null(m)) {
                  r[!m] <- NA
                }
                r <- aggregate(r, 4, mean)
                r
              }

              sky_chunks <- Map(.get_sky_chunks, files)


              indices <- Map(function(x) freq(is.na(x), value = 0), sky_chunks)
              indices <- unlist(indices)
              indices <- order(indices, decreasing = TRUE)
              master <- sky_chunks[[indices[1]]]
              norm_hour_24clock <- abs(hour_24clock - hour_24clock[indices[1]])
              indices <- order(norm_hour_24clock, decreasing = FALSE)
              sky_chunks <- Map(function(i) sky_chunks[[i]], indices[-1])

              sky_chunks <- stack(sky_chunks)

              # combine
              for (i in nlayers(sky_chunks)) {
                sky <- cover(master, raster::subset(sky_chunks, i))
              }

              sky <- disaggregate(sky, 4, "bilinear")
              if (!compareRaster(sky, z, stopiffalse=FALSE)) {
                sky <- resample(sky, z)
              }

              # reproject and filter
              g <- makePolarGrid(z, a, asAngle(7.5))
              pano <- fisheye2pano(sky, g, fun = function(x) max(x, na.rm = TRUE))
              pano[is.infinite(pano)] <- NA

              thr <- median(pano[], na.rm = TRUE) - sd(pano[], na.rm = TRUE)
              pano[pano < thr] <- NA

              # fill NA
              pano <- (focal(pano, matrix(1, 3, 3),
                             fun = mean, na.rm = TRUE, NAonly = TRUE))
              pano <- (focal(pano, matrix(1, 3, 3),
                             fun = mean, na.rm = TRUE, NAonly = TRUE))

              # reprojet back
              pano <- disaggregate(pano, 4, "bilinear")
              g <- makePolarGrid(z, a, asAngle(7.5/4))
              pano_g <- fisheye2pano(g, g, fun = max)
              extent(pano) <- extent(pano_g)
              pano <- resample(pano, pano_g)

              rcl <- data.frame(pano_g[], pano[])
              sky <- reclassify(g, rcl)

              m_aux <- doMask(z, previousMask = NULL,
                              alim = asAngle(c(5, 355)),
                              zlim = asAngle(c(0,80))
              )
              sky[!m_aux] <- NA

              if (!is.null(m)) {
                sky[!m] <- NA
              }


              ## fill the voids
              .interpolate_akima <- function(x, linear = TRUE) {

                xy <- coordinates(x)
                z <- values(x)

                xy <- xy[!is.na(z),]
                z <- z[!is.na(z)]

                xo <- seq(xmin(x), xmax(x), length=ncol(x))
                yo <- seq(ymin(x), ymax(x), length=nrow(x))
                r <- akima::interp(xy[,1], xy[,2], z, xo=xo, yo=yo, linear)

                r <- raster(r)
                extent(r) <- extent(x)
                r <- extend(r, x)
                projection(r) <- projection(x)
                r
              }

              r <- raster(ncol = 100, nrow = 100)
              extent(r) <- extent(0, ncol(sky), 0, nrow(sky))
              projection(r) <- NA

              .interpolate_and_merge <- function(sky) {
                aux <- resample(sky, r)
                aux <- .interpolate_akima(aux)
                i <- 0
                while (i < 3) {
                  i <- i + 1
                  aux <- focal(aux, matrix(1,3,3), fun = mean,
                               na.rm = TRUE, NAonly = TRUE)
                }
                aux <- resample(aux, sky)
                cover(sky, aux)
              }

              sky <- .interpolate_and_merge(sky)

              sky_name_i <-  paste0("sky4block", block, ".tif")
              sky_name_i <- paste0(path4output, "/", sky_name_i)

              writeRaster(sky, sky_name_i,
                          format = "GTiff", datatype = "INT2U",
                          overwrite = TRUE)

              sky_name[block_info$block == block] <- sky_name_i

            }
            cbind(block_info, sky_name)
          }
)



##### MBLT2 ####

.interpolate_akima <- function(x, linear = TRUE) {

  xy <- coordinates(x)
  z <- values(x)

  xy <- xy[!is.na(z),]
  z <- z[!is.na(z)]

  xo <- seq(xmin(x), xmax(x), length=ncol(x))
  yo <- seq(ymin(x), ymax(x), length=nrow(x))
  r <- akima::interp(xy[,1], xy[,2], z, xo=xo, yo=yo, linear)

  r <- raster(r)
  extent(r) <- extent(x)
  r <- extend(r, x)
  projection(r) <- projection(x)
  r
}


#' @title todo
#'
#' @description todo
#'
#' @param ncp \code{\linkS4class{CanopyPhoto}}.
#' @param m todo
#' @param bin_plant todo
#' @param bin_sky todo
#' @param fill todo
#' @param sample_size must be even
#' @param size_range todo
#' @param channel todo
#' @param parallel todo
#' @param free_threads todo
#'
#' @return \code{\linkS4class{BinImage}}
#' @export MBLT2
#'
#' @examples #todo
setGeneric("MBLT2",
           function(ncp, m, bin_plant, bin_sky, fill = NULL,
                    sample_size = 30, size_range = NULL,
                    size_step = NULL,
                    channel  = "Blue", parallel = TRUE,
                    free_threads = 2)
             standardGeneric("MBLT2"))

#' @rdname MBLT2
setMethod("MBLT2",
          signature(ncp = "CanopyPhoto"),
          function (ncp, m, bin_plant, bin_sky, fill,
                    sample_size, size_range, size_step, channel,
                    parallel, free_threads) {

            compareRaster(ncp, m)
            compareRaster(ncp, bin_plant)
            compareRaster(ncp, bin_sky)
            if (!is.null(fill)) compareRaster(ncp, fill)

            blue <- raster::subset(ncp, channel) * 255
            #blue <- raster::aggregate(blue, 4)

            if (is.null(size_range)) {
              size_range <- ncol(blue) * 0.02
              size_range <- round(size_range)
              if(round(size_range/2) != size_range/2) {
                size_range <- size_range + 1
              }
            }
            if (is.null(size_step)) {
              size_step <- round(size_range * 0.2)
              if (round(size_step/2) != size_step/2) {
                size_step <- size_step - 1
              }
            }

            bin_plant[!m] <- 0
            bin_sky[!m] <- 0

            sizes <- round(sqrt(sample_size*4))
            if (sizes/2 == round(sizes/2)) sizes <- sizes + 1
            sizes <- seq(sizes, sizes + size_range, size_step)


            fun <- function(x, size) focal(x, w = matrix(1/size,
                                                         nc=size, nr=size))


            if (parallel) {
              # go parallel
              no_threads <- parallel::detectCores() - free_threads

              ## Initiate cluster
              cl <- parallel::makeCluster(no_threads)
              parallel::clusterExport(cl, c("sizes", "bin_plant"),
                                      environment())
              stack_plant <- parallel::parLapply(cl,
                                                 sizes, function(size) fun(bin_plant, size))

              ## finish
              parallel::stopCluster(cl)

              ## Initiate cluster
              cl <- parallel::makeCluster(no_threads)
              parallel::clusterExport(cl, c("sizes", "bin_sky"),
                                      environment())
              stack_sky <- parallel::parLapply(cl,
                                               sizes, function(size) fun(bin_sky, size))

              ## finish
              parallel::stopCluster(cl)
            } else {
              stack_plant <- lapply(sizes, function(size) fun(bin_plant, size))
              stack_sky <- lapply(sizes, function(size) fun(bin_sky, size))
            }

            stack_plant <- Map(function(i) stack_plant[[i]] *
                                 sizes[i], seq_along(sizes) )
            stack_sky <- Map(function(i) stack_sky[[i]] * sizes[i],
                             seq_along(sizes) )

            # Flatten over sample_size
            fun <- function(x) {
              x[x > sample_size] <- sample_size
              x
            }
            stack_plant <- Map(fun, stack_plant)
            stack_sky <- Map(fun, stack_sky)

            # Create label
            fun <- function(x, size) {x * 100 + max(sizes) - size}
            stack_plant <- Map(fun, stack_plant, sizes)
            stack_sky <- Map(fun, stack_sky, sizes)

            # Find the minimum size that gets at least the sample_size
            flattened_plant <- calc(stack(stack_plant), max)
            ## transform the label into data
            min_size_plant <- flattened_plant - trunc(flattened_plant / 100) * 100
            min_size_plant <- max(sizes) - min_size_plant

            flattened_sky <- calc(stack(stack_sky), max)
            ## transform the label into data
            min_size_sky <- flattened_sky - trunc(flattened_sky / 100) * 100
            min_size_sky <- max(sizes) - min_size_sky
            rm(flattened_plant, flattened_sky, stack_plant,
               stack_sky)

            # filter out non-mixed-pixels
            mixed_pixel_mask <- !bin_plant & !bin_sky & m
            min_size_plant[!mixed_pixel_mask] <- NA
            min_size_sky[!mixed_pixel_mask] <- NA

            # for unknow reason, I needed to run this.
            min_size_plant <- round(min_size_plant)
            min_size_sky <- round(min_size_sky)

            # Get the required sizes
            sizes_plant <- unique(min_size_plant)
            sizes_sky <- unique(min_size_sky)

            # Remove the sizes that should not be there.
            temp <- match(sizes_plant, sizes)
            for (i in sizes_plant[is.na(temp)]) {
              index <- which.min(abs(sizes - i))
              min_size_plant[min_size_plant == i] <- sizes[index]
            }

            temp <- match(sizes_sky, sizes)
            for (i in sizes_sky[is.na(temp)]) {
              index <- which.min(abs(sizes - i))
              min_size_sky[min_size_sky == i] <- sizes[index]
            }

            # Get the required sizes --again
            sizes_plant <- unique(min_size_plant)
            sizes_sky <- unique(min_size_sky)

            # Filter the blue layer
            blue_plant <- blue_sky <- blue
            blue_plant[!bin_plant] <- NA
            blue_sky[!bin_sky] <- NA

            # Generate the required sizes
            fun <- function(x, y) focal(x, matrix(1, y, y),
                                        fun = median, na.rm = TRUE)

            if (parallel) {
              ## Initiate cluster
              cl <- parallel::makeCluster(no_threads)
              parallel::clusterExport(cl, c("sizes_plant", "blue_plant"),
                                      environment())
              stat_plant <- parallel::parLapply(cl,
                                                sizes_plant, function(y) fun(blue_plant, y))
              ## finish
              parallel::stopCluster(cl)

              ## Initiate cluster
              cl <- parallel::makeCluster(no_threads)
              parallel::clusterExport(cl, c("sizes_sky", "blue_sky"),
                                      environment())
              stat_sky <- parallel::parLapply(cl,
                                              sizes_sky, function(y) fun(blue_sky, y))

              ## finish
              parallel::stopCluster(cl)
            } else {
              stat_plant <- Map(function(y) fun(blue_plant, y),
                                sizes_plant)
              stat_sky <- Map(function(y) fun(blue_sky, y),
                              sizes_sky)
            }

            # Find the most local stat
            most_local_stat_plant <- most_local_stat_sky <- raster(blue)

            for (i in seq_along(sizes_plant)) {
              indices <- min_size_plant == sizes_plant[i]
              most_local_stat_plant[indices] <- stat_plant[[i]][indices]
            }

            for (i in seq_along(sizes_sky)) {
              indices <- min_size_sky == sizes_sky[i]
              most_local_stat_sky[indices] <- stat_sky[[i]][indices]
            }

            stat_plant <- most_local_stat_plant
            stat_sky <- most_local_stat_sky
            rm(most_local_stat_plant, most_local_stat_sky)

            ## fill the voids
            r <- raster(ncol = 100, nrow = 100)
            extent(r) <- extent(0, ncol(blue), 0, nrow(blue))
            projection(r) <- NA

            fun <- function(stat_x) {
              aux <- resample(stat_x, r)
              aux <- .interpolate_akima(aux)
              i <- 0
              while (i < 3) {
                i <- i + 1
                aux <- focal(aux, matrix(1,3,3), fun = mean,
                             na.rm = TRUE, NAonly = TRUE)
              }
              aux <- resample(aux, stat_x)
              cover(stat_x, aux)
            }

            stat_plant <- fun(stat_plant)
            stat_sky <- fun(stat_sky)

            # ## fill the voids --option 2
            # fun <- function(x) {
            #   aux <- aggregate(x, 10)
            #   aux <- disaggregate(aux, 10, method = "bilinear")
            #   aux <- resample(aux, x)
            #   cover(x, aux)
            # }
            # stat_plant <- fun(stat_plant)
            # stat_sky <- fun(stat_sky)

            # ## fill the voids --option 1
            #
            # fun <- function(stat_x) {
            #   stat_x <- focal(stat_x, matrix(1, 5, 5), median,
            #                   NAonly = TRUE, na.rm = TRUE)
            #   no_of_loops <- 0
            #   unlock <- TRUE
            #   no_mixed_pixels <- freq(mixed_pixel_mask, value = 1)
            #   while (unlock) {
            #     no_of_loops <- no_of_loops + 1
            #     stat_x <- focal(stat_x, matrix(1, 5, 5), median,
            #                     NAonly = TRUE, na.rm = TRUE)
            #     unlock <- stat_x
            #     unlock[!mixed_pixel_mask] <- 0
            #     unlock <- freq(is.na(unlock), value = 1) / no_mixed_pixels * 100
            #     unlock <- unlock > 1
            #     if (unlock) unlock <- no_of_loops < size_range
            #
            #   }
            #   stat_x
            # }
            #
            #
            # if (parallel) {
            #   ## Initiate cluster
            #   cl <- parallel::makeCluster(no_threads)
            #   parallel::clusterExport(cl, c("stat_plant",
            #                                 "stat_sky",
            #                                 "mixed_pixel_mask"),
            #                           environment())
            #   stack_xs <- parallel::parLapply(cl,
            #                                   list(stat_plant, stat_sky), fun)
            #
            #   ## finish
            #   parallel::stopCluster(cl)
            #
            #   ## Clean
            #   stat_plant <- stack_xs[[1]]
            #   stat_sky <- stack_xs[[2]]
            #
            # } else {
            #   stat_plant <- fun(stat_plant)
            #   stat_sky <- fun(stat_sky)
            # }



            # Equation 12 from Leblanc et al. (2005)
            # DOI: 10.1016/j.agrformet.2004.09.006
            Leblanc2005_eq12 <- function(DN, DNmin, DNmax) {
              (DN - DNmin) / (DNmax - DNmin)
            }

            # combine pure pixels with mixed pixels
            gf <- Leblanc2005_eq12(blue, stat_plant, stat_sky)
            gf[!mixed_pixel_mask] <- NA
            gf[gf > 0.9] <- 1
            gf[gf < 0] <- 0
            gf[bin_sky] <- 1
            gf[bin_plant] <- 0

            # fill
            if (!is.null(fill)) {
              gf <- cover(gf, fill)
            }

            return(gf)

          }
)

##### subpixel2bin ####

#' @title todo
#'
#' @description todo
#'
#' @param subpixel todo
#' @param segmentation todo
#'
#' @return \code{\linkS4class{BinImage}}
#' @export subpixel2bin
#'
#' @examples #todo
setGeneric("subpixel2bin",
           function(subpixel, segmentation)
             standardGeneric("subpixel2bin"))

#' @rdname subpixel2bin
setMethod("subpixel2bin",
          signature(subpixel = "RasterLayer"),
          function (subpixel, segmentation) {

            stopifnot(class(segmentation)[[1]] == "PolarSegmentation")

            fun <- function(x) {
              no_of_pixels <- round(mean(x) * length(x))

              if (no_of_pixels > 0 &
                  no_of_pixels != length(x)) {
                indices <- order(x, decreasing = TRUE)[1:no_of_pixels]
                x[indices] <- 1
                x[x != 1] <- 0

              } else {
                x <- as.numeric(x > 0.5)
              }
              x
            }

            .cells <- subpixel
            values(.cells) <- 1:ncell(subpixel)
            .cells <- tapply(values(.cells), values(segmentation), function(x) x)
            .cells <- unlist(.cells)
            bin <- tapply(values(subpixel), values(segmentation), fun)
            subpixel[.cells] <- unlist(bin)
            as(subpixel, "BinImage")

          }
)

