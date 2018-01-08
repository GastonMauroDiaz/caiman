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
#' @param cOpenSkyEV function.
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
#' @param percentE numeric or NULL. Song et al. (2010) used 1 %.
#' @param absoluteE numeric of NULL.
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
           function(files, df, mask, percentE = NULL, absoluteE = 0.001, ...)
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

              x <- raster::calc(x, mean)
              x[!mask] <- NA
              thrs <- (round(getMin(x)) + 1):(round(getMax(x)) - 1)

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

