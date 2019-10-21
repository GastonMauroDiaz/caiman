#### gs4cimes ####
#' @title todo
#'
#' @param x todo
#' @param ... todo
#' @param z todo
#' @param a todo
#' @param m todo
#' @param file todo
#' @param forceEqualLength todo
#' @param txt todo
#'
#' @return todo
#'
#' @examples #todo
setGeneric("gs4cimes", function(x, ...) standardGeneric("gs4cimes"))
#' @export gs4cimes

#' @rdname gs4cimes
setMethod("gs4cimes",
          signature(x = "BinImage"),
          function (x, z, a, m, file, forceEqualLength = FALSE, txt = TRUE) {

            stopifnot(class(z) == "ZenithImage")
            stopifnot(class(a) == "AzimuthImage")
            stopifnot(class(m) == "BinImage")

            df <- data.frame(z = z[m], a = a[m], bin = x[m])
            df <- df[order(df$z, df$a), ]

            df$z <- round(df$z)

            .extractSequences <- function(x) {
              foo <- x - c(x[-1], x[1])
              foo <- (1:length(foo))[foo != 0]
              foo <- c(foo[1], foo[-1] - foo[-length(foo)])

              if (any(is.na(foo))) foo <- length(x)

              if (x[1] == 0) foo <- c(0, foo) else foo <- c(1, foo)

              return(foo)
            }

            sequences <- tapply(as.numeric(df$bin), df$z, .extractSequences)

            .splitSequence <- function(sequence, angle) {

              if (forceEqualLength) {
                foo <- length(sequence)
                if (round(foo / 2) != foo / 2) sequence <- sequence[-foo]
              }

              if (length(sequence) != 2) {

                angle <- paste(angle, "0", sep = ".")

                .makeLine <- function(x, index) {
                  x <- paste(x[index], collapse = " ")
                  paste0(x, " ")
                }

                subSeq1 <- .makeLine(sequence, seq(2, length(sequence), 2))
                subSeq2 <- .makeLine(sequence, seq(3, length(sequence), 2))

                if (sequence[1] == 1) {
                  out <- c(angle, subSeq1, subSeq2)
                } else {
                  out <- c(angle, subSeq2, subSeq1)
                }
                return(out)

              }
            }

            gs <- Map(.splitSequence, sequences, names(sequences))
            index <- unlist(lapply(gs, is.null))
            gs <- gs[!index]
            gs <- unlist(gs)

            if (txt) cat(sapply(gs, toString), file = file, sep= "\n", fill = FALSE)

          }
)


#' @rdname gs4cimes
setMethod("gs4cimes",
          signature(x = "character"),
          function (x, z, a, ...) {

            # seguramente sea mejor usar on.exit() y setwd(), ver el mismo problema
            # en extractSkyBright

            foo <- substr(x, nchar(x), nchar(x))
            if (any(foo == c(letters, LETTERS))) stop("Use the slash path at the end of x.")

            path <- x

            files <- dir(path, pattern = ".tif", ignore.case = TRUE)

            path.out <- paste0(path, "gs/")
            dir.create(paste0(path, "gs"))

            for (i in files) {

              x <- raster(paste0(path, i), NAvalues = Inf)

              if (getMax(x) == 255) {
                foo <- !is.na(x)
                if (getMin(foo) == 0) x <- foo else x <- normalize(x, 0, 255)
              }

              x <- as(x, "BinImage")

              gs4cimes(x, z, a,
                       file = paste0(path.out, paste0("gs_", strsplit(i, "\\.")[[1]][1], ".txt")),
                       ...)
            }

          })

#### gf4cimes ####
#' @title todo
#'
#' @param x todo
#' @param ... todo
#' @param z todo
#' @param a todo
#' @param m todo
#' @param angleWidth todo
#' @param file todo
#' @param txt todo
#'
#' @return todo
#'
#' @examples todo
setGeneric("gf4cimes", function(x, ...) standardGeneric("gf4cimes"))
#' @export gf4cimes

#' @rdname gf4cimes
setMethod("gf4cimes",
          signature(x = "BinImage"),
          function (x, z, a, m, angleWidth, file, txt = TRUE) {

            stopifnot(getMax(x) == 1)
            stopifnot(getMin(x) == 0)

            skg <- makePolarGrid(z, a, angleWidth)

            if (!angleWidth@degrees) angleWidth <- switchUnit(angleWidth)
            angleWidth <- angleWidth@values

            stopifnot(class(z) == "ZenithImage")
            stopifnot(class(a) == "AzimuthImage")
            stopifnot(class(m) == "BinImage")

            skg[m == 0] <- NA
            gf <- extractFeatures(x, skg, mean)
            pixels <- extractFeatures(x, skg, length)

            sectors <- trunc(as.numeric(names(gf)) / 1000)
            rings <- ((as.numeric(names(gf)) / 1000) - sectors) * 1000

            fun <- function(x) { # implementar en makeRings y makePolarSectors
              round(x * angleWidth - angleWidth / 2, 2)
            }

            df <- data.frame(zenith = fun(rings), azimuth = fun(sectors),
                             gapfraction = gf, pixels)

            foo <- df$zenith * 360 - df$azimuth
            # df <- df[order(df$zenith), ]
            df <- df[order(foo),]

            firstLine <- c(length(levels(factor(df$zenith))),
                           length(levels(factor(df$azimuth))))
            firstLine <- paste(firstLine, collapse = "   ")

            fun <- function(x) {
              foo <- x[1:3] - trunc(x[1:3])
              foo <- as.character(foo + 1000 + 0.0000001)
              foo <- substr(foo, 5, rep(11, 3))
              foo <- c(paste0(trunc(x[1:3]) , foo), x[4])
              paste(foo, collapse = "   ")
            }

            gf <- c(firstLine, apply(df, 1, fun))

            if (txt) cat(sapply(gf, toString), file = file, sep= "\n", fill = FALSE)
          }
)


#' @rdname gf4cimes
setMethod("gf4cimes",
          signature(x = "character"),
          function (x, z, a, m, subfolder = "out",...) {

            path.old <- getwd()
            on.exit(setwd(path.old))
            setwd(x)

            path <- x

            files <- dir(path, pattern = ".tif", ignore.case = TRUE)

            path.out <- paste0(getwd(), "/", subfolder, "/")
            dir.create(path.out, showWarnings = FALSE)

            pb <- pbCreate(length(files), "text")

            for (i in seq_along(files)) {
              pbStep(pb, i)
              x <- raster(paste0(path, files[i]))
              if (getMax(x) == 255) x <- is.na(x)
              x <- as(x, "BinImage")

              gf4cimes(x, z, a, m,
                       paste0(path.out, paste0("gf_", strsplit(files[i], "\\.")[[1]][1], ".txt")),
                       ...)
            }
            pbClose(pb)

          })


#### makeBat4cimes ####

setGeneric("makeBat4cimes", function(x, ...) standardGeneric("makeBat4cimes"))

setMethod("makeBat4cimes",
          signature(x = "character"),
          function (x, pattern = "gf_", p, param = "", resultFile = NULL, ...) {

            old.path <- getwd()
            on.exit(setwd(old.path))
            setwd(x)

            files <- dir(pattern = pattern)

            lines <- character(length(files))

            if (!is.null(resultFile)) result <- resultFile

            for (i in 1:length(files)){
              if (is.null(resultFile)) result <- paste("result", files[i], sep="_")
              lines[i] <- paste(p, param, files[i], result)
            }

            lines <- as.data.frame(lines)
            write.table(lines, paste("batch-", p, ".bat", sep = ""), quote = FALSE,
                        row.names = FALSE, col.names = FALSE)
          })




#### nh2005 ####
.nh2005 <- function(x, thrs, parallel, freeThreads) {

  get_avg_brightness_difference <- function(i) {

    thr <- thrs[i]
    set.seed(thr)
    w <- matrix(c(0.25,0.25,0,
                  0.25,0.25,0,
                  0,0,0), nrow = 3, ncol = 3)

    bin <- x > thr
    edge_m <- focal(bin, w)
    edge_m <- edge_m !=1 & edge_m != 0
    x[!edge_m] <- NA

    .cells <- x
    .cells[] <- 1:ncell(x)
    .cells <- .cells[edge_m]

    .cells <- sample(.cells, round(length(.cells) * 0.3))

    # |3|4|0|
    # |2|1|0|
    # |0|0|0|

    row_col_1 <- rowColFromCell(x, .cells)
    fun <- function(move_row, move_col) {
      x <- row_col_1
      x[,1] <- row_col_1[,1] + move_row
      x[,2] <- row_col_1[,2] + move_col
      x
    }
    row_col_2 <- fun(0,-1)
    row_col_3 <- fun(-1,-1)
    row_col_4 <- fun(-1,0)

    ds <- data.frame(c1 = x[row_col_1],
                     c2 = x[row_col_2],
                     c3 = x[row_col_3],
                     c4 = x[row_col_4])

    # Filter NAs
    indices <- apply(ds, 1, mean) #to spot the NAs
    indices <- !is.na(indices)
    ds <- ds[indices,]

    # fun <- function(x) {
    #   for_calc <- expand.grid(x[x > thr], x[x <= thr])
    #   mean(abs(for_calc[,1] - for_calc[,2]))
    # }
    fun <- function(x) {
      abs(mean(x[x > thr]) - mean(x[x <= thr]))
    }

    x <- apply(ds, 1, fun)

    #    x <- x[!is.nan(x)]
    mean(x)

  }

  if (parallel) {

    # go parallel
    no_threads <- parallel::detectCores() - freeThreads

    ## Initiate cluster
    cl <- parallel::makeCluster(no_threads)
    parallel::clusterExport(cl,
                            c("x", "thrs", "get_avg_brightness_difference"),
                            environment())
    ABDs <- parallel::parLapply(cl, seq_along(thrs), get_avg_brightness_difference)
    ## finish
    parallel::stopCluster(cl)

    out <- thrs[which.max(unlist(ABDs))]

  } else {

    ABDs <- lapply(seq_along(thrs), get_avg_brightness_difference)
    out <- thrs[which.max(unlist(ABDs))]

  }
  return(out)


}


#' @title Experimental.
#'
#' @param x \code{\linkS4class{CanopyPhoto}} or
#'   \code{\linkS4class{RasterLayer}}.
#' @param subset numeric or character. It indicates the channel to process
#'   (represented as integer or by their name).
#' @param ... Additional arguments (none implemented).
#'
#' @return \code{\linkS4class{BinImage}}.
#'
#' @seealso \code{\link{presetThr}}.
#'
#'
setGeneric("nh2005", function(x, parallel = TRUE, freeThreads = 1, ...) standardGeneric("nh2005"))
#' @export nh2005

#' @describeIn nh2005 The argument \code{subset} will be passed to
#'   \code{\link[raster]{subset}} for selecting which channel of the
#'   \code{\linkS4class{CanopyPhoto}} will be processed. It computes the
#'   threshold of the selected layer and return a \code{\linkS4class{BinImage}}.
setMethod("nh2005",
          signature(x = "CanopyPhoto"),
          function (x, subset = 3)
          {
            x <- raster::subset(x, subset)
            od <- x@file@name
            x <- nh2005(x)
            x@originalData <- od
            x@processedLayer <- subset
            validObject(x)
            return(x)
          }
)

#' @describeIn nh2005 Compute the threshold and return a
#'   \code{\linkS4class{BinImage}}.
setMethod("nh2005",
          signature(x = "RasterLayer"),
          function(x, parallel, freeThreads)
          {
            thr <- .nh2005(x, thrs = seq(getMin(x), getMax(x) - 1, 1), parallel, freeThreads)
            thrs <- seq(thr - 5, thr + 5, 1)
            thrs <- thrs[thrs > getMin(x) & thrs < getMax(x)]
            thr <- .nh2005(x, thrs, parallel, freeThreads)
            presetThr(x, thr)
          }
)



