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