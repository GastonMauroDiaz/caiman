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