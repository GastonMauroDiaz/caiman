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