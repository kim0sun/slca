# Compare captured output across installed libraries using the same example seeds.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
.libPaths(c(args[1], .libPaths()))
library(slca)
rd <- tools::Rd_db("slca")
result <- list()
path <- tempfile(fileext = ".R")
for (nm in names(rd)) {
   set.seed(123)
   unlink(path)
   tools::Rd2ex(rd[[nm]], out = path, commentDonttest = FALSE)
   if (!file.exists(path)) next
   warnings <- character()
   error <- NULL
   output <- capture.output(withCallingHandlers(tryCatch(
      source(path, local = new.env(parent = globalenv()), print.eval = TRUE),
      error = function(e) error <<- conditionMessage(e)),
      warning = function(w) {
         warnings <<- c(warnings, conditionMessage(w))
         invokeRestart("muffleWarning")
      }))
   result[[nm]] <- list(output = output, warnings = warnings, error = error)
   cat(nm, ":", length(output), "output lines;", length(warnings), "warnings\n")
}
saveRDS(result, args[2])
stopifnot(all(vapply(result, function(x) is.null(x$error), logical(1))))
