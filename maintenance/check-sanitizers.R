# Run with an instrumented slca library and the sanitizer runtime preloaded.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
.libPaths(c(args[1], .libPaths()))
library(slca)
set.seed(123)
rd <- tools::Rd_db("slca")
example_file <- tempfile(fileext = ".R")
for (nm in names(rd)) {
   cat("EXAMPLE:", nm, "\n")
   tools::Rd2ex(rd[[nm]], out = example_file, commentDonttest = FALSE)
   sys.source(example_file, envir = new.env(parent = globalenv()))
}
cat("ALL EXAMPLES COMPLETED\n")
res <- testthat::test_dir(file.path(args[2], "tests", "testthat"),
   env = new.env(parent = asNamespace("slca")), reporter = "summary",
   stop_on_failure = TRUE)
tab <- as.data.frame(res)
print(colSums(tab[c("failed", "error", "warning", "skipped", "passed")]))
cat("ALL SANITIZED TESTS COMPLETED\n")
sessionInfo()
