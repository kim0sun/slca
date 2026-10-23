# Run from the repository root. No estimation is performed.
encoder <- new.env()
sys.source("R/data_encode.R", envir = encoder)
without_cache <- function(x) {
   attr(x$mf, "y") <- NULL
   attr(x$mf, "yu") <- NULL
   x
}
for (nm in c("nlsy_jlcpa", "nlsy_jlta")) {
   path <- file.path("data", paste0(nm, ".rda"))
   env <- new.env()
   stopifnot(identical(load(path, envir = env), nm))
   original <- env[[nm]]
   fit <- original
   child <- fit$model$measure$indicator
   attr(fit$mf, "y") <- encoder$encode_response(fit$mf, child)
   attr(fit$mf, "yu") <- encoder$encode_response(attr(fit$mf, "y_unique"), child)
   stopifnot(identical(without_cache(original), without_cache(fit)))
   if (!identical(original, fit)) {
      env[[nm]] <- fit
      tmp <- tempfile(fileext = ".rda")
      save(list = nm, file = tmp, envir = env, compress = "bzip2", version = 2)
      restored <- new.env()
      load(tmp, envir = restored)
      # Serialized formula environments have new identities after loading.
      stopifnot(isTRUE(all.equal(restored[[nm]], fit)),
                identical(restored[[nm]][setdiff(names(fit), "mf")],
                          fit[setdiff(names(fit), "mf")]))
      stopifnot(file.copy(tmp, path, overwrite = TRUE))
      unlink(tmp)
   }
   cat(nm, ": response caches current; all other fields unchanged\n", sep = "")
}
