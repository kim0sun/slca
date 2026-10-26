# Arguments: installed library, CRAN 1.4.0 data directory, output directory,
# optional --update to replace bundled files only after all checks pass.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% 3:4)
update <- length(args) == 4L
if (update) stopifnot(identical(args[4], "--update"))
baseline <- "072d02b119e00019af4ff94c58652e063f495d58"
datasets <- c("nlsy_jlcpa", "nlsy_jlta")
for (nm in datasets) {
   path <- file.path(args[2], paste0(nm, ".rda"))
   expected <- system2("git", c("rev-parse", paste0(baseline, ":data/", nm, ".rda")),
                       stdout = TRUE)
   actual <- system2("git", c("hash-object", shQuote(path)), stdout = TRUE)
   stopifnot(length(expected) == 1L, grepl("^[0-9a-f]{40}$", expected),
             identical(actual, expected))
}
library(slca, lib.loc = args[1])
dir.create(args[3], recursive = TRUE, showWarnings = FALSE)
internal <- function(name) getFromNamespace(name, "slca")
delta <- function(a, b) {
   a <- unlist(a, use.names = FALSE)
   b <- unlist(b, use.names = FALSE)
   stopifnot(length(a) == length(b), identical(is.na(a), is.na(b)),
             identical(is.infinite(a), is.infinite(b)),
             identical(a[is.infinite(a)], b[is.infinite(b)]))
   max(c(0, abs(a[is.finite(a)] - b[is.finite(b)])))
}
results <- list()
objects <- list()
warning_log <- character()
withCallingHandlers({
for (nm in datasets) {
   env <- new.env()
   load(file.path(args[2], paste0(nm, ".rda")), env)
   old <- env[[nm]]
   raw <- as.data.frame(old$mf)
   lev <- attr(old$mf, "levels")
   raw[] <- lapply(names(raw), function(v)
      factor(old$mf[[v]], levels = seq_along(lev[[v]]), labels = lev[[v]]))
   current <- old
   current$mf <- internal("proc_data")(raw, old$model, old$control$na.rm)
   stopifnot(identical(as.matrix(old$mf), as.matrix(current$mf)),
             identical(attr(old$mf, "levels"), attr(current$mf, "levels")))
   current$arg <- internal("arg_mf")(
      old$model, old$arg, current$mf, old$fix2zero)
   stopifnot(isTRUE(all.equal(old$arg, current$arg)))
   current$skeleton <- internal("get_frame")(old$model, current$arg, current$mf)
   stopifnot(isTRUE(all.equal(old$skeleton, current$skeleton)))
   current$control <- slcaControl()
   stopifnot(all(names(old$control) %in% names(current$control)))
   current$control[names(old$control)] <- old$control
   # No class permutation or optimization: refresh scores and posteriors only.
   current <- reorder(current)
   a <- current$arg
   a$nlev <- unlist(a$nlev)
   a$ref <- a$ref - 1
   a$par <- current$par
   a$y <- attr(current$mf, "y")
   calc <- internal("calcModel")
   current$loglikelihood <- do.call(calc, a[names(formals(calc))])$ll
   # Keep the original Hessian and convergence history; neither is re-estimated.
   current$hessian <- if (is.null(old$hessian))
      matrix(NA_real_, length(old$par), length(old$par)) else old$hessian
   stopifnot(identical(old$par, current$par),
             identical(old$convergence, current$convergence),
             isTRUE(all.equal(predict(current, type = "posterior"),
                              predict(current, raw, type = "posterior"),
                              tolerance = 1e-10)))
   checks <- c(
      parameter = delta(old$par, current$par),
      logit = delta(old$logit, current$logit),
      posterior = delta(old$posterior, current$posterior),
      score = delta(old$score, current$score),
      covariance = delta(vcov(old), vcov(current)),
      default_SE = delta(sqrt(diag(vcov(old))), sqrt(diag(vcov(current)))),
      hessian_SE = delta(sqrt(diag(vcov(old, hessian = TRUE))),
                         sqrt(diag(vcov(current, hessian = TRUE)))),
      logLik = delta(logLik(old), logLik(current)),
      AIC = delta(AIC(old), AIC(current)),
      BIC = delta(BIC(old), BIC(current)),
      saturated_logLik = delta(attr(old$mf, "loglik"), attr(current$mf, "loglik")),
      class_assignments_changed = sum(predict(old) != predict(current)),
      summary_text_identical = identical(capture.output(summary(old)),
                                         capture.output(summary(current))),
      parameter_SE_text_identical = identical(capture.output(param(old, se = TRUE)),
                                              capture.output(param(current, se = TRUE))),
      convergence_identical = identical(old$convergence, current$convergence)
   )
   results[[nm]] <- checks
   objects[[nm]] <- current
   writeLines(capture.output(summary(old)), file.path(args[3], paste0(nm, "-before.txt")))
   writeLines(capture.output(summary(current)), file.path(args[3], paste0(nm, "-after.txt")))
   cat("\n", nm, "\n", sep = "")
   print(checks, digits = 16)
   print(gof(old, current), digits = 16)
}
}, warning = function(w) {
   warning_log <<- c(warning_log, conditionMessage(w))
   invokeRestart("muffleWarning")
})
writeLines(warning_log, file.path(args[3], "warnings.txt"))
if (length(warning_log)) {
   cat("Warnings retained in warnings.txt:\n")
   print(unique(warning_log))
}
write.csv(do.call(rbind, results), file.path(args[3], "comparison.csv"))
writeLines(capture.output(sessionInfo()), file.path(args[3], "session-info.txt"))
writeLines(c(paste("Executed:", Sys.time()),
             paste("Baseline:", baseline),
             paste("Commit:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
             paste("Package version:", packageVersion("slca")),
             paste("Script MD5:", tools::md5sum("maintenance/check-fixed-parameters.R")),
             paste("Library:", normalizePath(args[1])),
             paste("Legacy data:", normalizePath(args[2])),
             "No optimization or random sampling; convergence and available Hessian retained.",
             "An unavailable Hessian is represented by an all-NA matrix."),
           file.path(args[3], "provenance.txt"))

# Check both objects before writing either replacement; leave a report on failure.
for (checks in results) {
   flags <- grepl("identical$", names(checks))
   if (!all(checks[flags] == 1) || !all(checks[!flags] == 0))
      stop("Regeneration changed numerical results or output; review comparison.csv before replacing data.")
}
for (nm in datasets) {
   current <- objects[[nm]]
   saveRDS(current, file.path(args[3], paste0(nm, ".rds")))
   env <- list2env(setNames(list(current), nm), parent = emptyenv())
   path <- file.path(args[3], paste0(nm, ".rda"))
   save(list = nm, file = path, envir = env, compress = "bzip2", version = 2)
   restored <- new.env()
   load(path, restored)
   stopifnot(isTRUE(all.equal(restored[[nm]], current)),
             identical(restored[[nm]]$par, current$par))
}
if (update) {
   for (nm in datasets)
      stopifnot(file.copy(file.path(args[3], paste0(nm, ".rda")),
                          file.path("data", paste0(nm, ".rda")), overwrite = TRUE))
   cat("Both bundled objects replaced after exact numerical and output checks.\n")
} else cat("Validation passed; bundled data unchanged (use --update to replace).\n")
