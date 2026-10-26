# Run once per installed library; compare the resulting RDS snapshots.
# Arguments: installed library, output RDS.
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
library(slca, lib.loc = args[1])
snapshot <- function(fit, dat) {
   list(par = fit$par, score = fit$score, hessian = fit$hessian,
        posterior = fit$posterior, loglikelihood = fit$loglikelihood,
        convergence = fit$convergence,
        covariance = vcov(fit), covariance_hessian = vcov(fit, hessian = TRUE),
        fit = as.matrix(gof(fit)),
        predicted = predict(fit, dat, type = "posterior"),
        summary = capture.output(summary(fit)),
        parameters = capture.output(param(fit, se = TRUE)))
}
warnings <- character()
result <- withCallingHandlers({
   models <- list(
      mixed = slca(l[2] ~ y1 + y2 + y3 + y4),
      tree = slca(a[2] ~ x1 + x2 + x3, b[2] ~ y1 + y2 + y3, a ~ b)
   )
   result <- list()
   for (name in names(models)) {
      model <- models[[name]]
      sim <- if (name == "mixed")
         simulate(model, nsim = 200, seed = 139, nlevel = c(4, 2, 3, 2)) else
         simulate(model, nsim = 200, seed = 139)
      result[[paste0(name, "-simulation")]] <- sim
      for (missing in c(FALSE, TRUE)) {
         dat <- sim$response
         if (missing) {
            dat[1:35, 1] <- NA
            dat[20:50, 2] <- NA
         }
         for (method in c("em", "hybrid", "nlm")) {
            set.seed(803)
            fit <- estimate(model, dat, method = method,
               control = slcaControl(init.param = sim$parm, hessian = TRUE,
                                    em.iterlim = 5000, nlm.iterlim = 500))
            key <- paste(name, missing, method, sep = "-")
            result[[key]] <- snapshot(fit, dat)
            cat(key, "logLik", as.numeric(logLik(fit)), "\n")
            if (method == "em" && missing) {
               set.seed(732)
               result[[paste0(key, "-absolute-boot")]] <-
                  gof(fit, test = "boot", nboot = 3, maxiter = 2000)
            }
         }
      }
   }
   dat <- result$`mixed-simulation`$response
   dat[1:35, 1] <- NA
   dat[20:50, 2] <- NA
   set.seed(192)
   null <- estimate(models$mixed, dat)
   alt <- estimate(slca(l[3] ~ y1 + y2 + y3 + y4), dat,
                   control = slcaControl(nrep = 3))
   set.seed(194)
   result$relative_boot <- slca::compare(null, alt, test = "boot", nboot = 3,
                                        method = "em", nrep = 2, maxiter = 2000)
   for (name in c("nlsy_jlcpa", "nlsy_jlta")) {
      fit <- get(name)
      result[[name]] <- list(
         par = fit$par, score = fit$score, posterior = fit$posterior,
         logLik = logLik(fit), AIC = AIC(fit), BIC = BIC(fit),
         summary = capture.output(summary(fit)),
         parameters = capture.output(param(fit, se = TRUE)),
         reordered = reorder(fit)$posterior)
   }
   result
}, warning = function(w) {
   warnings <<- c(warnings, conditionMessage(w))
   invokeRestart("muffleWarning")
})
saveRDS(list(results = result, warnings = warnings), args[2])
cat("Saved", length(result), "cases and", length(warnings), "warnings\n")
