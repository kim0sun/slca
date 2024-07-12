#' @exportS3Method stats::logLik slca
logLik.slca <- function(object, ...) {
   res <- if (inherits(object, "estimated"))
      structure(sum(object$loglikelihood),
                df = object$arg$df,
                nobs = object$arg$nobs
      ) else NA

   class(res) <- "logLik"
   res
}

#' @exportS3Method stats::deviance slca
deviance.slca <- function(object, ...) {
   if (!inherits(object, "estimated"))
      stop("slca model is not estimated")
   sapply(objects, function(x)
      2 * (attr(x$mf, "loglik") - stats::logLik(x)))
}

#' Goodness of Fit Test for Estimated `slca` Model
#'
#' Provides AIC, BIC and deviance statistic (G-squared) for goodness of fit test for the fitted model. Absolute model fit can be tested with deviance statistics, if `test` argument is specified.
#'
#' @aliases gof gof.slca
#'
#' @usage
#' gof(object, ...)
#'
#' \method{gof}{slca}(
#'    object, ..., test = c("none", "chisq", "boot"), nboot = 100,
#'    maxiter = 100, tol = 1e-6, verbose = FALSE
#' )
#'
#' @param object an object of class `slca` and `estimated`.
#' @param ... additional objects of class `slca` and `estimated`.
#' @param test a character string specifying the type of test to be conducted. If `"chisq"`, a chi-squared test is conducted. If `"boot"`, a bootstrap test is conducted.
#' @param nboot an integer specifying the number of bootstrap rounds to be performed.
#' @param plot a logical value indicating whether to print histogram of G-squared statistics for boostrap samples, only for `test = "boot"`.
#' @param maxiter an integer specifying maximum number of iterations allowed for the estimation process of each bootstrapping round.
#' @param tol a numeric value setting tolerance for the convergence of each bootstrapping round.
#' @param verbose a logical value indicating whether to print progress updates on the number of bootstrapping rounds completed.
#'
#' @returns
#' A `data.frame` containing the number of parameters (Df), loglikelihood, AIC, BIC, G-squared statistics, and the residual degree of freedom for each object.
#' Depending on the `test` argument, the p-value for the corresponding statistical test may also be included.
#'
#' @seealso \link[slca]{compare}
#'
#' @example man/examples/diag.R
#'
#' @export
gof <- function(object, ...) UseMethod("gof")

#' @exportS3Method slca::gof slca
gof.slca <- function(
      object, ...,  test = c("none", "chisq", "boot"),
      nboot = 100, plot = FALSE,
      maxiter = 100, tol = 1e-6, verbose = FALSE
) {
   cl <- match.call(expand.dots = FALSE)
   objects <- list(object, ...)
   est <- sapply(objects, inherits, "estimated")
   mn <- sapply(c(cl[["object"]], cl[["..."]]), deparse)
   if (all(!est)) stop("at least 1 model should be estimated")
   objects <- objects[est]
   mn <- mn[est]
   nmodel <- length(mn)

   test <- match.arg(test)

   df <- sapply(objects, function(x) x$arg$df)
   aic <- sapply(objects, stats::AIC)
   bic <- sapply(objects, stats::BIC)
   ll <- sapply(objects, stats::logLik)
   gsq <- sapply(objects, function(x)
      2 * (attr(x$mf, "loglik") - stats::logLik(x)))
   resdf <- sapply(objects, function(x)
      attr(x$mf, "df") - x$arg$df)

   tab <- cbind(df, ll, aic, bic, gsq, resdf)
   rownames(tab) <- mn
   dt <- data.frame(tab)
   names(dt) <- c("Df", "logLik", "AIC", "BIC", "Gsq", "Res. Df")

   if (test == "chisq") {
      dt["Pr(>Chi)"] <-
         stats::pchisq(gsq, resdf, lower.tail = FALSE)
   }
   if (test == "boot") {
      pb <- numeric(nmodel)
      if (verbose) cat("START Bootstrap Sampling.\n")
      for (i in seq_len(nmodel)) {
         obj <- objects[[i]]
         lt <- obj$model$latent
         mr <- obj$model$measure
         arg <- obj$arg
         par <- unlist(obj$par)
         gb <- numeric(nboot)

         trace <- paste0(mn[i], "(", i, "/", nmodel, ") : ")
         for (b in 1:nboot) {
            if (verbose) cat("\r", trace, b, "/", nboot, sep = "")
            sim <- simModel(
               arg$nobs, arg$nvar, arg$nlev, par,
               arg$nlv, arg$nrl, arg$nlf, arg$npi, arg$ntau, arg$nrho,
               arg$ul, arg$vl, arg$lf, arg$rt, arg$eqrl, arg$eqlf,
               arg$nc, arg$nk, arg$nl, arg$ncl,
               arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
            )
            y <- data.frame(do.call(cbind, sim$y))
            colnames(y) <- unlist(mr[["indicator"]])
            y[] <- lapply(names(y), function(x) {
               res <- factor(y[[x]])
               levels(res) <- attr(obj$mf, "levels")[[x]]
               res
            })
            mf <- proc_data(y, obj$model, obj$control$na.rm)
            con <- obj$control
            con$verbose <- FALSE
            con$em.iterlim <- maxiter; con$nlm.iterlim <- maxiter
            con$em.tol <- tol; con$nlm.tol <- tol
            est <- estModel(obj$method, con, par, mf, arg)
            etc <- calcModel(
               attr(mf, "y"), arg$nobs, arg$nvar, unlist(arg$nlev),
               est$par, arg$fix0, arg$ref - 1, arg$nlv, arg$nrl, arg$nlf,
               arg$npi, arg$ntau, arg$nrho, arg$ul, arg$vl,
               arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
               arg$nc, arg$nk, arg$nl, arg$ncl,
               arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
            )
            gb[b] <- 2 * (attr(mf, "loglik") - sum(etc$ll))
         }
         if (verbose) cat("\n")
         pb[i] <- mean(gb >= gsq[i])
         if (plot) {
            hist(gb, breaks = "FD",
                 main = bquote(.(mn[i]) ~ ": Bootstrap Histogram"),
                 xlab = bquote("G"^2 ~ "statistic"))
            abline(v = gsq[i], col = "red", lwd = 1.5)
         }
      }
      if (verbose) cat("END.\n")
      dt["Pr(Boot)"] <-pb
   }
   structure(dt, heading = "Analysis of Goodness of Fit Table\n",
             class = c("anova", "data.frame"))
}

#' Compare Two Estimated `slca` Models
#'
#' Provides relative model fit test for two fitted SLCM models with deviance statistic.
#'
#' @param model1 an object of class `slca` and `estimated`.
#' @param model2 another object of class `slca` and `estimated`.
#' @param test a character string specifying the type of test to be conducted. If `"chisq"`, a chi-squared test is conducted. If `"boot"`, a bootstrap test is conducted.
#' @param nboot an integer specifying the number of bootstrap rounds to be performed.
#' @param method estimation method for bootstrapping.
#' @param plot a logical value indicating whether to print histogram of G-squared statistics for boostrap samples, only for `test = "boot"`.
#' @param maxiter an integer specifying maximum number of iterations allowed for the estimation process of each bootstrapping round.
#' @param tol a numeric value setting tolerance for the convergence of each bootstrapping round.
#' @param verbose a logical value indicating whether to print progress updates on the number of bootstrapping rounds completed.
#'
#' @returns
#' A `data.frame` containing the number of parameters (Df), loglikelihood, AIC, BIC, G-squared statistics, and the residual degree of freedom for each object.
#' Depending on the `test` argument, the p-value for the corresponding statistical test may also be included.
#'
#' @seealso \link[slca]{gof}
#'
#' @example man/examples/diag.R
#'
#' @export
compare <- function(
      model1, model2, test = c("none", "chisq", "boot"), nboot = 100,
      method = c("hybrid", "em", "nlm"), plot = FALSE,
      maxiter = 1000, tol = 1e-8, verbose = FALSE
) {
   models <- list(model1, model2)
   cl <- match.call()
   test <- match.arg(test)
   method <- match.arg(method)
   name <- c(cl[["model1"]], cl[["model2"]])
   if (any(!sapply(models, inherits, "estimated")))
      stop("both model should be estimated")
   mf1 <- model1$mf
   mf2 <- model2$mf
   attr(mf1, "y") <- NULL
   attr(mf2, "y") <- NULL
   if (!all.equal(mf1, mf2))
      stop("datasets used for models are different")

   df <- sapply(models, function(x) x$arg$df)
   models <- models[order(df)]
   h0 <- models[[1]]; h1 <- models[[2]]

   aic <- sapply(models, stats::AIC)
   bic <- sapply(models, stats::BIC)
   ll <- sapply(models, stats::logLik)
   df <- sort(df)
   resdf <- diff(df)

   gsq <- 2 * (stats::logLik(h1) - stats::logLik(h0))
   tab <- cbind(df, ll, aic, bic, c(NA, gsq), c(NA, resdf))
   rownames(tab) <- unlist(name[order(df)])
   dt <- data.frame(tab)
   names(dt) <- c("Df", "logLik", "AIC", "BIC", "Gsq", "Res. Df")

   if (test == "chisq") {
      dt$`Pr(>Chi)` <- c(NA, stats::pchisq(gsq, resdf, lower.tail = FALSE))
   } else if (test == "boot") {
      if (gsq < 1e-8) gb <- Inf
      else {
         gb <- numeric(nboot)
         if (verbose) cat("START Bootstrap Sampling. \n")
         blank <- rep(" ", nchar(nboot))
         for (b in seq_len(nboot)) {
            if (verbose) cat("\r", b, "/", nboot, blank, sep = "")
            arg0 <- h0$arg
            sim <- simModel(
               arg0$nobs, arg0$nvar, arg0$nlev, h0$par,
               arg0$nlv, arg0$nrl, arg0$nlf,
               arg0$npi, arg0$ntau, arg0$nrho,
               arg0$ul, arg0$vl, arg0$lf, arg0$rt,
               arg0$eqrl, arg0$eqlf,
               arg0$nc, arg0$nk, arg0$nl, arg0$ncl,
               arg0$nc_pi, arg0$nk_tau, arg0$nl_tau,
               arg0$nc_rho, arg0$nr_rho
            )
            y <- data.frame(do.call(cbind, sim$y))
            colnames(y) <- unlist(h0$model$measure[["indicator"]])
            y[] <- lapply(names(y), function(x) {
               res <- factor(y[[x]])
               levels(res) <- attr(h0$mf, "levels")[[x]]
               res
            })
            mf0 <- proc_data(y, h0$model, h0$control$na.rm)
            mf1 <- proc_data(y, h1$model, h1$control$na.rm)

            con <- slcaControl()
            con$verbose <- FALSE
            con$em.iterlim <- maxiter
            con$nlm.iterlim <- maxiter
            con$em.tol <- tol
            con$nlm.tol <- tol

            arg1 <- h1$arg
            est0 <- estModel(method, con, h0$par, mf0, arg0)
            est1 <- estModel(method, con, h1$par, mf1, arg1)
            etc0 <- calcModel(
               attr(mf0, "y"), arg0$nobs, arg0$nvar, unlist(arg0$nlev),
               est0$par, arg0$fix0, arg0$ref - 1, arg0$nlv, arg0$nrl, arg0$nlf,
               arg0$npi, arg0$ntau, arg0$nrho, arg0$ul, arg0$vl,
               arg0$lf, arg0$tr, arg0$rt, arg0$eqrl, arg0$eqlf,
               arg0$nc, arg0$nk, arg0$nl, arg0$ncl,
               arg0$nc_pi, arg0$nk_tau, arg0$nl_tau, arg0$nc_rho, arg0$nr_rho
            )
            etc1 <- calcModel(
               attr(mf1, "y"), arg1$nobs, arg1$nvar, unlist(arg1$nlev),
               est1$par, arg1$fix0, arg1$ref - 1, arg1$nlv, arg1$nrl, arg1$nlf,
               arg1$npi, arg1$ntau, arg1$nrho, arg1$ul, arg1$vl,
               arg1$lf, arg1$tr, arg1$rt, arg1$eqrl, arg1$eqlf,
               arg1$nc, arg1$nk, arg1$nl, arg1$ncl,
               arg1$nc_pi, arg1$nk_tau, arg1$nl_tau, arg1$nc_rho, arg1$nr_rho
            )
            gb[b] <- 2 * (sum(etc1$ll) - sum(etc0$ll))
         }
         if (verbose) cat("\r")
      }
      if (verbose) cat("DONE Bootstrap Sampling.\n")
      dt$`Pr(Boot)` <- c(NA, mean(gb >= gsq))
      if (plot) {
         hist(gb, breaks = "FD",
              main = "Bootstrap Histogram",
              xlab = bquote("G"^2 ~ "statistic"))
         abline(v = gsq, col = "red", lwd = 1.5)
      }
   }
   title = "Analysis of Relative Model Fit\n"
   notes = paste0("Model H0: ", name[[which.min(df)]],
                  "\nModel H1: ", name[[which.max(df)]])
   structure(dt, heading = c(title, notes),
             class = c("anova", "data.frame"))
}
