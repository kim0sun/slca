#' @exportS3Method stats::logLik slcafit
logLik.slcafit <- function(object, ...) {
   res <- if (inherits(object, "slcafit"))
      structure(sum(object$loglikelihood),
                df = object$arg$df,
                nobs = object$arg$nobs
      ) else NA

   class(res) <- "logLik"
   res
}

#' @exportS3Method stats::deviance slcafit
deviance.slcafit <- function(object, ...) {
   2 * (attr(object$mf, "loglik") - sum(object$loglikelihood))
}

add_boot_attrs <- function(x, fail, msg) {
   attr(x, "bootFail") <- fail
   attr(x, "boot.fail.msgs") <- lapply(msg, table)
   attr(x, "boot.all.msgs") <- msg
   x
}

#' Goodness-of-Fit Test for Fitted `slca` Model
#'
#' Computes the AIC, BIC, and deviance statistic (G-squared) for assessing the goodness-of-fit of a fitted `slca` model. If the `test` argument is specified, absolute model fit can be evaluated using deviance statistics.
##'
#' @usage
#' gof(object, ...)
#'
#' \method{gof}{slcafit}(
#'    object, ..., test = c("none", "chisq", "boot"),
#'    nboot = 100, plot = FALSE,
#'    maxiter = 100, tol = 1e-6, verbose = FALSE
#' )
#'
#' @param object an object of class `slcafit`.
#' @param ... additional objects of class `slcafit` for comparison.
#' @param test a character string specifying the type of test to be conducted. If `"chisq"`, a chi-squared test is conducted. If `"boot"`, a bootstrap test is conducted.
#' @param nboot a positive number specifying the number of bootstrap rounds to be performed. Non-integer values are rounded up.
#' @param plot a logical value indicating whether to print histogram of G-squared statistics for boostrap samples, only for `test = "boot"`. The default is `FALSE`.
#' @param maxiter an integer specifying the maximum number of iterations allowed for the estimation process during each bootstrap iteration. The default is 100.
#' @param tol a numeric value specifying the convergence tolerance for each bootstrap iteration. The default is `1e-6`.
#' @param verbose a logical value indicating whether to print progress updates on the number of bootstrapping rounds completed.
#'
#' @returns
#' A `data.frame` containing the number of parameters (Df), loglikelihood, AIC, BIC, G-squared statistics, and the residual degree of freedom for each object.
#' If a statistical test is performed (using `test`), the result includes the corresponding p-value.
#' For `test = "boot"`, failed bootstrap replicates are omitted from the p-value calculation and recorded in the `bootFail`, `boot.fail.msgs`, and `boot.all.msgs` attributes.
#'
#' @seealso \link[slca]{compare}
#'
#' @example man/examples/diagnostic.R
#'
#' @export
gof <- function(object, ...) {
   if (missing(object)) {
      dots <- list(...)
      if (!length(dots)) stop("argument \"object\" is missing, with no default")
      nms <- names(dots)
      if (length(nms) && !is.na(nms[1]) && nzchar(nms[1]))
         attr(dots[[1]], "gof.name") <- nms[1]
      if (length(dots) > 1) {
         for (i in 2:length(dots)) {
            if (length(nms) >= i && !is.na(nms[i]) && nzchar(nms[i]) &&
                inherits(dots[[i]], "slcafit"))
               attr(dots[[i]], "gof.name") <- nms[i]
         }
      }
      object <- dots[[1]]
      dots <- dots[-1]
      return(do.call(gof, c(list(object), dots)))
   }
   UseMethod("gof")
}

gof_model_name <- function(expr, object, i) {
   nm <- attr(object, "gof.name", exact = TRUE)
   if (length(nm) == 1L && !is.na(nm) && nzchar(nm)) return(nm)

   s <- tryCatch(deparse(expr, nlines = 1L),
                 error = function(e) NA_character_)
   if (length(s) == 1L && !is.na(s) && nchar(s) <= 40L &&
       !grepl("^(structure|list)\\(", s))
      return(s)
   paste0("Model", i)
}

#' @rdname gof
#' @exportS3Method slca::gof slcafit
gof.slcafit <- function(
      object, ...,  test = c("none", "chisq", "boot"),
      nboot = 100, plot = FALSE,
      maxiter = 100, tol = 1e-6, verbose = FALSE
) {
   cl <- match.call(expand.dots = FALSE)
   objects <- list(object, ...)
   exprs <- c(list(cl[["object"]]), as.list(cl[["..."]]))
   mn <- vapply(seq_along(objects), function(i)
      gof_model_name(exprs[[i]], objects[[i]], i), character(1))
   est <- sapply(objects, inherits, "slcafit")
   if (all(!est)) stop("all models should be estimated")
   objects <- objects[est]
   mn <- mn[est]
   nmodel <- length(mn)

   test <- match.arg(test)
   if (test == "boot") nboot <- positive_count(nboot, "nboot")

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
      boot_fail <- integer(nmodel)
      boot_msg <- vector("list", nmodel)
      if (verbose) cat("START Bootstrap Sampling.\n")
      for (i in seq_len(nmodel)) {
         obj <- objects[[i]]
         lt <- obj$model$latent
         mr <- obj$model$measure
         arg <- obj$arg
         par <- unlist(obj$par)
         gb <- rep(NA_real_, nboot)
         fail_msg <- character()

         trace <- paste0(mn[i], "(", i, "/", nmodel, ") : ")
         for (b in 1:nboot) {
            if (verbose) cat("\r", trace, b, "/", nboot, sep = "")
            res <- tryCatch({
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
                  factor(y[[x]],
                         levels = seq_along(attr(obj$mf, "levels")[[x]]) - 1,
                         labels = attr(obj$mf, "levels")[[x]])
               })
               mf <- proc_data(y, obj$model, obj$control$na.rm)
               con <- obj$control
               con$verbose <- FALSE
               con$em.iterlim <- maxiter; con$nlm.iterlim <- maxiter
               con$em.tol <- tol; con$nlm.tol <- tol
               est <- estModel(obj$method, con, par, mf, arg)
               if (inherits(est, "error")) stop(conditionMessage(est))
               etc <- calcModel(
                  attr(mf, "y"), arg$nobs, arg$nvar, unlist(arg$nlev),
                  est$par, arg$fix0, arg$ref - 1, arg$nlv, arg$nrl, arg$nlf,
                  arg$npi, arg$ntau, arg$nrho, arg$ul, arg$vl,
                  arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
                  arg$nc, arg$nk, arg$nl, arg$ncl,
                  arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
               )
               2 * (attr(mf, "loglik") - sum(etc$ll))
            }, error = function(e) e)
            if (inherits(res, "error")) fail_msg <- c(fail_msg, conditionMessage(res))
            else gb[b] <- res
         }
         if (verbose) cat("\n")
         boot_fail[i] <- length(fail_msg)
         boot_msg[[i]] <- fail_msg
         ok <- !is.na(gb)
         pb[i] <- if (any(ok)) mean(gb[ok] >= gsq[i]) else NA_real_
         if (plot && any(ok)) {
            graphics::hist(gb[ok], breaks = "FD",
                 main = bquote(.(mn[i]) ~ ": Bootstrap Histogram"),
                 xlab = bquote("G"^2 ~ "statistic"),
                 xlim = c(min(min(gb[ok]), gsq[i]), max(max(gb[ok]), gsq[i])))
            graphics::abline(v = gsq[i], col = "red", lwd = 1.5)
         }
      }
      if (verbose) cat("END.\n")
      dt["Pr(Boot)"] <-pb
      names(boot_fail) <- mn
      names(boot_msg) <- mn
      if (sum(boot_fail) > 0)
         warning(sum(boot_fail), " bootstrap replicate(s) failed.",
                 call. = FALSE)
      dt <- add_boot_attrs(dt, boot_fail, boot_msg)
   }
   structure(dt, heading = "Analysis of Goodness of Fit Table\n",
             class = c("anova", "data.frame"))
}

#' Compare Two Fitted `slca` Models
#'
#' Conducts a relative model fit test between two fitted SLCM models using the deviance statistic.
#'
#' @param model1 an object of class `slcafit`.
#' @param model2 another object of class `slcafit` to be compared with `model1`.
#' @param test a character string specifying the type of test to be conducted. If `"chisq"`, a chi-squared test is conducted. If `"boot"`, a bootstrap test is conducted.
#' @param nboot a positive number specifying the number of bootstrap iterations to perform (used only when `test = "boot"`). Non-integer values are rounded up. The default is 50.
#' @param method a character string specifying the estimation method for bootstrapping.
#' @param plot a logical value indicating whether to display a histogram of G-squared statistics for the bootstrap samples (applicable only for `test = "boot"`). The default is `FALSE`.
#' @param maxiter an integer specifying the maximum number of iterations allowed during each bootstrap estimation round. The default is 100.
#' @param tol numeric value setting the convergence tolerance for each bootstrap iteration. The default is `1e-6`.
#' @param verbose a logical value indicating whether to print progress updates on completed bootstrap iterations. The default is `FALSE`.
#'
#' @returns
#' A `data.frame` containing the number of parameters (Df), loglikelihood, AIC, BIC, G-squared statistics, and the residual degree of freedom for each object.
#' If a statistical test is conducted (via `test`), the resulting p-value for the comparison is also included.
#' For `test = "boot"`, failed bootstrap replicates are omitted from the p-value calculation and recorded in the `bootFail`, `boot.fail.msgs`, and `boot.all.msgs` attributes.
#'
#' @seealso \link[slca]{gof}
#'
#' @example man/examples/diagnostic.R
#'
#' @export
compare <- function(
      model1, model2, test = c("none", "chisq", "boot"), nboot = 50,
      method = c("hybrid", "em", "nlm"), plot = FALSE,
      maxiter = 1000, tol = 1e-8, verbose = FALSE
) {
   models <- list(model1, model2)
   cl <- match.call()
   test <- match.arg(test)
   method <- match.arg(method)
   if (test == "boot") nboot <- positive_count(nboot, "nboot")
   name <- c(cl[["model1"]], cl[["model2"]])
   if (any(!sapply(models, inherits, "slcafit")))
      stop("both model should be estimated")
   mf1 <- model1$mf
   mf2 <- model2$mf
   attr(mf1, "y") <- NULL
   attr(mf2, "y") <- NULL
   if (!all.equal(mf1, mf2))
      stop("datasets used for models are different")

   df <- sapply(models, function(x) x$arg$df)
   models <- models[order(df)]
   name <- name[order(df)]
   h0 <- models[[1]]; h1 <- models[[2]]

   aic <- sapply(models, stats::AIC)
   bic <- sapply(models, stats::BIC)
   ll <- sapply(models, stats::logLik)
   dff <- sort(df)
   resdf <- diff(dff)

   gsq <- 2 * (stats::logLik(h1) - stats::logLik(h0))
   tab <- cbind(dff, ll, aic, bic, c(NA, gsq), c(NA, resdf))
   rownames(tab) <- unlist(name)
   dt <- data.frame(tab)
   names(dt) <- c("Df", "logLik", "AIC", "BIC", "Gsq", "Res. Df")

   if (test == "chisq") {
      dt$`Pr(>Chi)` <- c(NA, stats::pchisq(gsq, resdf, lower.tail = FALSE))
   } else if (test == "boot") {
      if (gsq < 1e-8) gb <- Inf
      else {
         gb <- rep(NA_real_, nboot)
         fail_msg <- character()
         if (verbose) cat("START Bootstrap Sampling. \n")
         blank <- rep(" ", nchar(nboot))
         for (b in seq_len(nboot)) {
            if (verbose) cat("\r", b, "/", nboot, blank, sep = "")
            res <- tryCatch({
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
                  factor(y[[x]],
                         levels = seq_along(attr(h0$mf, "levels")[[x]]) - 1,
                         labels = attr(h0$mf, "levels")[[x]])
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
               if (inherits(est0, "error")) stop(conditionMessage(est0))
               if (inherits(est1, "error")) stop(conditionMessage(est1))
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
               2 * (sum(etc1$ll) - sum(etc0$ll))
            }, error = function(e) e)
            if (inherits(res, "error")) fail_msg <- c(fail_msg, conditionMessage(res))
            else gb[b] <- res
         }
         if (verbose) cat("\r")
      }
      if (verbose) cat("DONE Bootstrap Sampling.\n")
      ok <- !is.na(gb)
      dt$`Pr(Boot)` <- c(NA, if (any(ok)) mean(gb[ok] >= gsq) else NA_real_)
      if (exists("fail_msg") && length(fail_msg) > 0)
         warning(length(fail_msg), " bootstrap replicate(s) failed.",
                 call. = FALSE)
      dt <- add_boot_attrs(dt, if (exists("fail_msg")) length(fail_msg) else 0L,
                           list(compare = if (exists("fail_msg")) fail_msg else character()))
      if (plot && any(ok)) {
         graphics::hist(gb[ok], breaks = "FD",
              main = "Bootstrap Histogram",
              xlab = bquote("G"^2 ~ "statistic"),
              xlim = c(min(min(gb[ok]), gsq), max(max(gb[ok]), gsq)))
         graphics::abline(v = gsq, col = "red", lwd = 1.5)
      }
   }
   title = "Analysis of Relative Model Fit\n"
   notes = paste0("Model H0: ", name[[1]],
                  "\nModel H1: ", name[[2]])
   structure(dt, heading = c(title, notes),
             class = c("anova", "data.frame"))
}
