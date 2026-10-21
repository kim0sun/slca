audit_fit <- function(missing = FALSE, hessian = FALSE) {
   m <- slca(l[2] ~ y1 + y2 + y3 + y4)
   dat <- simulate(m, nsim = 300, seed = 710,
      parm = c(.6, .4, .8, .2, .7, .3, .9, .1, .65, .35,
               .2, .8, .3, .7, .15, .85, .25, .75))$response
   if (missing) dat$y1[1:80] <- NA
   estimate(m, dat, control = slcaControl(em.iterlim = 3000,
                                         hessian = hessian))
}

audit_calc <- function(fit, par = fit$par) {
   arg <- fit$arg
   arg$nlev <- unlist(arg$nlev)
   arg$ref <- arg$ref - 1
   arg$y <- attr(fit$mf, "y")
   arg$par <- par
   do.call(calcModel, arg[names(formals(calcModel))])
}

test_that("missing-item scores agree with observed likelihood derivatives", {
   for (missing in c(FALSE, TRUE)) {
      fit <- audit_fit(missing)
      free <- which(!apply(fit$score, 2, anyNA))
      numeric_score <- fit$score[, free, drop = FALSE]
      for (j in seq_along(free)) {
         hi <- lo <- fit$par
         hi[free[j]] <- hi[free[j]] + 1e-5
         lo[free[j]] <- lo[free[j]] - 1e-5
         hi <- unlist(tapply(hi, fit$arg$id, norm2), use.names = FALSE)
         lo <- unlist(tapply(lo, fit$arg$id, norm2), use.names = FALSE)
         numeric_score[, j] <- (rowSums(audit_calc(fit, hi)$ll) -
                                 rowSums(audit_calc(fit, lo)$ll)) / 2e-5
      }
      expect_equal(fit$score[, free, drop = FALSE], numeric_score,
                   tolerance = 1e-7)
      expect_equal(unname(vcov(fit, type = "logit")[free, free]),
                   MASS::ginv(crossprod(numeric_score)), tolerance = 1e-6)
   }
})

test_that("large saturated tables include previously unmatched missing patterns", {
   dat <- as.data.frame(matrix(rep(c(1, 2), each = 160),
                               ncol = 16, byrow = TRUE))
   partial <- rep(NA_real_, 16)
   partial[1:2] <- c(1, 2)
   dat <- rbind(dat, partial)
   sat <- count_row(dat, rep(2, 16), FALSE)
   expect_equal(sum(sat$freq), 21)
   expect_equal(sum(sat$theta), 1)
   expect_equal(sat$loglik, 20 * log(10/21) + log(1/21), tolerance = 1e-7)
})

test_that("AddHealth saturated frequencies include every observation", {
   m <- slca(
      a[2] ~ S1w1 + S2w1 + S3w1 + S4w1 + D1w1 + D2w1 + F1w1 + F2w1,
      b[2] ~ S1w2 + S2w2 + S3w2 + S4w2 + D1w2 + D2w2 + F1w2 + F2w2,
      a ~ b
   )
   mf <- proc_data(addhealth, m$model, FALSE)
   expect_equal(sum(attr(mf, "freq")), nrow(mf))
   expect_equal(sum(attr(mf, "theta")), 1)
   expect_equal(attr(mf, "loglik"), -11977.637866, tolerance = 1e-7)
})

test_that("unmanageable saturated tables report unavailable fit statistics", {
   dat <- as.data.frame(rbind(rep(1, 24), rep(2, 24),
                             c(1, rep(NA, 23))))
   expect_warning(sat <- count_row(dat, rep(2, 24), FALSE), "unavailable")
   expect_true(is.na(sat$loglik))
   expect_true(all(is.na(sat$freq)))
})

test_that("all-missing cases are excluded only from estimation", {
   fit <- audit_fit()
   dat <- simulate(fit, nsim = 100, seed = 42)$response
   dat[1, ] <- NA
   reduced <- estimate(slca(l[2] ~ y1 + y2 + y3 + y4), dat)
   expect_equal(reduced$arg$nobs, 99)
   expect_identical(rownames(reduced$mf), as.character(2:100))
   expect_equal(nrow(predict(reduced, dat)), 100)
   covariates <- data.frame(x = rep(c(0, 1), 50))
   set.seed(14)
   reg <- regress(reduced, l ~ x, data = covariates)
   set.seed(14)
   reference <- regress(reduced, l ~ x, data = covariates[-1, , drop = FALSE])
   expect_equal(reg$coefficients, reference$coefficients)
   dat[,] <- NA
   expect_error(proc_data(dat, fit$model, FALSE), "no observations")
})

test_that("reorder transforms logits and Hessian without changing probabilities", {
   fit <- audit_fit(hessian = TRUE)
   changed <- reorder(fit, l = c(2, 1))
   expect_equal(changed$logit,
                changed$par - changed$par[changed$arg$ref_idx[changed$arg$id]])
   expect_equal(changed$logit[changed$arg$ref_idx], rep(0, length(changed$arg$ref)))
   old <- utils::relist(seq_along(fit$par), fit$skeleton$par)
   old$pi$l <- old$pi$l[, 2:1, drop = FALSE]
   old$rho[[1]] <- old$rho[[1]][, 2:1, drop = FALSE]
   ix <- unlist(old, use.names = FALSE)
   expect_equal(unname(vcov(changed, hessian = TRUE)),
                unname(vcov(fit, hessian = TRUE)[ix, ix]), tolerance = 1e-7)
   expect_equal(unname(changed$posterior$marginal$l),
                unname(fit$posterior$marginal$l[, 2:1]))
   restored <- reorder(changed, l = c(2, 1))
   expect_equal(restored$logit, fit$logit)
   expect_equal(restored$hessian, fit$hessian, tolerance = 1e-7)
})

test_that("confidence intervals support subsets and odds ratios", {
   fit <- audit_fit()
   expect_equal(confint(fit, parm = 1), confint(fit)[1, , drop = FALSE])
   reg <- structure(list(
      coefficients = matrix(c(.2, -.5, .7, .3, .8, -.2), 2,
         dimnames = list(c("1/3", "2/3"), c("(Intercept)", "x", "z"))),
      std.err = matrix(c(.1, .2, .3, .4, .5, .6), 2,
         dimnames = list(c("1/3", "2/3"), c("(Intercept)", "x", "z")))
   ), class = "reg.slca")
   capture.output(all <- confint(reg))
   capture.output(named <- confint(reg, parm = "x"))
   capture.output(indexed <- confint(reg, parm = 2))
   capture.output(odds <- confint(reg, odds.ratio = TRUE))
   expect_equal(named, lapply(all, function(x) x["x", , drop = FALSE]))
   expect_equal(indexed, named)
   expect_equal(odds, lapply(all, exp))
   expect_error(confint(reg, parm = "absent"), "unknown")
})

test_that("zero constraints work with multiple random starts", {
   fit <- audit_fit()
   dat <- simulate(fit, nsim = 100, seed = 83)$response
   constrained <- estimate(slca(l[2] ~ y1 + y2 + y3 + y4), dat,
      fix2zero = 3, control = slcaControl(nrep = 2, test.iter = 10))
   expect_identical(exp(constrained$par[3]), 0)
   expect_true(is.finite(as.numeric(logLik(constrained))))
   changed <- reorder(constrained, l = c(2, 1))
   expect_false(any(changed$arg$ref_idx %in% changed$fix2zero))
   expect_false(anyNA(changed$logit))
   constrained <- estimate(slca(l[2] ~ y1 + y2 + y3 + y4), dat,
      fix2zero = c(4, 11))
   changed <- reorder(constrained, l = c(2, 1))
   expect_false(any(changed$arg$ref_idx %in% changed$fix2zero))
   expect_true(all(changed$arg$ref >= 1))
   expect_false(anyNA(changed$logit))
})

test_that("simulation likelihood respects measurement block order", {
   models <- list(
      slca(a[2] ~ x1+x2+x3, b[3] ~ y1+y2+y3, a ~ b),
      slca(a[2] ~ x1+x2+x3, b[2] ~ y1+y2+y3, a ~ b,
           constraints = c("a", "b")),
      slca(a[2] ~ x1+x2+x3, b[3] ~ y1+y2+y3, r[2] ~ a+b),
      slca(a[2] ~ x1+x2+x3, b[3] ~ y1+y2+y3)
   )
   for (model in models) {
      sim <- simulate(model, nsim = 100, seed = 777)
      mf <- proc_data(sim$response, model$model, FALSE)
      arg <- arg_mf(model$model, model$arg, mf, NULL)
      fit <- list(arg = arg, mf = mf, par = log(unlist(sim$parm)))
      expect_equal(sim$llik, sum(audit_calc(fit)$ll), tolerance = 1e-10)
   }
})

test_that("modal ties use the same single assignment as predict", {
   fit <- audit_fit()
   fit$posterior$marginal$l[1, ] <- .5
   seen <- NULL
   original <- classification_error
   local_mocked_bindings(classification_error = function(w, p) {
      seen <<- w
      original(w, p)
   }, .package = "slca")
   regress(fit, l ~ 1, method = "ML")
   expect_equal(colSums(seen), rep(1, 300))
   expect_equal(unname(seen[, 1]), c(1, 0))
   expect_equal(unname(colSums(original(seen, fit$posterior$marginal$l))), c(1, 1))
})

test_that("cyclic latent structures are rejected", {
   expect_error(slca(a[2] ~ x1+x2, b[2] ~ y1+y2, a ~ b, b ~ a), "cycles")
})

test_that("absolute bootstrap retains missingness and rejects nonconvergence", {
   fit <- audit_fit(missing = TRUE)
   seen <- NULL
   local_mocked_bindings(estModel = function(method, control, par, mf, arg) {
      seen <<- is.na(mf)
      list(par = par, ll = -1, conv = c(EM = FALSE, nlm = NA))
   }, .package = "slca")
   expect_warning(out <- gof(fit, test = "boot", nboot = 1), "1 bootstrap")
   expect_equal(seen, is.na(fit$mf))
   expect_equal(unname(attr(out, "bootFail")), 1L)
   expect_true(is.na(out$`Pr(Boot)`))
})

test_that("absolute bootstrap p-values have finite simulation resolution", {
   fit <- audit_fit()
   fit$mf <- structure(fit$mf, loglik = 1e6)
   local_mocked_bindings(estModel = function(method, control, par, mf, arg) {
      list(par = par, ll = -1, conv = c(EM = TRUE, nlm = NA))
   }, .package = "slca")
   out <- gof(fit, test = "boot", nboot = 2)
   expect_equal(out$`Pr(Boot)`, 1/3)
})

test_that("relative tests reject negative observed likelihood ratios", {
   fit <- audit_fit()
   alternative <- fit
   alternative$arg$df <- fit$arg$df + 1
   alternative$loglikelihood <- fit$loglikelihood - 1
   expect_error(compare(fit, alternative, test = "boot"), "refit")
   expect_error(compare(fit, alternative, test = "chisq"), "refit")
   expect_s3_class(compare(fit, alternative), "anova")
})
