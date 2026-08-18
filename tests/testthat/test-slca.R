test_that("parenthesis/bracket identical", {
   expect_equal(slca(l[3] ~ a + b + c), slca(l(3) ~ a + b + c))
})
test_that("stop without number works", {
   expect_error(slca(l ~ a + b + c))
})
test_that("stop if there are multiple parents", {
   expect_error(slca(l[3] ~ a + b + c,
                     l2[3] ~ a + b + c))
})

test_that("encoded responses use -1 for missing and zero-based categories", {
   m <- slca(l[2] ~ y1 + y2)
   dat <- data.frame(
      y1 = factor(c(1, 2, NA, 1)),
      y2 = factor(c(2, NA, 1, 1))
   )
   mf <- proc_data(dat, m$model, na.rm = FALSE)

   expect_true(-1 %in% attr(mf, "y"))
   expect_true(0 %in% attr(mf, "y"))
   expect_false(any(attr(mf, "y") > 1))
   expect_equal(sum(attr(mf, "freq")), nrow(dat))
})

test_that("estimate handles partially missing responses in score calculation", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   dat <- data.frame(
      y1 = factor(c(1, 1, 2, 2, NA, 1, 2, NA)),
      y2 = factor(c(1, 2, 1, NA, 2, 1, NA, 2)),
      y3 = factor(c(2, 1, NA, 2, 1, NA, 2, 1))
   )

   fit <- estimate(
      m, dat,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   expect_s3_class(fit, "slcafit")
   expect_true(is.finite(as.numeric(logLik(fit))))
   expect_equal(nrow(fit$score), nrow(dat))
   expect_true(all(is.finite(fit$score[, !colSums(is.na(fit$score)), drop = FALSE])))
   expect_named(fit$convergence, c("EM", "nlm"))
   expect_silent(capture.output(summary(fit)))
})

test_that("saturated frequencies handle equal-size missing patterns", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   dat <- data.frame(
      y1 = factor(c(1, 1, 2, 2, NA, 1, 2, NA)),
      y2 = factor(c(1, 2, 1, 2, 2, 1, NA, 2)),
      y3 = factor(c(1, 2, 2, 1, 1, NA, 2, 1))
   )

   mf <- expect_silent(proc_data(dat, m$model, na.rm = FALSE))

   expect_equal(sum(attr(mf, "freq")), nrow(dat), tolerance = 1e-5)
   expect_true(is.finite(attr(mf, "loglik")))

   fit <- estimate(
      m, dat,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   gsq <- 2 * (attr(fit$mf, "loglik") - logLik(fit))

   expect_true(is.finite(as.numeric(gsq)))
   expect_gte(as.numeric(gsq), 0)
})

test_that("existing varied-size missing patterns keep saturated frequencies", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   dat <- data.frame(
      y1 = factor(c(1, 1, 2, 2, NA, 1, 2, NA)),
      y2 = factor(c(1, 2, 1, 2, NA, 1, NA, 2)),
      y3 = factor(c(1, 2, 2, 1, 1, NA, 2, 1))
   )

   mf <- expect_silent(proc_data(dat, m$model, na.rm = FALSE))

   expect_equal(sum(attr(mf, "freq")), nrow(dat), tolerance = 1e-5)
   expect_true(is.finite(attr(mf, "loglik")))
})

test_that("simulate and re-estimate use zero-based encoded responses", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )

   sim <- simulate(m, nsim = 60, seed = 7, parm = par)

   expect_true(is.finite(sim$llik))
   expect_null(attr(sim$response, "y"))
   expect_true(all(vapply(sim$response, is.factor, logical(1))))
   response_values <- unlist(lapply(sim$response, as.character),
                             use.names = FALSE)
   expect_true(all(response_values %in% c("1", "2")))

   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   expect_s3_class(fit, "slcafit")
   expect_true(is.finite(as.numeric(logLik(fit))))

   summary_out <- capture.output(summary(fit))
   param_out <- capture.output(param(fit))
   expect_true(any(grepl("^ +1 +2$", summary_out)))
   expect_false(any(grepl("^ +0 +1", summary_out)))
   expect_true(any(grepl("1\\(V1\\)", param_out)))
   expect_false(any(grepl("0\\(V1\\)", param_out)))
})

test_that("predict uses stored marginal posterior without new data", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   pred <- predict(fit)
   post <- predict(fit, type = "posterior")

   expect_s3_class(pred, "data.frame")
   expect_equal(nrow(pred), nrow(sim$response))
   expect_named(pred, "l")
   expect_true(all(pred$l %in% 1:2))
   expect_equal(post, fit$posterior$marginal)
   expect_equal(row.names(pred), row.names(sim$response))
})

test_that("predict validates and accepts supported new data formats", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   mat <- as.matrix(sim$response[1:3, ])
   vec <- as.vector(t(mat))

   expect_equal(nrow(predict(fit, newdata = mat)), 3)
   expect_equal(nrow(predict(fit, newdata = vec)), 3)
   expect_error(predict(fit, newdata = vec[-1]), "multiple")
   expect_error(predict(fit, newdata = sim$response[1:3, -1]),
                "missing manifest")

   unknown <- sim$response[1:3, ]
   unknown$y1 <- as.character(unknown$y1)
   unknown$y1[1] <- "new"
   expect_error(predict(fit, newdata = unknown), "unknown levels")
})

test_that("simulate validates nlevel and warns on mismatched parameters", {
   m <- slca(l[2] ~ y1 + y2 + y3)

   sim <- simulate(m, nsim = 20, seed = 1, nlevel = 3)
   expect_equal(vapply(sim$response, nlevels, integer(1)), c(y1 = 3, y2 = 3, y3 = 3))

   sim_named <- simulate(m, nsim = 20, seed = 1, nlevel = c(y2 = 4))
   expect_equal(vapply(sim_named$response, nlevels, integer(1)),
                c(y1 = 2, y2 = 4, y3 = 2))

   expect_error(simulate(m, nsim = 20, nlevel = c(y4 = 3)), "unknown")
   expect_error(simulate(m, nsim = 20, nlevel = c(2, 3)), "nlevel")
   expect_error(simulate(m, nsim = 20, nlevel = 1), "greater than or equal")
   expect_warning(simulate(m, nsim = 20, seed = 1, parm = 1:3), "parm")
   expect_error(simulate(m, nsim = 0), "positive")
   expect_warning(sim_round <- simulate(m, nsim = 2.5, seed = 1),
                  "rounded up")
   expect_equal(nrow(sim_round$response), 3)
})

test_that("estimate validates initial parameter length", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   dat <- data.frame(
      y1 = factor(c(1, 1, 2, 2)),
      y2 = factor(c(1, 2, 1, 2)),
      y3 = factor(c(1, 2, 2, 1))
   )

   expect_error(
      estimate(m, dat, control = slcaControl(init.param = 1:3)),
      "init.param"
   )
})

test_that("regress uses marginal posterior after prediction changes", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   covar <- data.frame(x = seq_len(nrow(sim$response)) %% 2)

   reg <- regress(fit, l ~ x, data = covar,
                  imputation = "modal", method = "naive")

   expect_s3_class(reg, "reg.slca")
})

test_that("predict handles structural models with multiple latent variables", {
   m <- slca(lx[2] ~ x1 + x2 + x3,
             ly[3] ~ y1 + y2 + y3,
             lx ~ ly)
   sim <- simulate(m, nsim = 50, seed = 5)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   pred <- predict(fit)
   post <- predict(fit, type = "posterior")

   expect_named(pred, c("lx", "ly"))
   expect_equal(nrow(pred), nrow(sim$response))
   expect_named(post, c("lx", "ly"))
   expect_equal(post, fit$posterior$marginal)
   expect_equal(nrow(predict(fit, newdata = sim$response[1:4, ])), 4)
})

test_that("regress supports bias-adjusted methods with stored posterior", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   covar <- data.frame(x = seq_len(nrow(sim$response)) %% 2)

   expect_s3_class(
      regress(fit, l ~ x, data = covar,
              imputation = "modal", method = "BCH"),
      "reg.slca"
   )
   expect_s3_class(
      regress(fit, l ~ x, data = covar,
              imputation = "modal", method = "ML"),
      "reg.slca"
   )
})

test_that("variance methods handle missing responses", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   dat <- sim$response
   dat$y1[c(1, 3)] <- NA
   dat$y2[2] <- NA
   fit <- estimate(
      m, dat,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   expect_true(all(is.finite(vcov(fit)[!is.na(vcov(fit))])))
   expect_true(all(is.finite(confint(fit, parm = 1:2))))
})

test_that("hessian covariance falls back or uses stored Hessian", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   fit_score <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   fit_hess <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6, hessian = TRUE)
   )

   expect_warning(v_score <- vcov(fit_score, hessian = TRUE), "Hessian")
   expect_true(all(is.finite(v_score[!is.na(v_score)])))
   v_hess <- vcov(fit_hess, hessian = TRUE)
   expect_true(all(is.finite(v_hess[!is.na(v_hess)])))
})

test_that("regression validates latent outcome and confidence intervals", {
   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 60, seed = 7, parm = par)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   covar <- data.frame(x = seq_len(nrow(sim$response)) %% 2)

   expect_error(regress(fit, y1 ~ x, data = covar), "single latent")
   expect_error(regress(fit, l + l ~ x, data = covar), "single latent")

   reg <- regress(fit, l ~ x, data = covar)
   expect_silent(out <- capture.output(ci <- confint(reg)))
   expect_true(length(out) > 0)
   expect_true(is.list(ci))

   se <- matrix(
      sqrt(pmax(diag(reg$vcov), 0)),
      nrow = nrow(reg$std.err), byrow = TRUE
   )
   expect_equal(unname(reg$std.err), se)

   wald <- reg$coefficients / reg$std.err
   pval <- 2 * stats::pnorm(abs(wald), lower.tail = FALSE)
   old_pval <- stats::pnorm(abs(wald), 1, lower.tail = FALSE)
   expect_false(isTRUE(all.equal(pval, old_pval)))
   summary_out <- capture.output(summary(reg, digits = 8))
   expect_true(all(vapply(
      format(pval, digits = 8),
      function(x) any(grepl(x, summary_out, fixed = TRUE)),
      logical(1)
   )))
})

test_that("model and diagnostic count inputs are validated", {
   expect_error(slca(), "model formula")
   expect_error(slca(l[x] ~ y1 + y2), "number of classes")
   expect_s3_class(slca(l[2] ~ y1 + y2), "slca")

   m <- slca(l[2] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m, nsim = 40, seed = 7, parm = par)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   expect_error(gof(fit, test = "boot", nboot = 0), "positive")
   expect_warning(gof(fit, test = "boot", nboot = 1.5,
                      maxiter = 5, tol = 1e-5), "rounded up")
   expect_error(compare(fit, fit, test = "boot", nboot = 0), "positive")
})

test_that("reorder warns on ignored or conflicting specifications", {
   m <- slca(lx[2] ~ x1 + x2 + x3,
             ly[2] ~ y1 + y2 + y3,
             constraints = list(c("lx", "ly")))
   sim <- simulate(m, nsim = 50, seed = 5)
   fit <- estimate(
      m, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   expect_warning(reorder(fit, nope = c(2, 1)), "Ignoring unknown")
   expect_warning(reorder(fit, lx = c(2, 1), ly = c(1, 2)),
                  "Conflicting")
})

test_that("bootstrap diagnostics keep failure metadata", {
   m1 <- slca(l[2] ~ y1 + y2 + y3)
   m2 <- slca(l[3] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m1, nsim = 50, seed = 7, parm = par)
   fit1 <- estimate(
      m1, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   fit2 <- estimate(
      m2, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   bgof <- gof(fit1, test = "boot", nboot = 1, maxiter = 5, tol = 1e-5)
   bcmp <- compare(fit1, fit2, test = "boot", nboot = 1,
                   maxiter = 5, tol = 1e-5)

   expect_equal(attr(bgof, "bootFail"), c(fit1 = 0L))
   expect_equal(attr(bcmp, "bootFail"), 0L)
   expect_true(is.list(attr(bgof, "boot.fail.msgs")))
   expect_true(is.list(attr(bcmp, "boot.fail.msgs")))
})

test_that("bootstrap diagnostics record failed refits", {
   m1 <- slca(l[2] ~ y1 + y2 + y3)
   m2 <- slca(l[3] ~ y1 + y2 + y3)
   par <- c(
      .6, .4,
      .8, .2, .7, .3, .6, .4,
      .3, .7, .4, .6, .5, .5
   )
   sim <- simulate(m1, nsim = 50, seed = 7, parm = par)
   fit1 <- estimate(
      m1, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )
   fit2 <- estimate(
      m2, sim$response,
      control = slcaControl(em.iterlim = 50, em.tol = 1e-6)
   )

   local_mocked_bindings(
      estModel = function(...) simpleError("forced bootstrap failure"),
      .package = "slca"
   )

   expect_warning(
      bgof <- gof(fit1, test = "boot", nboot = 1, maxiter = 5, tol = 1e-5),
      "1 bootstrap replicate"
   )
   expect_warning(
      bcmp <- compare(fit1, fit2, test = "boot", nboot = 1,
                      maxiter = 5, tol = 1e-5),
      "1 bootstrap replicate"
   )

   expect_equal(attr(bgof, "bootFail"), c(fit1 = 1L))
   expect_equal(attr(bcmp, "bootFail"), 1L)
   expect_true(is.na(bgof$`Pr(Boot)`))
   expect_true(is.na(bcmp$`Pr(Boot)`[2]))
   expect_true("forced bootstrap failure" %in%
               names(attr(bgof, "boot.fail.msgs")[[1]]))
   expect_true("forced bootstrap failure" %in%
               names(attr(bcmp, "boot.fail.msgs")[[1]]))
})
