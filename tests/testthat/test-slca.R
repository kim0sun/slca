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
