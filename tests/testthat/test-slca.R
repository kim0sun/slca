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
})
